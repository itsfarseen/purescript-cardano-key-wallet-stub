module Cardano.Wallet.Key
  ( KeyWallet(KeyWallet)
  , PrivateDrepKey(PrivateDrepKey)
  , PrivatePaymentKey(PrivatePaymentKey)
  , PrivateStakeKey(PrivateStakeKey)
  , privateKeyToPkh
  , privateKeyToPkhCred
  , privateKeysToAddress
  , privateKeysToKeyWallet
  , getPrivateDrepKey
  , getPrivatePaymentKey
  , getPrivateStakeKey
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , encodeAeson
  )
import Cardano.Types.Address (Address)
import Cardano.Types.Credential (Credential)
import Cardano.Types.Ed25519KeyHash (Ed25519KeyHash)
import Cardano.Types.NetworkId (NetworkId)
import Cardano.Types.PrivateKey (PrivateKey)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)

import Partial.Unsafe (unsafeCrashWith)

newtype DataSignature = DataSignature Unit

-------------------------------------------------------------------------------
-- Key backend
-------------------------------------------------------------------------------

-- | An interface that wraps `PrivateKey`s. Used in CTL.
-- | Technically, can be implemented with remote calls, e.g. over HTTP,
-- | to provide signing services without revealing the private key.
newtype KeyWallet = KeyWallet Unit

derive instance Newtype KeyWallet _

newtype PrivatePaymentKey = PrivatePaymentKey Unit

derive instance Newtype PrivatePaymentKey _

instance Show PrivatePaymentKey where
  show _ = "(PrivatePaymentKey Unit)"

instance EncodeAeson PrivatePaymentKey where
  encodeAeson (PrivatePaymentKey _) = encodeAeson 0

instance DecodeAeson PrivatePaymentKey where
  decodeAeson _ = pure $ PrivatePaymentKey unit

newtype PrivateStakeKey = PrivateStakeKey Unit

derive instance Newtype PrivateStakeKey _

instance Show PrivateStakeKey where
  show _ = "(PrivateStakeKey Unit)"

instance EncodeAeson PrivateStakeKey where
  encodeAeson (PrivateStakeKey _) = encodeAeson 0

instance DecodeAeson PrivateStakeKey where
  decodeAeson _ = pure $ PrivateStakeKey unit

newtype PrivateDrepKey = PrivateDrepKey Unit

derive instance Newtype PrivateDrepKey _

instance Show PrivateDrepKey where
  show _ = "(PrivateDrepKey Unit)"

instance EncodeAeson PrivateDrepKey where
  encodeAeson (PrivateDrepKey _) = encodeAeson 0

instance DecodeAeson PrivateDrepKey where
  decodeAeson _ = pure $ PrivateDrepKey unit

privateKeyToPkh :: forall t. Newtype t PrivateKey => t -> Ed25519KeyHash
privateKeyToPkh _ = unsafeCrashWith "privateKeyToPkh: not implemented"

privateKeyToPkhCred :: forall t. Newtype t PrivateKey => t -> Credential
privateKeyToPkhCred _ = unsafeCrashWith "privateKeyToPkhCred: not implemented"

getPrivatePaymentKey :: KeyWallet -> Aff PrivatePaymentKey
getPrivatePaymentKey _ = unsafeCrashWith "getPrivatePaymentKey: not implemented"

getPrivateStakeKey :: KeyWallet -> Aff (Maybe PrivateStakeKey)
getPrivateStakeKey _ = unsafeCrashWith "getPrivateStakeKey: not implemented"

getPrivateDrepKey :: KeyWallet -> Aff (Maybe PrivateDrepKey)
getPrivateDrepKey _ = unsafeCrashWith "getPrivateDrepKey: not implemented"

privateKeysToAddress
  :: PrivatePaymentKey -> Maybe PrivateStakeKey -> NetworkId -> Address
privateKeysToAddress _ _ _ = unsafeCrashWith "privateKeysToAddress: not implemented"

privateKeysToKeyWallet
  :: PrivatePaymentKey
  -> Maybe PrivateStakeKey
  -> Maybe PrivateDrepKey
  -> KeyWallet
privateKeysToKeyWallet _ _ _ = unsafeCrashWith "privateKeysToAddress: not implemented"

