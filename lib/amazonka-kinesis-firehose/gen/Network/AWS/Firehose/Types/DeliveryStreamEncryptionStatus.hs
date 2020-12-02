{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.DeliveryStreamEncryptionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.DeliveryStreamEncryptionStatus where

import Network.AWS.Prelude

data DeliveryStreamEncryptionStatus
  = DSESDisabled
  | DSESDisabling
  | DSESDisablingFailed
  | DSESEnabled
  | DSESEnabling
  | DSESEnablingFailed
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText DeliveryStreamEncryptionStatus where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure DSESDisabled
      "disabling" -> pure DSESDisabling
      "disabling_failed" -> pure DSESDisablingFailed
      "enabled" -> pure DSESEnabled
      "enabling" -> pure DSESEnabling
      "enabling_failed" -> pure DSESEnablingFailed
      e ->
        fromTextError $
          "Failure parsing DeliveryStreamEncryptionStatus from value: '" <> e
            <> "'. Accepted values: disabled, disabling, disabling_failed, enabled, enabling, enabling_failed"

instance ToText DeliveryStreamEncryptionStatus where
  toText = \case
    DSESDisabled -> "DISABLED"
    DSESDisabling -> "DISABLING"
    DSESDisablingFailed -> "DISABLING_FAILED"
    DSESEnabled -> "ENABLED"
    DSESEnabling -> "ENABLING"
    DSESEnablingFailed -> "ENABLING_FAILED"

instance Hashable DeliveryStreamEncryptionStatus

instance NFData DeliveryStreamEncryptionStatus

instance ToByteString DeliveryStreamEncryptionStatus

instance ToQuery DeliveryStreamEncryptionStatus

instance ToHeader DeliveryStreamEncryptionStatus

instance FromJSON DeliveryStreamEncryptionStatus where
  parseJSON = parseJSONText "DeliveryStreamEncryptionStatus"
