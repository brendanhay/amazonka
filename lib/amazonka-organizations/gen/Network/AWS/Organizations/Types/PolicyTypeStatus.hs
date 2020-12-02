{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.PolicyTypeStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.PolicyTypeStatus where

import Network.AWS.Prelude

data PolicyTypeStatus
  = Enabled
  | PendingDisable
  | PendingEnable
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

instance FromText PolicyTypeStatus where
  parser =
    takeLowerText >>= \case
      "enabled" -> pure Enabled
      "pending_disable" -> pure PendingDisable
      "pending_enable" -> pure PendingEnable
      e ->
        fromTextError $
          "Failure parsing PolicyTypeStatus from value: '" <> e
            <> "'. Accepted values: enabled, pending_disable, pending_enable"

instance ToText PolicyTypeStatus where
  toText = \case
    Enabled -> "ENABLED"
    PendingDisable -> "PENDING_DISABLE"
    PendingEnable -> "PENDING_ENABLE"

instance Hashable PolicyTypeStatus

instance NFData PolicyTypeStatus

instance ToByteString PolicyTypeStatus

instance ToQuery PolicyTypeStatus

instance ToHeader PolicyTypeStatus

instance FromJSON PolicyTypeStatus where
  parseJSON = parseJSONText "PolicyTypeStatus"
