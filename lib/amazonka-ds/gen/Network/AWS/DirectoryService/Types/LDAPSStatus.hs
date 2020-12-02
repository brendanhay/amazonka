{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.LDAPSStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.LDAPSStatus where

import Network.AWS.Prelude

data LDAPSStatus
  = LDAPSSDisabled
  | LDAPSSEnableFailed
  | LDAPSSEnabled
  | LDAPSSEnabling
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

instance FromText LDAPSStatus where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure LDAPSSDisabled
      "enablefailed" -> pure LDAPSSEnableFailed
      "enabled" -> pure LDAPSSEnabled
      "enabling" -> pure LDAPSSEnabling
      e ->
        fromTextError $
          "Failure parsing LDAPSStatus from value: '" <> e
            <> "'. Accepted values: disabled, enablefailed, enabled, enabling"

instance ToText LDAPSStatus where
  toText = \case
    LDAPSSDisabled -> "Disabled"
    LDAPSSEnableFailed -> "EnableFailed"
    LDAPSSEnabled -> "Enabled"
    LDAPSSEnabling -> "Enabling"

instance Hashable LDAPSStatus

instance NFData LDAPSStatus

instance ToByteString LDAPSStatus

instance ToQuery LDAPSStatus

instance ToHeader LDAPSStatus

instance FromJSON LDAPSStatus where
  parseJSON = parseJSONText "LDAPSStatus"
