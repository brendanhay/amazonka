{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.ActionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.ActionType where

import Network.AWS.Prelude

data ActionType
  = AddOrganizationsServiceLinkedRole
  | ApproveAllFeatures
  | EnableAllFeatures
  | Invite
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

instance FromText ActionType where
  parser =
    takeLowerText >>= \case
      "add_organizations_service_linked_role" -> pure AddOrganizationsServiceLinkedRole
      "approve_all_features" -> pure ApproveAllFeatures
      "enable_all_features" -> pure EnableAllFeatures
      "invite" -> pure Invite
      e ->
        fromTextError $
          "Failure parsing ActionType from value: '" <> e
            <> "'. Accepted values: add_organizations_service_linked_role, approve_all_features, enable_all_features, invite"

instance ToText ActionType where
  toText = \case
    AddOrganizationsServiceLinkedRole -> "ADD_ORGANIZATIONS_SERVICE_LINKED_ROLE"
    ApproveAllFeatures -> "APPROVE_ALL_FEATURES"
    EnableAllFeatures -> "ENABLE_ALL_FEATURES"
    Invite -> "INVITE"

instance Hashable ActionType

instance NFData ActionType

instance ToByteString ActionType

instance ToQuery ActionType

instance ToHeader ActionType

instance ToJSON ActionType where
  toJSON = toJSONText

instance FromJSON ActionType where
  parseJSON = parseJSONText "ActionType"
