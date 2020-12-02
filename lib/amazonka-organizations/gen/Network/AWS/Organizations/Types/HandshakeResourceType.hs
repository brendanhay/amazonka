{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.HandshakeResourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.HandshakeResourceType where

import Network.AWS.Prelude

data HandshakeResourceType
  = Account
  | Email
  | MasterEmail
  | MasterName
  | Notes
  | Organization
  | OrganizationFeatureSet
  | ParentHandshake
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

instance FromText HandshakeResourceType where
  parser =
    takeLowerText >>= \case
      "account" -> pure Account
      "email" -> pure Email
      "master_email" -> pure MasterEmail
      "master_name" -> pure MasterName
      "notes" -> pure Notes
      "organization" -> pure Organization
      "organization_feature_set" -> pure OrganizationFeatureSet
      "parent_handshake" -> pure ParentHandshake
      e ->
        fromTextError $
          "Failure parsing HandshakeResourceType from value: '" <> e
            <> "'. Accepted values: account, email, master_email, master_name, notes, organization, organization_feature_set, parent_handshake"

instance ToText HandshakeResourceType where
  toText = \case
    Account -> "ACCOUNT"
    Email -> "EMAIL"
    MasterEmail -> "MASTER_EMAIL"
    MasterName -> "MASTER_NAME"
    Notes -> "NOTES"
    Organization -> "ORGANIZATION"
    OrganizationFeatureSet -> "ORGANIZATION_FEATURE_SET"
    ParentHandshake -> "PARENT_HANDSHAKE"

instance Hashable HandshakeResourceType

instance NFData HandshakeResourceType

instance ToByteString HandshakeResourceType

instance ToQuery HandshakeResourceType

instance ToHeader HandshakeResourceType

instance FromJSON HandshakeResourceType where
  parseJSON = parseJSONText "HandshakeResourceType"
