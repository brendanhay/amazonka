{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.OrganizationNodeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.OrganizationNodeType where

import Network.AWS.Prelude

data OrganizationNodeType
  = ONTAccount
  | ONTOrganization
  | ONTOrganizationalUnit
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

instance FromText OrganizationNodeType where
  parser =
    takeLowerText >>= \case
      "account" -> pure ONTAccount
      "organization" -> pure ONTOrganization
      "organizational_unit" -> pure ONTOrganizationalUnit
      e ->
        fromTextError $
          "Failure parsing OrganizationNodeType from value: '" <> e
            <> "'. Accepted values: account, organization, organizational_unit"

instance ToText OrganizationNodeType where
  toText = \case
    ONTAccount -> "ACCOUNT"
    ONTOrganization -> "ORGANIZATION"
    ONTOrganizationalUnit -> "ORGANIZATIONAL_UNIT"

instance Hashable OrganizationNodeType

instance NFData OrganizationNodeType

instance ToByteString OrganizationNodeType

instance ToQuery OrganizationNodeType

instance ToHeader OrganizationNodeType

instance ToJSON OrganizationNodeType where
  toJSON = toJSONText

instance FromJSON OrganizationNodeType where
  parseJSON = parseJSONText "OrganizationNodeType"
