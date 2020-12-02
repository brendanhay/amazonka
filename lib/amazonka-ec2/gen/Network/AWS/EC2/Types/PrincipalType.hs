{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PrincipalType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PrincipalType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data PrincipalType
  = PTAccount
  | PTAll
  | PTOrganizationUnit
  | PTRole
  | PTService
  | PTUser
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

instance FromText PrincipalType where
  parser =
    takeLowerText >>= \case
      "account" -> pure PTAccount
      "all" -> pure PTAll
      "organizationunit" -> pure PTOrganizationUnit
      "role" -> pure PTRole
      "service" -> pure PTService
      "user" -> pure PTUser
      e ->
        fromTextError $
          "Failure parsing PrincipalType from value: '" <> e
            <> "'. Accepted values: account, all, organizationunit, role, service, user"

instance ToText PrincipalType where
  toText = \case
    PTAccount -> "Account"
    PTAll -> "All"
    PTOrganizationUnit -> "OrganizationUnit"
    PTRole -> "Role"
    PTService -> "Service"
    PTUser -> "User"

instance Hashable PrincipalType

instance NFData PrincipalType

instance ToByteString PrincipalType

instance ToQuery PrincipalType

instance ToHeader PrincipalType

instance FromXML PrincipalType where
  parseXML = parseXMLText "PrincipalType"
