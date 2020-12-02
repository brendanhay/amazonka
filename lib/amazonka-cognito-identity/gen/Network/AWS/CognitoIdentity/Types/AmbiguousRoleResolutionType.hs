{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types.AmbiguousRoleResolutionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentity.Types.AmbiguousRoleResolutionType where

import Network.AWS.Prelude

data AmbiguousRoleResolutionType
  = AuthenticatedRole
  | Deny
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

instance FromText AmbiguousRoleResolutionType where
  parser =
    takeLowerText >>= \case
      "authenticatedrole" -> pure AuthenticatedRole
      "deny" -> pure Deny
      e ->
        fromTextError $
          "Failure parsing AmbiguousRoleResolutionType from value: '" <> e
            <> "'. Accepted values: authenticatedrole, deny"

instance ToText AmbiguousRoleResolutionType where
  toText = \case
    AuthenticatedRole -> "AuthenticatedRole"
    Deny -> "Deny"

instance Hashable AmbiguousRoleResolutionType

instance NFData AmbiguousRoleResolutionType

instance ToByteString AmbiguousRoleResolutionType

instance ToQuery AmbiguousRoleResolutionType

instance ToHeader AmbiguousRoleResolutionType

instance ToJSON AmbiguousRoleResolutionType where
  toJSON = toJSONText

instance FromJSON AmbiguousRoleResolutionType where
  parseJSON = parseJSONText "AmbiguousRoleResolutionType"
