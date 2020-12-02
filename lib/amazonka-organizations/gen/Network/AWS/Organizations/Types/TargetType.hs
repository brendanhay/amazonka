{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.TargetType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.TargetType where

import Network.AWS.Prelude

data TargetType
  = TTAccount
  | TTOrganizationalUnit
  | TTRoot
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

instance FromText TargetType where
  parser =
    takeLowerText >>= \case
      "account" -> pure TTAccount
      "organizational_unit" -> pure TTOrganizationalUnit
      "root" -> pure TTRoot
      e ->
        fromTextError $
          "Failure parsing TargetType from value: '" <> e
            <> "'. Accepted values: account, organizational_unit, root"

instance ToText TargetType where
  toText = \case
    TTAccount -> "ACCOUNT"
    TTOrganizationalUnit -> "ORGANIZATIONAL_UNIT"
    TTRoot -> "ROOT"

instance Hashable TargetType

instance NFData TargetType

instance ToByteString TargetType

instance ToQuery TargetType

instance ToHeader TargetType

instance FromJSON TargetType where
  parseJSON = parseJSONText "TargetType"
