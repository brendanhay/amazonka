{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.ParentType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.ParentType where

import Network.AWS.Prelude

data ParentType
  = OrganizationalUnit
  | Root
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

instance FromText ParentType where
  parser =
    takeLowerText >>= \case
      "organizational_unit" -> pure OrganizationalUnit
      "root" -> pure Root
      e ->
        fromTextError $
          "Failure parsing ParentType from value: '" <> e
            <> "'. Accepted values: organizational_unit, root"

instance ToText ParentType where
  toText = \case
    OrganizationalUnit -> "ORGANIZATIONAL_UNIT"
    Root -> "ROOT"

instance Hashable ParentType

instance NFData ParentType

instance ToByteString ParentType

instance ToQuery ParentType

instance ToHeader ParentType

instance FromJSON ParentType where
  parseJSON = parseJSONText "ParentType"
