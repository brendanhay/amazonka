{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.ChildType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.ChildType where

import Network.AWS.Prelude

data ChildType
  = CTAccount
  | CTOrganizationalUnit
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

instance FromText ChildType where
  parser =
    takeLowerText >>= \case
      "account" -> pure CTAccount
      "organizational_unit" -> pure CTOrganizationalUnit
      e ->
        fromTextError $
          "Failure parsing ChildType from value: '" <> e
            <> "'. Accepted values: account, organizational_unit"

instance ToText ChildType where
  toText = \case
    CTAccount -> "ACCOUNT"
    CTOrganizationalUnit -> "ORGANIZATIONAL_UNIT"

instance Hashable ChildType

instance NFData ChildType

instance ToByteString ChildType

instance ToQuery ChildType

instance ToHeader ChildType

instance ToJSON ChildType where
  toJSON = toJSONText

instance FromJSON ChildType where
  parseJSON = parseJSONText "ChildType"
