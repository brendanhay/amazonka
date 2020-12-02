{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.ContactType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.ContactType where

import Network.AWS.Prelude

data ContactType
  = Association
  | Company
  | Person
  | PublicBody
  | Reseller
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

instance FromText ContactType where
  parser =
    takeLowerText >>= \case
      "association" -> pure Association
      "company" -> pure Company
      "person" -> pure Person
      "public_body" -> pure PublicBody
      "reseller" -> pure Reseller
      e ->
        fromTextError $
          "Failure parsing ContactType from value: '" <> e
            <> "'. Accepted values: association, company, person, public_body, reseller"

instance ToText ContactType where
  toText = \case
    Association -> "ASSOCIATION"
    Company -> "COMPANY"
    Person -> "PERSON"
    PublicBody -> "PUBLIC_BODY"
    Reseller -> "RESELLER"

instance Hashable ContactType

instance NFData ContactType

instance ToByteString ContactType

instance ToQuery ContactType

instance ToHeader ContactType

instance ToJSON ContactType where
  toJSON = toJSONText

instance FromJSON ContactType where
  parseJSON = parseJSONText "ContactType"
