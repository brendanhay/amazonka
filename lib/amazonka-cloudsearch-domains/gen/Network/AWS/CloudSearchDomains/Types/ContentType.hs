{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.ContentType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types.ContentType where

import Network.AWS.Prelude

data ContentType
  = ApplicationJSON
  | ApplicationXML
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

instance FromText ContentType where
  parser =
    takeLowerText >>= \case
      "application/json" -> pure ApplicationJSON
      "application/xml" -> pure ApplicationXML
      e ->
        fromTextError $
          "Failure parsing ContentType from value: '" <> e
            <> "'. Accepted values: application/json, application/xml"

instance ToText ContentType where
  toText = \case
    ApplicationJSON -> "application/json"
    ApplicationXML -> "application/xml"

instance Hashable ContentType

instance NFData ContentType

instance ToByteString ContentType

instance ToQuery ContentType

instance ToHeader ContentType

instance ToJSON ContentType where
  toJSON = toJSONText
