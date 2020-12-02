{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.RecordType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.RecordType where

import Network.AWS.Prelude

data RecordType
  = A
  | Aaaa
  | Cname
  | Srv
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

instance FromText RecordType where
  parser =
    takeLowerText >>= \case
      "a" -> pure A
      "aaaa" -> pure Aaaa
      "cname" -> pure Cname
      "srv" -> pure Srv
      e ->
        fromTextError $
          "Failure parsing RecordType from value: '" <> e
            <> "'. Accepted values: a, aaaa, cname, srv"

instance ToText RecordType where
  toText = \case
    A -> "A"
    Aaaa -> "AAAA"
    Cname -> "CNAME"
    Srv -> "SRV"

instance Hashable RecordType

instance NFData RecordType

instance ToByteString RecordType

instance ToQuery RecordType

instance ToHeader RecordType

instance ToJSON RecordType where
  toJSON = toJSONText

instance FromJSON RecordType where
  parseJSON = parseJSONText "RecordType"
