{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.PredicateType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.PredicateType where

import Network.AWS.Prelude

data PredicateType
  = ByteMatch
  | GeoMatch
  | IPMatch
  | RegexMatch
  | SizeConstraint
  | SqlInjectionMatch
  | XSSMatch
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

instance FromText PredicateType where
  parser =
    takeLowerText >>= \case
      "bytematch" -> pure ByteMatch
      "geomatch" -> pure GeoMatch
      "ipmatch" -> pure IPMatch
      "regexmatch" -> pure RegexMatch
      "sizeconstraint" -> pure SizeConstraint
      "sqlinjectionmatch" -> pure SqlInjectionMatch
      "xssmatch" -> pure XSSMatch
      e ->
        fromTextError $
          "Failure parsing PredicateType from value: '" <> e
            <> "'. Accepted values: bytematch, geomatch, ipmatch, regexmatch, sizeconstraint, sqlinjectionmatch, xssmatch"

instance ToText PredicateType where
  toText = \case
    ByteMatch -> "ByteMatch"
    GeoMatch -> "GeoMatch"
    IPMatch -> "IPMatch"
    RegexMatch -> "RegexMatch"
    SizeConstraint -> "SizeConstraint"
    SqlInjectionMatch -> "SqlInjectionMatch"
    XSSMatch -> "XssMatch"

instance Hashable PredicateType

instance NFData PredicateType

instance ToByteString PredicateType

instance ToQuery PredicateType

instance ToHeader PredicateType

instance ToJSON PredicateType where
  toJSON = toJSONText

instance FromJSON PredicateType where
  parseJSON = parseJSONText "PredicateType"
