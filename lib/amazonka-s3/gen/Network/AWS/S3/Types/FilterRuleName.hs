{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.FilterRuleName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.FilterRuleName where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data FilterRuleName
  = Prefix
  | Suffix
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

instance FromText FilterRuleName where
  parser =
    takeLowerText >>= \case
      "prefix" -> pure Prefix
      "suffix" -> pure Suffix
      e ->
        fromTextError $
          "Failure parsing FilterRuleName from value: '" <> e
            <> "'. Accepted values: prefix, suffix"

instance ToText FilterRuleName where
  toText = \case
    Prefix -> "prefix"
    Suffix -> "suffix"

instance Hashable FilterRuleName

instance NFData FilterRuleName

instance ToByteString FilterRuleName

instance ToQuery FilterRuleName

instance ToHeader FilterRuleName

instance FromXML FilterRuleName where
  parseXML = parseXMLText "FilterRuleName"

instance ToXML FilterRuleName where
  toXML = toXMLText
