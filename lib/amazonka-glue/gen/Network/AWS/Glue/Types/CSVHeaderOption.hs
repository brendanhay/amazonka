{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CSVHeaderOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CSVHeaderOption where

import Network.AWS.Prelude

data CSVHeaderOption
  = Absent
  | Present
  | Unknown
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

instance FromText CSVHeaderOption where
  parser =
    takeLowerText >>= \case
      "absent" -> pure Absent
      "present" -> pure Present
      "unknown" -> pure Unknown
      e ->
        fromTextError $
          "Failure parsing CSVHeaderOption from value: '" <> e
            <> "'. Accepted values: absent, present, unknown"

instance ToText CSVHeaderOption where
  toText = \case
    Absent -> "ABSENT"
    Present -> "PRESENT"
    Unknown -> "UNKNOWN"

instance Hashable CSVHeaderOption

instance NFData CSVHeaderOption

instance ToByteString CSVHeaderOption

instance ToQuery CSVHeaderOption

instance ToHeader CSVHeaderOption

instance ToJSON CSVHeaderOption where
  toJSON = toJSONText

instance FromJSON CSVHeaderOption where
  parseJSON = parseJSONText "CSVHeaderOption"
