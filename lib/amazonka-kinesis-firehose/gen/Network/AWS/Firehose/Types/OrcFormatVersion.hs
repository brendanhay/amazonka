{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.OrcFormatVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.OrcFormatVersion where

import Network.AWS.Prelude

data OrcFormatVersion
  = V011
  | V012
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

instance FromText OrcFormatVersion where
  parser =
    takeLowerText >>= \case
      "v0_11" -> pure V011
      "v0_12" -> pure V012
      e ->
        fromTextError $
          "Failure parsing OrcFormatVersion from value: '" <> e
            <> "'. Accepted values: v0_11, v0_12"

instance ToText OrcFormatVersion where
  toText = \case
    V011 -> "V0_11"
    V012 -> "V0_12"

instance Hashable OrcFormatVersion

instance NFData OrcFormatVersion

instance ToByteString OrcFormatVersion

instance ToQuery OrcFormatVersion

instance ToHeader OrcFormatVersion

instance ToJSON OrcFormatVersion where
  toJSON = toJSONText

instance FromJSON OrcFormatVersion where
  parseJSON = parseJSONText "OrcFormatVersion"
