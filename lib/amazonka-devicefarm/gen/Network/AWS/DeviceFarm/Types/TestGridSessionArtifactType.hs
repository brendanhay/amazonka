{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.TestGridSessionArtifactType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.TestGridSessionArtifactType where

import Network.AWS.Prelude

data TestGridSessionArtifactType
  = SeleniumLog
  | Unknown
  | Video
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

instance FromText TestGridSessionArtifactType where
  parser =
    takeLowerText >>= \case
      "selenium_log" -> pure SeleniumLog
      "unknown" -> pure Unknown
      "video" -> pure Video
      e ->
        fromTextError $
          "Failure parsing TestGridSessionArtifactType from value: '" <> e
            <> "'. Accepted values: selenium_log, unknown, video"

instance ToText TestGridSessionArtifactType where
  toText = \case
    SeleniumLog -> "SELENIUM_LOG"
    Unknown -> "UNKNOWN"
    Video -> "VIDEO"

instance Hashable TestGridSessionArtifactType

instance NFData TestGridSessionArtifactType

instance ToByteString TestGridSessionArtifactType

instance ToQuery TestGridSessionArtifactType

instance ToHeader TestGridSessionArtifactType

instance FromJSON TestGridSessionArtifactType where
  parseJSON = parseJSONText "TestGridSessionArtifactType"
