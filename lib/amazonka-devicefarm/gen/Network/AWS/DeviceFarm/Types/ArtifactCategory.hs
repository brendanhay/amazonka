{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.ArtifactCategory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.ArtifactCategory where

import Network.AWS.Prelude

data ArtifactCategory
  = File
  | Log
  | Screenshot
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

instance FromText ArtifactCategory where
  parser =
    takeLowerText >>= \case
      "file" -> pure File
      "log" -> pure Log
      "screenshot" -> pure Screenshot
      e ->
        fromTextError $
          "Failure parsing ArtifactCategory from value: '" <> e
            <> "'. Accepted values: file, log, screenshot"

instance ToText ArtifactCategory where
  toText = \case
    File -> "FILE"
    Log -> "LOG"
    Screenshot -> "SCREENSHOT"

instance Hashable ArtifactCategory

instance NFData ArtifactCategory

instance ToByteString ArtifactCategory

instance ToQuery ArtifactCategory

instance ToHeader ArtifactCategory

instance ToJSON ArtifactCategory where
  toJSON = toJSONText
