{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.TestGridSessionArtifactCategory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.TestGridSessionArtifactCategory where

import Network.AWS.Prelude

data TestGridSessionArtifactCategory
  = TGSACLog
  | TGSACVideo
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

instance FromText TestGridSessionArtifactCategory where
  parser =
    takeLowerText >>= \case
      "log" -> pure TGSACLog
      "video" -> pure TGSACVideo
      e ->
        fromTextError $
          "Failure parsing TestGridSessionArtifactCategory from value: '" <> e
            <> "'. Accepted values: log, video"

instance ToText TestGridSessionArtifactCategory where
  toText = \case
    TGSACLog -> "LOG"
    TGSACVideo -> "VIDEO"

instance Hashable TestGridSessionArtifactCategory

instance NFData TestGridSessionArtifactCategory

instance ToByteString TestGridSessionArtifactCategory

instance ToQuery TestGridSessionArtifactCategory

instance ToHeader TestGridSessionArtifactCategory

instance ToJSON TestGridSessionArtifactCategory where
  toJSON = toJSONText
