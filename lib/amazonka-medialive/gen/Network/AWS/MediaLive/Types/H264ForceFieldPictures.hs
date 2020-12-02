{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264ForceFieldPictures
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264ForceFieldPictures where

import Network.AWS.Prelude

-- | H264 Force Field Pictures
data H264ForceFieldPictures
  = HFFPDisabled
  | HFFPEnabled
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

instance FromText H264ForceFieldPictures where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure HFFPDisabled
      "enabled" -> pure HFFPEnabled
      e ->
        fromTextError $
          "Failure parsing H264ForceFieldPictures from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText H264ForceFieldPictures where
  toText = \case
    HFFPDisabled -> "DISABLED"
    HFFPEnabled -> "ENABLED"

instance Hashable H264ForceFieldPictures

instance NFData H264ForceFieldPictures

instance ToByteString H264ForceFieldPictures

instance ToQuery H264ForceFieldPictures

instance ToHeader H264ForceFieldPictures

instance ToJSON H264ForceFieldPictures where
  toJSON = toJSONText

instance FromJSON H264ForceFieldPictures where
  parseJSON = parseJSONText "H264ForceFieldPictures"
