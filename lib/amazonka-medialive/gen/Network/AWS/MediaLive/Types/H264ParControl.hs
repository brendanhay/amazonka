{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264ParControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264ParControl where

import Network.AWS.Prelude

-- | H264 Par Control
data H264ParControl
  = InitializeFromSource
  | Specified
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

instance FromText H264ParControl where
  parser =
    takeLowerText >>= \case
      "initialize_from_source" -> pure InitializeFromSource
      "specified" -> pure Specified
      e ->
        fromTextError $
          "Failure parsing H264ParControl from value: '" <> e
            <> "'. Accepted values: initialize_from_source, specified"

instance ToText H264ParControl where
  toText = \case
    InitializeFromSource -> "INITIALIZE_FROM_SOURCE"
    Specified -> "SPECIFIED"

instance Hashable H264ParControl

instance NFData H264ParControl

instance ToByteString H264ParControl

instance ToQuery H264ParControl

instance ToHeader H264ParControl

instance ToJSON H264ParControl where
  toJSON = toJSONText

instance FromJSON H264ParControl where
  parseJSON = parseJSONText "H264ParControl"
