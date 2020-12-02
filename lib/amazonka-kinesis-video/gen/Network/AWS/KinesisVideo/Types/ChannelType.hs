{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.Types.ChannelType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideo.Types.ChannelType where

import Network.AWS.Prelude

data ChannelType = SingleMaster
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

instance FromText ChannelType where
  parser =
    takeLowerText >>= \case
      "single_master" -> pure SingleMaster
      e ->
        fromTextError $
          "Failure parsing ChannelType from value: '" <> e
            <> "'. Accepted values: single_master"

instance ToText ChannelType where
  toText = \case
    SingleMaster -> "SINGLE_MASTER"

instance Hashable ChannelType

instance NFData ChannelType

instance ToByteString ChannelType

instance ToQuery ChannelType

instance ToHeader ChannelType

instance ToJSON ChannelType where
  toJSON = toJSONText

instance FromJSON ChannelType where
  parseJSON = parseJSONText "ChannelType"
