{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AlphaBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AlphaBehavior where

import Network.AWS.Prelude

-- | Ignore this setting unless this input is a QuickTime animation with an alpha channel. Use this setting to create separate Key and Fill outputs. In each output, specify which part of the input MediaConvert uses. Leave this setting at the default value DISCARD to delete the alpha channel and preserve the video. Set it to REMAP_TO_LUMA to delete the video and map the alpha channel to the luma channel of your outputs.
data AlphaBehavior
  = Discard
  | RemapToLuma
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

instance FromText AlphaBehavior where
  parser =
    takeLowerText >>= \case
      "discard" -> pure Discard
      "remap_to_luma" -> pure RemapToLuma
      e ->
        fromTextError $
          "Failure parsing AlphaBehavior from value: '" <> e
            <> "'. Accepted values: discard, remap_to_luma"

instance ToText AlphaBehavior where
  toText = \case
    Discard -> "DISCARD"
    RemapToLuma -> "REMAP_TO_LUMA"

instance Hashable AlphaBehavior

instance NFData AlphaBehavior

instance ToByteString AlphaBehavior

instance ToQuery AlphaBehavior

instance ToHeader AlphaBehavior

instance ToJSON AlphaBehavior where
  toJSON = toJSONText

instance FromJSON AlphaBehavior where
  parseJSON = parseJSONText "AlphaBehavior"
