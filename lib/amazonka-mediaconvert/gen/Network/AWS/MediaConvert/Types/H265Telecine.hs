{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265Telecine
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265Telecine where

import Network.AWS.Prelude

-- | This field applies only if the Streams > Advanced > Framerate (framerate) field  is set to 29.970. This field works with the Streams > Advanced > Preprocessors > Deinterlacer  field (deinterlace_mode) and the Streams > Advanced > Interlaced Mode field (interlace_mode)  to identify the scan type for the output: Progressive, Interlaced, Hard Telecine or Soft Telecine. - Hard: produces 29.97i output from 23.976 input. - Soft: produces 23.976; the player converts this output to 29.97i.
data H265Telecine
  = HTHard
  | HTNone
  | HTSoft
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

instance FromText H265Telecine where
  parser =
    takeLowerText >>= \case
      "hard" -> pure HTHard
      "none" -> pure HTNone
      "soft" -> pure HTSoft
      e ->
        fromTextError $
          "Failure parsing H265Telecine from value: '" <> e
            <> "'. Accepted values: hard, none, soft"

instance ToText H265Telecine where
  toText = \case
    HTHard -> "HARD"
    HTNone -> "NONE"
    HTSoft -> "SOFT"

instance Hashable H265Telecine

instance NFData H265Telecine

instance ToByteString H265Telecine

instance ToQuery H265Telecine

instance ToHeader H265Telecine

instance ToJSON H265Telecine where
  toJSON = toJSONText

instance FromJSON H265Telecine where
  parseJSON = parseJSONText "H265Telecine"
