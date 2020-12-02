{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MxfProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MxfProfile where

import Network.AWS.Prelude

-- | Specify the MXF profile, also called shim, for this output. When you choose Auto, MediaConvert chooses a profile based on the video codec and resolution. For a list of codecs supported with each MXF profile, see https://docs.aws.amazon.com/mediaconvert/latest/ug/codecs-supported-with-each-mxf-profile.html. For more information about the automatic selection behavior, see https://docs.aws.amazon.com/mediaconvert/latest/ug/default-automatic-selection-of-mxf-profiles.html.
data MxfProfile
  = MPD10
  | MPOP1A
  | MPXdcam
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

instance FromText MxfProfile where
  parser =
    takeLowerText >>= \case
      "d_10" -> pure MPD10
      "op1a" -> pure MPOP1A
      "xdcam" -> pure MPXdcam
      e ->
        fromTextError $
          "Failure parsing MxfProfile from value: '" <> e
            <> "'. Accepted values: d_10, op1a, xdcam"

instance ToText MxfProfile where
  toText = \case
    MPD10 -> "D_10"
    MPOP1A -> "OP1A"
    MPXdcam -> "XDCAM"

instance Hashable MxfProfile

instance NFData MxfProfile

instance ToByteString MxfProfile

instance ToQuery MxfProfile

instance ToHeader MxfProfile

instance ToJSON MxfProfile where
  toJSON = toJSONText

instance FromJSON MxfProfile where
  parseJSON = parseJSONText "MxfProfile"
