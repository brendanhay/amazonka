{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AacVbrQuality
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AacVbrQuality where

import Network.AWS.Prelude

-- | VBR Quality Level - Only used if rate_control_mode is VBR.
data AacVbrQuality
  = AVQHigh
  | AVQLow
  | AVQMediumHigh
  | AVQMediumLow
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

instance FromText AacVbrQuality where
  parser =
    takeLowerText >>= \case
      "high" -> pure AVQHigh
      "low" -> pure AVQLow
      "medium_high" -> pure AVQMediumHigh
      "medium_low" -> pure AVQMediumLow
      e ->
        fromTextError $
          "Failure parsing AacVbrQuality from value: '" <> e
            <> "'. Accepted values: high, low, medium_high, medium_low"

instance ToText AacVbrQuality where
  toText = \case
    AVQHigh -> "HIGH"
    AVQLow -> "LOW"
    AVQMediumHigh -> "MEDIUM_HIGH"
    AVQMediumLow -> "MEDIUM_LOW"

instance Hashable AacVbrQuality

instance NFData AacVbrQuality

instance ToByteString AacVbrQuality

instance ToQuery AacVbrQuality

instance ToHeader AacVbrQuality

instance ToJSON AacVbrQuality where
  toJSON = toJSONText

instance FromJSON AacVbrQuality where
  parseJSON = parseJSONText "AacVbrQuality"
