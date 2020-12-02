{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AacSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AacSpecification where

import Network.AWS.Prelude

-- | Use MPEG-2 AAC instead of MPEG-4 AAC audio for raw or MPEG-2 Transport Stream containers.
data AacSpecification
  = ASMPEG2
  | ASMPEG4
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

instance FromText AacSpecification where
  parser =
    takeLowerText >>= \case
      "mpeg2" -> pure ASMPEG2
      "mpeg4" -> pure ASMPEG4
      e ->
        fromTextError $
          "Failure parsing AacSpecification from value: '" <> e
            <> "'. Accepted values: mpeg2, mpeg4"

instance ToText AacSpecification where
  toText = \case
    ASMPEG2 -> "MPEG2"
    ASMPEG4 -> "MPEG4"

instance Hashable AacSpecification

instance NFData AacSpecification

instance ToByteString AacSpecification

instance ToQuery AacSpecification

instance ToHeader AacSpecification

instance ToJSON AacSpecification where
  toJSON = toJSONText

instance FromJSON AacSpecification where
  parseJSON = parseJSONText "AacSpecification"
