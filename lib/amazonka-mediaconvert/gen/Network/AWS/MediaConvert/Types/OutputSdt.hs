{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.OutputSdt
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.OutputSdt where

import Network.AWS.Prelude

-- | Selects method of inserting SDT information into output stream.  "Follow input SDT" copies SDT information from input stream to  output stream. "Follow input SDT if present" copies SDT information from  input stream to output stream if SDT information is present in the input, otherwise it will fall back on the user-defined values. Enter "SDT  Manually" means user will enter the SDT information. "No SDT" means output  stream will not contain SDT information.
data OutputSdt
  = SdtFollow
  | SdtFollowIfPresent
  | SdtManual
  | SdtNone
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

instance FromText OutputSdt where
  parser =
    takeLowerText >>= \case
      "sdt_follow" -> pure SdtFollow
      "sdt_follow_if_present" -> pure SdtFollowIfPresent
      "sdt_manual" -> pure SdtManual
      "sdt_none" -> pure SdtNone
      e ->
        fromTextError $
          "Failure parsing OutputSdt from value: '" <> e
            <> "'. Accepted values: sdt_follow, sdt_follow_if_present, sdt_manual, sdt_none"

instance ToText OutputSdt where
  toText = \case
    SdtFollow -> "SDT_FOLLOW"
    SdtFollowIfPresent -> "SDT_FOLLOW_IF_PRESENT"
    SdtManual -> "SDT_MANUAL"
    SdtNone -> "SDT_NONE"

instance Hashable OutputSdt

instance NFData OutputSdt

instance ToByteString OutputSdt

instance ToQuery OutputSdt

instance ToHeader OutputSdt

instance ToJSON OutputSdt where
  toJSON = toJSONText

instance FromJSON OutputSdt where
  parseJSON = parseJSONText "OutputSdt"
