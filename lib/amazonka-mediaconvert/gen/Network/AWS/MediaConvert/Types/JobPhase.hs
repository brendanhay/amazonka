{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.JobPhase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.JobPhase where

import Network.AWS.Prelude

-- | A job's phase can be PROBING, TRANSCODING OR UPLOADING
data JobPhase
  = Probing
  | Transcoding
  | Uploading
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

instance FromText JobPhase where
  parser =
    takeLowerText >>= \case
      "probing" -> pure Probing
      "transcoding" -> pure Transcoding
      "uploading" -> pure Uploading
      e ->
        fromTextError $
          "Failure parsing JobPhase from value: '" <> e
            <> "'. Accepted values: probing, transcoding, uploading"

instance ToText JobPhase where
  toText = \case
    Probing -> "PROBING"
    Transcoding -> "TRANSCODING"
    Uploading -> "UPLOADING"

instance Hashable JobPhase

instance NFData JobPhase

instance ToByteString JobPhase

instance ToQuery JobPhase

instance ToHeader JobPhase

instance FromJSON JobPhase where
  parseJSON = parseJSONText "JobPhase"
