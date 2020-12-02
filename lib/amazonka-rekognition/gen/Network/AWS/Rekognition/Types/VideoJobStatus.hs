{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.VideoJobStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.VideoJobStatus where

import Network.AWS.Prelude

data VideoJobStatus
  = VJSFailed
  | VJSInProgress
  | VJSSucceeded
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

instance FromText VideoJobStatus where
  parser =
    takeLowerText >>= \case
      "failed" -> pure VJSFailed
      "in_progress" -> pure VJSInProgress
      "succeeded" -> pure VJSSucceeded
      e ->
        fromTextError $
          "Failure parsing VideoJobStatus from value: '" <> e
            <> "'. Accepted values: failed, in_progress, succeeded"

instance ToText VideoJobStatus where
  toText = \case
    VJSFailed -> "FAILED"
    VJSInProgress -> "IN_PROGRESS"
    VJSSucceeded -> "SUCCEEDED"

instance Hashable VideoJobStatus

instance NFData VideoJobStatus

instance ToByteString VideoJobStatus

instance ToQuery VideoJobStatus

instance ToHeader VideoJobStatus

instance FromJSON VideoJobStatus where
  parseJSON = parseJSONText "VideoJobStatus"
