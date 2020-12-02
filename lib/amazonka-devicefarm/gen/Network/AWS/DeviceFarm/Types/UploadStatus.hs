{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.UploadStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.UploadStatus where

import Network.AWS.Prelude

data UploadStatus
  = USFailed
  | USInitialized
  | USProcessing
  | USSucceeded
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

instance FromText UploadStatus where
  parser =
    takeLowerText >>= \case
      "failed" -> pure USFailed
      "initialized" -> pure USInitialized
      "processing" -> pure USProcessing
      "succeeded" -> pure USSucceeded
      e ->
        fromTextError $
          "Failure parsing UploadStatus from value: '" <> e
            <> "'. Accepted values: failed, initialized, processing, succeeded"

instance ToText UploadStatus where
  toText = \case
    USFailed -> "FAILED"
    USInitialized -> "INITIALIZED"
    USProcessing -> "PROCESSING"
    USSucceeded -> "SUCCEEDED"

instance Hashable UploadStatus

instance NFData UploadStatus

instance ToByteString UploadStatus

instance ToQuery UploadStatus

instance ToHeader UploadStatus

instance FromJSON UploadStatus where
  parseJSON = parseJSONText "UploadStatus"
