{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ApplicationVersionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ApplicationVersionStatus where

import Network.AWS.Prelude

data ApplicationVersionStatus
  = AVSBuilding
  | AVSFailed
  | AVSProcessed
  | AVSProcessing
  | AVSUnprocessed
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

instance FromText ApplicationVersionStatus where
  parser =
    takeLowerText >>= \case
      "building" -> pure AVSBuilding
      "failed" -> pure AVSFailed
      "processed" -> pure AVSProcessed
      "processing" -> pure AVSProcessing
      "unprocessed" -> pure AVSUnprocessed
      e ->
        fromTextError $
          "Failure parsing ApplicationVersionStatus from value: '" <> e
            <> "'. Accepted values: building, failed, processed, processing, unprocessed"

instance ToText ApplicationVersionStatus where
  toText = \case
    AVSBuilding -> "Building"
    AVSFailed -> "Failed"
    AVSProcessed -> "Processed"
    AVSProcessing -> "Processing"
    AVSUnprocessed -> "Unprocessed"

instance Hashable ApplicationVersionStatus

instance NFData ApplicationVersionStatus

instance ToByteString ApplicationVersionStatus

instance ToQuery ApplicationVersionStatus

instance ToHeader ApplicationVersionStatus

instance FromXML ApplicationVersionStatus where
  parseXML = parseXMLText "ApplicationVersionStatus"
