{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.Destination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.Destination where

import Network.AWS.Prelude

data Destination
  = CloudwatchLogs
  | S3
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

instance FromText Destination where
  parser =
    takeLowerText >>= \case
      "cloudwatch_logs" -> pure CloudwatchLogs
      "s3" -> pure S3
      e ->
        fromTextError $
          "Failure parsing Destination from value: '" <> e
            <> "'. Accepted values: cloudwatch_logs, s3"

instance ToText Destination where
  toText = \case
    CloudwatchLogs -> "CLOUDWATCH_LOGS"
    S3 -> "S3"

instance Hashable Destination

instance NFData Destination

instance ToByteString Destination

instance ToQuery Destination

instance ToHeader Destination

instance ToJSON Destination where
  toJSON = toJSONText

instance FromJSON Destination where
  parseJSON = parseJSONText "Destination"
