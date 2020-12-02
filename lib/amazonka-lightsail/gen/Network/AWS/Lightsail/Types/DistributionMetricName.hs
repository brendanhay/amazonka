{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.DistributionMetricName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.DistributionMetricName where

import Network.AWS.Prelude

data DistributionMetricName
  = BytesDownloaded
  | BytesUploaded
  | HTTP4xxErrorRate
  | HTTP5xxErrorRate
  | Requests
  | TotalErrorRate
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

instance FromText DistributionMetricName where
  parser =
    takeLowerText >>= \case
      "bytesdownloaded" -> pure BytesDownloaded
      "bytesuploaded" -> pure BytesUploaded
      "http4xxerrorrate" -> pure HTTP4xxErrorRate
      "http5xxerrorrate" -> pure HTTP5xxErrorRate
      "requests" -> pure Requests
      "totalerrorrate" -> pure TotalErrorRate
      e ->
        fromTextError $
          "Failure parsing DistributionMetricName from value: '" <> e
            <> "'. Accepted values: bytesdownloaded, bytesuploaded, http4xxerrorrate, http5xxerrorrate, requests, totalerrorrate"

instance ToText DistributionMetricName where
  toText = \case
    BytesDownloaded -> "BytesDownloaded"
    BytesUploaded -> "BytesUploaded"
    HTTP4xxErrorRate -> "Http4xxErrorRate"
    HTTP5xxErrorRate -> "Http5xxErrorRate"
    Requests -> "Requests"
    TotalErrorRate -> "TotalErrorRate"

instance Hashable DistributionMetricName

instance NFData DistributionMetricName

instance ToByteString DistributionMetricName

instance ToQuery DistributionMetricName

instance ToHeader DistributionMetricName

instance ToJSON DistributionMetricName where
  toJSON = toJSONText

instance FromJSON DistributionMetricName where
  parseJSON = parseJSONText "DistributionMetricName"
