{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.InsightType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.InsightType where

import Network.AWS.Prelude

data InsightType = APICallRateInsight
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

instance FromText InsightType where
  parser =
    takeLowerText >>= \case
      "apicallrateinsight" -> pure APICallRateInsight
      e ->
        fromTextError $
          "Failure parsing InsightType from value: '" <> e
            <> "'. Accepted values: apicallrateinsight"

instance ToText InsightType where
  toText = \case
    APICallRateInsight -> "ApiCallRateInsight"

instance Hashable InsightType

instance NFData InsightType

instance ToByteString InsightType

instance ToQuery InsightType

instance ToHeader InsightType

instance ToJSON InsightType where
  toJSON = toJSONText

instance FromJSON InsightType where
  parseJSON = parseJSONText "InsightType"
