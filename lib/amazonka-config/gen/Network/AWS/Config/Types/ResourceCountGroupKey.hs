{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ResourceCountGroupKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ResourceCountGroupKey where

import Network.AWS.Prelude

data ResourceCountGroupKey
  = RCGKAWSRegion
  | RCGKAccountId
  | RCGKResourceType
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

instance FromText ResourceCountGroupKey where
  parser =
    takeLowerText >>= \case
      "aws_region" -> pure RCGKAWSRegion
      "account_id" -> pure RCGKAccountId
      "resource_type" -> pure RCGKResourceType
      e ->
        fromTextError $
          "Failure parsing ResourceCountGroupKey from value: '" <> e
            <> "'. Accepted values: aws_region, account_id, resource_type"

instance ToText ResourceCountGroupKey where
  toText = \case
    RCGKAWSRegion -> "AWS_REGION"
    RCGKAccountId -> "ACCOUNT_ID"
    RCGKResourceType -> "RESOURCE_TYPE"

instance Hashable ResourceCountGroupKey

instance NFData ResourceCountGroupKey

instance ToByteString ResourceCountGroupKey

instance ToQuery ResourceCountGroupKey

instance ToHeader ResourceCountGroupKey

instance ToJSON ResourceCountGroupKey where
  toJSON = toJSONText
