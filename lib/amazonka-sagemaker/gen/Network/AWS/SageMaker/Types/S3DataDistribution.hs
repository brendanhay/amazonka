{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.S3DataDistribution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.S3DataDistribution where

import Network.AWS.Prelude

data S3DataDistribution
  = FullyReplicated
  | ShardedByS3Key
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

instance FromText S3DataDistribution where
  parser =
    takeLowerText >>= \case
      "fullyreplicated" -> pure FullyReplicated
      "shardedbys3key" -> pure ShardedByS3Key
      e ->
        fromTextError $
          "Failure parsing S3DataDistribution from value: '" <> e
            <> "'. Accepted values: fullyreplicated, shardedbys3key"

instance ToText S3DataDistribution where
  toText = \case
    FullyReplicated -> "FullyReplicated"
    ShardedByS3Key -> "ShardedByS3Key"

instance Hashable S3DataDistribution

instance NFData S3DataDistribution

instance ToByteString S3DataDistribution

instance ToQuery S3DataDistribution

instance ToHeader S3DataDistribution

instance ToJSON S3DataDistribution where
  toJSON = toJSONText

instance FromJSON S3DataDistribution where
  parseJSON = parseJSONText "S3DataDistribution"
