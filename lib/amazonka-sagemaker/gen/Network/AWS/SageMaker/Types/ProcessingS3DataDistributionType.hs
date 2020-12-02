{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingS3DataDistributionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingS3DataDistributionType where

import Network.AWS.Prelude

data ProcessingS3DataDistributionType
  = PSDDTFullyReplicated
  | PSDDTShardedByS3Key
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

instance FromText ProcessingS3DataDistributionType where
  parser =
    takeLowerText >>= \case
      "fullyreplicated" -> pure PSDDTFullyReplicated
      "shardedbys3key" -> pure PSDDTShardedByS3Key
      e ->
        fromTextError $
          "Failure parsing ProcessingS3DataDistributionType from value: '" <> e
            <> "'. Accepted values: fullyreplicated, shardedbys3key"

instance ToText ProcessingS3DataDistributionType where
  toText = \case
    PSDDTFullyReplicated -> "FullyReplicated"
    PSDDTShardedByS3Key -> "ShardedByS3Key"

instance Hashable ProcessingS3DataDistributionType

instance NFData ProcessingS3DataDistributionType

instance ToByteString ProcessingS3DataDistributionType

instance ToQuery ProcessingS3DataDistributionType

instance ToHeader ProcessingS3DataDistributionType

instance ToJSON ProcessingS3DataDistributionType where
  toJSON = toJSONText

instance FromJSON ProcessingS3DataDistributionType where
  parseJSON = parseJSONText "ProcessingS3DataDistributionType"
