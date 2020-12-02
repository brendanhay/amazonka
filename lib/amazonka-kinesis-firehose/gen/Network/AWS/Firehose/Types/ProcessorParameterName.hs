{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ProcessorParameterName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ProcessorParameterName where

import Network.AWS.Prelude

data ProcessorParameterName
  = BufferIntervalInSeconds
  | BufferSizeInMBs
  | LambdaARN
  | NumberOfRetries
  | RoleARN
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

instance FromText ProcessorParameterName where
  parser =
    takeLowerText >>= \case
      "bufferintervalinseconds" -> pure BufferIntervalInSeconds
      "buffersizeinmbs" -> pure BufferSizeInMBs
      "lambdaarn" -> pure LambdaARN
      "numberofretries" -> pure NumberOfRetries
      "rolearn" -> pure RoleARN
      e ->
        fromTextError $
          "Failure parsing ProcessorParameterName from value: '" <> e
            <> "'. Accepted values: bufferintervalinseconds, buffersizeinmbs, lambdaarn, numberofretries, rolearn"

instance ToText ProcessorParameterName where
  toText = \case
    BufferIntervalInSeconds -> "BufferIntervalInSeconds"
    BufferSizeInMBs -> "BufferSizeInMBs"
    LambdaARN -> "LambdaArn"
    NumberOfRetries -> "NumberOfRetries"
    RoleARN -> "RoleArn"

instance Hashable ProcessorParameterName

instance NFData ProcessorParameterName

instance ToByteString ProcessorParameterName

instance ToQuery ProcessorParameterName

instance ToHeader ProcessorParameterName

instance ToJSON ProcessorParameterName where
  toJSON = toJSONText

instance FromJSON ProcessorParameterName where
  parseJSON = parseJSONText "ProcessorParameterName"
