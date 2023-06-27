{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.MediaInsightsPipelineConfigurationElementType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.MediaInsightsPipelineConfigurationElementType
  ( MediaInsightsPipelineConfigurationElementType
      ( ..,
        MediaInsightsPipelineConfigurationElementType_AmazonTranscribeCallAnalyticsProcessor,
        MediaInsightsPipelineConfigurationElementType_AmazonTranscribeProcessor,
        MediaInsightsPipelineConfigurationElementType_KinesisDataStreamSink,
        MediaInsightsPipelineConfigurationElementType_LambdaFunctionSink,
        MediaInsightsPipelineConfigurationElementType_S3RecordingSink,
        MediaInsightsPipelineConfigurationElementType_SnsTopicSink,
        MediaInsightsPipelineConfigurationElementType_SqsQueueSink,
        MediaInsightsPipelineConfigurationElementType_VoiceAnalyticsProcessor
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MediaInsightsPipelineConfigurationElementType = MediaInsightsPipelineConfigurationElementType'
  { fromMediaInsightsPipelineConfigurationElementType ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern MediaInsightsPipelineConfigurationElementType_AmazonTranscribeCallAnalyticsProcessor :: MediaInsightsPipelineConfigurationElementType
pattern MediaInsightsPipelineConfigurationElementType_AmazonTranscribeCallAnalyticsProcessor = MediaInsightsPipelineConfigurationElementType' "AmazonTranscribeCallAnalyticsProcessor"

pattern MediaInsightsPipelineConfigurationElementType_AmazonTranscribeProcessor :: MediaInsightsPipelineConfigurationElementType
pattern MediaInsightsPipelineConfigurationElementType_AmazonTranscribeProcessor = MediaInsightsPipelineConfigurationElementType' "AmazonTranscribeProcessor"

pattern MediaInsightsPipelineConfigurationElementType_KinesisDataStreamSink :: MediaInsightsPipelineConfigurationElementType
pattern MediaInsightsPipelineConfigurationElementType_KinesisDataStreamSink = MediaInsightsPipelineConfigurationElementType' "KinesisDataStreamSink"

pattern MediaInsightsPipelineConfigurationElementType_LambdaFunctionSink :: MediaInsightsPipelineConfigurationElementType
pattern MediaInsightsPipelineConfigurationElementType_LambdaFunctionSink = MediaInsightsPipelineConfigurationElementType' "LambdaFunctionSink"

pattern MediaInsightsPipelineConfigurationElementType_S3RecordingSink :: MediaInsightsPipelineConfigurationElementType
pattern MediaInsightsPipelineConfigurationElementType_S3RecordingSink = MediaInsightsPipelineConfigurationElementType' "S3RecordingSink"

pattern MediaInsightsPipelineConfigurationElementType_SnsTopicSink :: MediaInsightsPipelineConfigurationElementType
pattern MediaInsightsPipelineConfigurationElementType_SnsTopicSink = MediaInsightsPipelineConfigurationElementType' "SnsTopicSink"

pattern MediaInsightsPipelineConfigurationElementType_SqsQueueSink :: MediaInsightsPipelineConfigurationElementType
pattern MediaInsightsPipelineConfigurationElementType_SqsQueueSink = MediaInsightsPipelineConfigurationElementType' "SqsQueueSink"

pattern MediaInsightsPipelineConfigurationElementType_VoiceAnalyticsProcessor :: MediaInsightsPipelineConfigurationElementType
pattern MediaInsightsPipelineConfigurationElementType_VoiceAnalyticsProcessor = MediaInsightsPipelineConfigurationElementType' "VoiceAnalyticsProcessor"

{-# COMPLETE
  MediaInsightsPipelineConfigurationElementType_AmazonTranscribeCallAnalyticsProcessor,
  MediaInsightsPipelineConfigurationElementType_AmazonTranscribeProcessor,
  MediaInsightsPipelineConfigurationElementType_KinesisDataStreamSink,
  MediaInsightsPipelineConfigurationElementType_LambdaFunctionSink,
  MediaInsightsPipelineConfigurationElementType_S3RecordingSink,
  MediaInsightsPipelineConfigurationElementType_SnsTopicSink,
  MediaInsightsPipelineConfigurationElementType_SqsQueueSink,
  MediaInsightsPipelineConfigurationElementType_VoiceAnalyticsProcessor,
  MediaInsightsPipelineConfigurationElementType'
  #-}
