{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.Input
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.Input where

import Network.AWS.KinesisAnalytics.Types.InputParallelism
import Network.AWS.KinesisAnalytics.Types.InputProcessingConfiguration
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInput
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsInput
import Network.AWS.KinesisAnalytics.Types.SourceSchema
import Network.AWS.Lens
import Network.AWS.Prelude

-- | When you configure the application input, you specify the streaming source, the in-application stream name that is created, and the mapping between the two. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> .
--
--
--
-- /See:/ 'input' smart constructor.
data Input = Input'
  { _iInputParallelism ::
      !(Maybe InputParallelism),
    _iInputProcessingConfiguration ::
      !(Maybe InputProcessingConfiguration),
    _iKinesisStreamsInput :: !(Maybe KinesisStreamsInput),
    _iKinesisFirehoseInput :: !(Maybe KinesisFirehoseInput),
    _iNamePrefix :: !Text,
    _iInputSchema :: !SourceSchema
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Input' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iInputParallelism' - Describes the number of in-application streams to create.  Data from your source is routed to these in-application input streams. (see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> .
--
-- * 'iInputProcessingConfiguration' - The <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputProcessingConfiguration.html InputProcessingConfiguration> for the input. An input processor transforms records as they are received from the stream, before the application's SQL code executes. Currently, the only input processing configuration available is <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessor.html InputLambdaProcessor> .
--
-- * 'iKinesisStreamsInput' - If the streaming source is an Amazon Kinesis stream, identifies the stream's Amazon Resource Name (ARN) and an IAM role that enables Amazon Kinesis Analytics to access the stream on your behalf. Note: Either @KinesisStreamsInput@ or @KinesisFirehoseInput@ is required.
--
-- * 'iKinesisFirehoseInput' - If the streaming source is an Amazon Kinesis Firehose delivery stream, identifies the delivery stream's ARN and an IAM role that enables Amazon Kinesis Analytics to access the stream on your behalf. Note: Either @KinesisStreamsInput@ or @KinesisFirehoseInput@ is required.
--
-- * 'iNamePrefix' - Name prefix to use when creating an in-application stream. Suppose that you specify a prefix "MyInApplicationStream." Amazon Kinesis Analytics then creates one or more (as per the @InputParallelism@ count you specified) in-application streams with names "MyInApplicationStream_001," "MyInApplicationStream_002," and so on.
--
-- * 'iInputSchema' - Describes the format of the data in the streaming source, and how each data element maps to corresponding columns in the in-application stream that is being created. Also used to describe the format of the reference data source.
input ::
  -- | 'iNamePrefix'
  Text ->
  -- | 'iInputSchema'
  SourceSchema ->
  Input
input pNamePrefix_ pInputSchema_ =
  Input'
    { _iInputParallelism = Nothing,
      _iInputProcessingConfiguration = Nothing,
      _iKinesisStreamsInput = Nothing,
      _iKinesisFirehoseInput = Nothing,
      _iNamePrefix = pNamePrefix_,
      _iInputSchema = pInputSchema_
    }

-- | Describes the number of in-application streams to create.  Data from your source is routed to these in-application input streams. (see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> .
iInputParallelism :: Lens' Input (Maybe InputParallelism)
iInputParallelism = lens _iInputParallelism (\s a -> s {_iInputParallelism = a})

-- | The <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputProcessingConfiguration.html InputProcessingConfiguration> for the input. An input processor transforms records as they are received from the stream, before the application's SQL code executes. Currently, the only input processing configuration available is <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessor.html InputLambdaProcessor> .
iInputProcessingConfiguration :: Lens' Input (Maybe InputProcessingConfiguration)
iInputProcessingConfiguration = lens _iInputProcessingConfiguration (\s a -> s {_iInputProcessingConfiguration = a})

-- | If the streaming source is an Amazon Kinesis stream, identifies the stream's Amazon Resource Name (ARN) and an IAM role that enables Amazon Kinesis Analytics to access the stream on your behalf. Note: Either @KinesisStreamsInput@ or @KinesisFirehoseInput@ is required.
iKinesisStreamsInput :: Lens' Input (Maybe KinesisStreamsInput)
iKinesisStreamsInput = lens _iKinesisStreamsInput (\s a -> s {_iKinesisStreamsInput = a})

-- | If the streaming source is an Amazon Kinesis Firehose delivery stream, identifies the delivery stream's ARN and an IAM role that enables Amazon Kinesis Analytics to access the stream on your behalf. Note: Either @KinesisStreamsInput@ or @KinesisFirehoseInput@ is required.
iKinesisFirehoseInput :: Lens' Input (Maybe KinesisFirehoseInput)
iKinesisFirehoseInput = lens _iKinesisFirehoseInput (\s a -> s {_iKinesisFirehoseInput = a})

-- | Name prefix to use when creating an in-application stream. Suppose that you specify a prefix "MyInApplicationStream." Amazon Kinesis Analytics then creates one or more (as per the @InputParallelism@ count you specified) in-application streams with names "MyInApplicationStream_001," "MyInApplicationStream_002," and so on.
iNamePrefix :: Lens' Input Text
iNamePrefix = lens _iNamePrefix (\s a -> s {_iNamePrefix = a})

-- | Describes the format of the data in the streaming source, and how each data element maps to corresponding columns in the in-application stream that is being created. Also used to describe the format of the reference data source.
iInputSchema :: Lens' Input SourceSchema
iInputSchema = lens _iInputSchema (\s a -> s {_iInputSchema = a})

instance Hashable Input

instance NFData Input

instance ToJSON Input where
  toJSON Input' {..} =
    object
      ( catMaybes
          [ ("InputParallelism" .=) <$> _iInputParallelism,
            ("InputProcessingConfiguration" .=)
              <$> _iInputProcessingConfiguration,
            ("KinesisStreamsInput" .=) <$> _iKinesisStreamsInput,
            ("KinesisFirehoseInput" .=) <$> _iKinesisFirehoseInput,
            Just ("NamePrefix" .= _iNamePrefix),
            Just ("InputSchema" .= _iInputSchema)
          ]
      )
