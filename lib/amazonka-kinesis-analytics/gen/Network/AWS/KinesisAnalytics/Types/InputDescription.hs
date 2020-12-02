{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.InputDescription where

import Network.AWS.KinesisAnalytics.Types.InputParallelism
import Network.AWS.KinesisAnalytics.Types.InputProcessingConfigurationDescription
import Network.AWS.KinesisAnalytics.Types.InputStartingPositionConfiguration
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInputDescription
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsInputDescription
import Network.AWS.KinesisAnalytics.Types.SourceSchema
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the application input configuration. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> .
--
--
--
-- /See:/ 'inputDescription' smart constructor.
data InputDescription = InputDescription'
  { _idInputStartingPositionConfiguration ::
      !(Maybe InputStartingPositionConfiguration),
    _idInputParallelism :: !(Maybe InputParallelism),
    _idInputId :: !(Maybe Text),
    _idInAppStreamNames :: !(Maybe [Text]),
    _idKinesisFirehoseInputDescription ::
      !(Maybe KinesisFirehoseInputDescription),
    _idInputSchema :: !(Maybe SourceSchema),
    _idKinesisStreamsInputDescription ::
      !(Maybe KinesisStreamsInputDescription),
    _idNamePrefix :: !(Maybe Text),
    _idInputProcessingConfigurationDescription ::
      !(Maybe InputProcessingConfigurationDescription)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idInputStartingPositionConfiguration' - Point at which the application is configured to read from the input stream.
--
-- * 'idInputParallelism' - Describes the configured parallelism (number of in-application streams mapped to the streaming source).
--
-- * 'idInputId' - Input ID associated with the application input. This is the ID that Amazon Kinesis Analytics assigns to each input configuration you add to your application.
--
-- * 'idInAppStreamNames' - Returns the in-application stream names that are mapped to the stream source.
--
-- * 'idKinesisFirehoseInputDescription' - If an Amazon Kinesis Firehose delivery stream is configured as a streaming source, provides the delivery stream's ARN and an IAM role that enables Amazon Kinesis Analytics to access the stream on your behalf.
--
-- * 'idInputSchema' - Describes the format of the data in the streaming source, and how each data element maps to corresponding columns in the in-application stream that is being created.
--
-- * 'idKinesisStreamsInputDescription' - If an Amazon Kinesis stream is configured as streaming source, provides Amazon Kinesis stream's Amazon Resource Name (ARN) and an IAM role that enables Amazon Kinesis Analytics to access the stream on your behalf.
--
-- * 'idNamePrefix' - In-application name prefix.
--
-- * 'idInputProcessingConfigurationDescription' - The description of the preprocessor that executes on records in this input before the application's code is run.
inputDescription ::
  InputDescription
inputDescription =
  InputDescription'
    { _idInputStartingPositionConfiguration =
        Nothing,
      _idInputParallelism = Nothing,
      _idInputId = Nothing,
      _idInAppStreamNames = Nothing,
      _idKinesisFirehoseInputDescription = Nothing,
      _idInputSchema = Nothing,
      _idKinesisStreamsInputDescription = Nothing,
      _idNamePrefix = Nothing,
      _idInputProcessingConfigurationDescription = Nothing
    }

-- | Point at which the application is configured to read from the input stream.
idInputStartingPositionConfiguration :: Lens' InputDescription (Maybe InputStartingPositionConfiguration)
idInputStartingPositionConfiguration = lens _idInputStartingPositionConfiguration (\s a -> s {_idInputStartingPositionConfiguration = a})

-- | Describes the configured parallelism (number of in-application streams mapped to the streaming source).
idInputParallelism :: Lens' InputDescription (Maybe InputParallelism)
idInputParallelism = lens _idInputParallelism (\s a -> s {_idInputParallelism = a})

-- | Input ID associated with the application input. This is the ID that Amazon Kinesis Analytics assigns to each input configuration you add to your application.
idInputId :: Lens' InputDescription (Maybe Text)
idInputId = lens _idInputId (\s a -> s {_idInputId = a})

-- | Returns the in-application stream names that are mapped to the stream source.
idInAppStreamNames :: Lens' InputDescription [Text]
idInAppStreamNames = lens _idInAppStreamNames (\s a -> s {_idInAppStreamNames = a}) . _Default . _Coerce

-- | If an Amazon Kinesis Firehose delivery stream is configured as a streaming source, provides the delivery stream's ARN and an IAM role that enables Amazon Kinesis Analytics to access the stream on your behalf.
idKinesisFirehoseInputDescription :: Lens' InputDescription (Maybe KinesisFirehoseInputDescription)
idKinesisFirehoseInputDescription = lens _idKinesisFirehoseInputDescription (\s a -> s {_idKinesisFirehoseInputDescription = a})

-- | Describes the format of the data in the streaming source, and how each data element maps to corresponding columns in the in-application stream that is being created.
idInputSchema :: Lens' InputDescription (Maybe SourceSchema)
idInputSchema = lens _idInputSchema (\s a -> s {_idInputSchema = a})

-- | If an Amazon Kinesis stream is configured as streaming source, provides Amazon Kinesis stream's Amazon Resource Name (ARN) and an IAM role that enables Amazon Kinesis Analytics to access the stream on your behalf.
idKinesisStreamsInputDescription :: Lens' InputDescription (Maybe KinesisStreamsInputDescription)
idKinesisStreamsInputDescription = lens _idKinesisStreamsInputDescription (\s a -> s {_idKinesisStreamsInputDescription = a})

-- | In-application name prefix.
idNamePrefix :: Lens' InputDescription (Maybe Text)
idNamePrefix = lens _idNamePrefix (\s a -> s {_idNamePrefix = a})

-- | The description of the preprocessor that executes on records in this input before the application's code is run.
idInputProcessingConfigurationDescription :: Lens' InputDescription (Maybe InputProcessingConfigurationDescription)
idInputProcessingConfigurationDescription = lens _idInputProcessingConfigurationDescription (\s a -> s {_idInputProcessingConfigurationDescription = a})

instance FromJSON InputDescription where
  parseJSON =
    withObject
      "InputDescription"
      ( \x ->
          InputDescription'
            <$> (x .:? "InputStartingPositionConfiguration")
            <*> (x .:? "InputParallelism")
            <*> (x .:? "InputId")
            <*> (x .:? "InAppStreamNames" .!= mempty)
            <*> (x .:? "KinesisFirehoseInputDescription")
            <*> (x .:? "InputSchema")
            <*> (x .:? "KinesisStreamsInputDescription")
            <*> (x .:? "NamePrefix")
            <*> (x .:? "InputProcessingConfigurationDescription")
      )

instance Hashable InputDescription

instance NFData InputDescription
