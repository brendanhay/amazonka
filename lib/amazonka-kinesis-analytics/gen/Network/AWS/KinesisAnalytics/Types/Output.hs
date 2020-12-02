{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.Output
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.Output where

import Network.AWS.KinesisAnalytics.Types.DestinationSchema
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutput
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutput
import Network.AWS.KinesisAnalytics.Types.LambdaOutput
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes application output configuration in which you identify an in-application stream and a destination where you want the in-application stream data to be written. The destination can be an Amazon Kinesis stream or an Amazon Kinesis Firehose delivery stream.
--
--
--
--
-- For limits on how many destinations an application can write and other limitations, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/limits.html Limits> .
--
--
-- /See:/ 'output' smart constructor.
data Output = Output'
  { _oLambdaOutput :: !(Maybe LambdaOutput),
    _oKinesisStreamsOutput :: !(Maybe KinesisStreamsOutput),
    _oKinesisFirehoseOutput :: !(Maybe KinesisFirehoseOutput),
    _oName :: !Text,
    _oDestinationSchema :: !DestinationSchema
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Output' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oLambdaOutput' - Identifies an AWS Lambda function as the destination.
--
-- * 'oKinesisStreamsOutput' - Identifies an Amazon Kinesis stream as the destination.
--
-- * 'oKinesisFirehoseOutput' - Identifies an Amazon Kinesis Firehose delivery stream as the destination.
--
-- * 'oName' - Name of the in-application stream.
--
-- * 'oDestinationSchema' - Describes the data format when records are written to the destination. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output> .
output ::
  -- | 'oName'
  Text ->
  -- | 'oDestinationSchema'
  DestinationSchema ->
  Output
output pName_ pDestinationSchema_ =
  Output'
    { _oLambdaOutput = Nothing,
      _oKinesisStreamsOutput = Nothing,
      _oKinesisFirehoseOutput = Nothing,
      _oName = pName_,
      _oDestinationSchema = pDestinationSchema_
    }

-- | Identifies an AWS Lambda function as the destination.
oLambdaOutput :: Lens' Output (Maybe LambdaOutput)
oLambdaOutput = lens _oLambdaOutput (\s a -> s {_oLambdaOutput = a})

-- | Identifies an Amazon Kinesis stream as the destination.
oKinesisStreamsOutput :: Lens' Output (Maybe KinesisStreamsOutput)
oKinesisStreamsOutput = lens _oKinesisStreamsOutput (\s a -> s {_oKinesisStreamsOutput = a})

-- | Identifies an Amazon Kinesis Firehose delivery stream as the destination.
oKinesisFirehoseOutput :: Lens' Output (Maybe KinesisFirehoseOutput)
oKinesisFirehoseOutput = lens _oKinesisFirehoseOutput (\s a -> s {_oKinesisFirehoseOutput = a})

-- | Name of the in-application stream.
oName :: Lens' Output Text
oName = lens _oName (\s a -> s {_oName = a})

-- | Describes the data format when records are written to the destination. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output> .
oDestinationSchema :: Lens' Output DestinationSchema
oDestinationSchema = lens _oDestinationSchema (\s a -> s {_oDestinationSchema = a})

instance Hashable Output

instance NFData Output

instance ToJSON Output where
  toJSON Output' {..} =
    object
      ( catMaybes
          [ ("LambdaOutput" .=) <$> _oLambdaOutput,
            ("KinesisStreamsOutput" .=) <$> _oKinesisStreamsOutput,
            ("KinesisFirehoseOutput" .=) <$> _oKinesisFirehoseOutput,
            Just ("Name" .= _oName),
            Just ("DestinationSchema" .= _oDestinationSchema)
          ]
      )
