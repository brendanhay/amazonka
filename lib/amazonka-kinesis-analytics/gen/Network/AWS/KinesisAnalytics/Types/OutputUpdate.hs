{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.OutputUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.OutputUpdate where

import Network.AWS.KinesisAnalytics.Types.DestinationSchema
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutputUpdate
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutputUpdate
import Network.AWS.KinesisAnalytics.Types.LambdaOutputUpdate
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes updates to the output configuration identified by the @OutputId@ .
--
--
--
-- /See:/ 'outputUpdate' smart constructor.
data OutputUpdate = OutputUpdate'
  { _ouKinesisStreamsOutputUpdate ::
      !(Maybe KinesisStreamsOutputUpdate),
    _ouDestinationSchemaUpdate :: !(Maybe DestinationSchema),
    _ouKinesisFirehoseOutputUpdate ::
      !(Maybe KinesisFirehoseOutputUpdate),
    _ouNameUpdate :: !(Maybe Text),
    _ouLambdaOutputUpdate :: !(Maybe LambdaOutputUpdate),
    _ouOutputId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OutputUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ouKinesisStreamsOutputUpdate' - Describes an Amazon Kinesis stream as the destination for the output.
--
-- * 'ouDestinationSchemaUpdate' - Describes the data format when records are written to the destination. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output> .
--
-- * 'ouKinesisFirehoseOutputUpdate' - Describes an Amazon Kinesis Firehose delivery stream as the destination for the output.
--
-- * 'ouNameUpdate' - If you want to specify a different in-application stream for this output configuration, use this field to specify the new in-application stream name.
--
-- * 'ouLambdaOutputUpdate' - Describes an AWS Lambda function as the destination for the output.
--
-- * 'ouOutputId' - Identifies the specific output configuration that you want to update.
outputUpdate ::
  -- | 'ouOutputId'
  Text ->
  OutputUpdate
outputUpdate pOutputId_ =
  OutputUpdate'
    { _ouKinesisStreamsOutputUpdate = Nothing,
      _ouDestinationSchemaUpdate = Nothing,
      _ouKinesisFirehoseOutputUpdate = Nothing,
      _ouNameUpdate = Nothing,
      _ouLambdaOutputUpdate = Nothing,
      _ouOutputId = pOutputId_
    }

-- | Describes an Amazon Kinesis stream as the destination for the output.
ouKinesisStreamsOutputUpdate :: Lens' OutputUpdate (Maybe KinesisStreamsOutputUpdate)
ouKinesisStreamsOutputUpdate = lens _ouKinesisStreamsOutputUpdate (\s a -> s {_ouKinesisStreamsOutputUpdate = a})

-- | Describes the data format when records are written to the destination. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output> .
ouDestinationSchemaUpdate :: Lens' OutputUpdate (Maybe DestinationSchema)
ouDestinationSchemaUpdate = lens _ouDestinationSchemaUpdate (\s a -> s {_ouDestinationSchemaUpdate = a})

-- | Describes an Amazon Kinesis Firehose delivery stream as the destination for the output.
ouKinesisFirehoseOutputUpdate :: Lens' OutputUpdate (Maybe KinesisFirehoseOutputUpdate)
ouKinesisFirehoseOutputUpdate = lens _ouKinesisFirehoseOutputUpdate (\s a -> s {_ouKinesisFirehoseOutputUpdate = a})

-- | If you want to specify a different in-application stream for this output configuration, use this field to specify the new in-application stream name.
ouNameUpdate :: Lens' OutputUpdate (Maybe Text)
ouNameUpdate = lens _ouNameUpdate (\s a -> s {_ouNameUpdate = a})

-- | Describes an AWS Lambda function as the destination for the output.
ouLambdaOutputUpdate :: Lens' OutputUpdate (Maybe LambdaOutputUpdate)
ouLambdaOutputUpdate = lens _ouLambdaOutputUpdate (\s a -> s {_ouLambdaOutputUpdate = a})

-- | Identifies the specific output configuration that you want to update.
ouOutputId :: Lens' OutputUpdate Text
ouOutputId = lens _ouOutputId (\s a -> s {_ouOutputId = a})

instance Hashable OutputUpdate

instance NFData OutputUpdate

instance ToJSON OutputUpdate where
  toJSON OutputUpdate' {..} =
    object
      ( catMaybes
          [ ("KinesisStreamsOutputUpdate" .=)
              <$> _ouKinesisStreamsOutputUpdate,
            ("DestinationSchemaUpdate" .=) <$> _ouDestinationSchemaUpdate,
            ("KinesisFirehoseOutputUpdate" .=)
              <$> _ouKinesisFirehoseOutputUpdate,
            ("NameUpdate" .=) <$> _ouNameUpdate,
            ("LambdaOutputUpdate" .=) <$> _ouLambdaOutputUpdate,
            Just ("OutputId" .= _ouOutputId)
          ]
      )
