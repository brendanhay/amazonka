{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.StopActivityStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a database activity stream that was started using the AWS console, the @start-activity-stream@ AWS CLI command, or the @StartActivityStream@ action.
--
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/DBActivityStreams.html Database Activity Streams> in the /Amazon Aurora User Guide/ .
module Network.AWS.RDS.StopActivityStream
  ( -- * Creating a Request
    stopActivityStream,
    StopActivityStream,

    -- * Request Lenses
    sasApplyImmediately,
    sasResourceARN,

    -- * Destructuring the Response
    stopActivityStreamResponse,
    StopActivityStreamResponse,

    -- * Response Lenses
    sasrsStatus,
    sasrsKinesisStreamName,
    sasrsKMSKeyId,
    sasrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopActivityStream' smart constructor.
data StopActivityStream = StopActivityStream'
  { _sasApplyImmediately ::
      !(Maybe Bool),
    _sasResourceARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopActivityStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sasApplyImmediately' - Specifies whether or not the database activity stream is to stop as soon as possible, regardless of the maintenance window for the database.
--
-- * 'sasResourceARN' - The Amazon Resource Name (ARN) of the DB cluster for the database activity stream. For example, @arn:aws:rds:us-east-1:12345667890:cluster:das-cluster@ .
stopActivityStream ::
  -- | 'sasResourceARN'
  Text ->
  StopActivityStream
stopActivityStream pResourceARN_ =
  StopActivityStream'
    { _sasApplyImmediately = Nothing,
      _sasResourceARN = pResourceARN_
    }

-- | Specifies whether or not the database activity stream is to stop as soon as possible, regardless of the maintenance window for the database.
sasApplyImmediately :: Lens' StopActivityStream (Maybe Bool)
sasApplyImmediately = lens _sasApplyImmediately (\s a -> s {_sasApplyImmediately = a})

-- | The Amazon Resource Name (ARN) of the DB cluster for the database activity stream. For example, @arn:aws:rds:us-east-1:12345667890:cluster:das-cluster@ .
sasResourceARN :: Lens' StopActivityStream Text
sasResourceARN = lens _sasResourceARN (\s a -> s {_sasResourceARN = a})

instance AWSRequest StopActivityStream where
  type Rs StopActivityStream = StopActivityStreamResponse
  request = postQuery rds
  response =
    receiveXMLWrapper
      "StopActivityStreamResult"
      ( \s h x ->
          StopActivityStreamResponse'
            <$> (x .@? "Status")
            <*> (x .@? "KinesisStreamName")
            <*> (x .@? "KmsKeyId")
            <*> (pure (fromEnum s))
      )

instance Hashable StopActivityStream

instance NFData StopActivityStream

instance ToHeaders StopActivityStream where
  toHeaders = const mempty

instance ToPath StopActivityStream where
  toPath = const "/"

instance ToQuery StopActivityStream where
  toQuery StopActivityStream' {..} =
    mconcat
      [ "Action" =: ("StopActivityStream" :: ByteString),
        "Version" =: ("2014-10-31" :: ByteString),
        "ApplyImmediately" =: _sasApplyImmediately,
        "ResourceArn" =: _sasResourceARN
      ]

-- | /See:/ 'stopActivityStreamResponse' smart constructor.
data StopActivityStreamResponse = StopActivityStreamResponse'
  { _sasrsStatus ::
      !(Maybe ActivityStreamStatus),
    _sasrsKinesisStreamName ::
      !(Maybe Text),
    _sasrsKMSKeyId :: !(Maybe Text),
    _sasrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopActivityStreamResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sasrsStatus' - The status of the database activity stream.
--
-- * 'sasrsKinesisStreamName' - The name of the Amazon Kinesis data stream used for the database activity stream.
--
-- * 'sasrsKMSKeyId' - The AWS KMS key identifier used for encrypting messages in the database activity stream.
--
-- * 'sasrsResponseStatus' - -- | The response status code.
stopActivityStreamResponse ::
  -- | 'sasrsResponseStatus'
  Int ->
  StopActivityStreamResponse
stopActivityStreamResponse pResponseStatus_ =
  StopActivityStreamResponse'
    { _sasrsStatus = Nothing,
      _sasrsKinesisStreamName = Nothing,
      _sasrsKMSKeyId = Nothing,
      _sasrsResponseStatus = pResponseStatus_
    }

-- | The status of the database activity stream.
sasrsStatus :: Lens' StopActivityStreamResponse (Maybe ActivityStreamStatus)
sasrsStatus = lens _sasrsStatus (\s a -> s {_sasrsStatus = a})

-- | The name of the Amazon Kinesis data stream used for the database activity stream.
sasrsKinesisStreamName :: Lens' StopActivityStreamResponse (Maybe Text)
sasrsKinesisStreamName = lens _sasrsKinesisStreamName (\s a -> s {_sasrsKinesisStreamName = a})

-- | The AWS KMS key identifier used for encrypting messages in the database activity stream.
sasrsKMSKeyId :: Lens' StopActivityStreamResponse (Maybe Text)
sasrsKMSKeyId = lens _sasrsKMSKeyId (\s a -> s {_sasrsKMSKeyId = a})

-- | -- | The response status code.
sasrsResponseStatus :: Lens' StopActivityStreamResponse Int
sasrsResponseStatus = lens _sasrsResponseStatus (\s a -> s {_sasrsResponseStatus = a})

instance NFData StopActivityStreamResponse
