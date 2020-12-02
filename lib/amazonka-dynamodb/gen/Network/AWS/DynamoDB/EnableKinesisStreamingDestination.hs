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
-- Module      : Network.AWS.DynamoDB.EnableKinesisStreamingDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts table data replication to the specified Kinesis data stream at a timestamp chosen during the enable workflow. If this operation doesn't return results immediately, use DescribeKinesisStreamingDestination to check if streaming to the Kinesis data stream is ACTIVE.
module Network.AWS.DynamoDB.EnableKinesisStreamingDestination
  ( -- * Creating a Request
    enableKinesisStreamingDestination,
    EnableKinesisStreamingDestination,

    -- * Request Lenses
    eksdTableName,
    eksdStreamARN,

    -- * Destructuring the Response
    kinesisStreamingDestinationOutput,
    KinesisStreamingDestinationOutput,

    -- * Response Lenses
    ksdoDestinationStatus,
    ksdoStreamARN,
    ksdoTableName,
  )
where

import Network.AWS.DynamoDB.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'enableKinesisStreamingDestination' smart constructor.
data EnableKinesisStreamingDestination = EnableKinesisStreamingDestination'
  { _eksdTableName ::
      !Text,
    _eksdStreamARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnableKinesisStreamingDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eksdTableName' - The name of the DynamoDB table.
--
-- * 'eksdStreamARN' - The ARN for a Kinesis data stream.
enableKinesisStreamingDestination ::
  -- | 'eksdTableName'
  Text ->
  -- | 'eksdStreamARN'
  Text ->
  EnableKinesisStreamingDestination
enableKinesisStreamingDestination pTableName_ pStreamARN_ =
  EnableKinesisStreamingDestination'
    { _eksdTableName = pTableName_,
      _eksdStreamARN = pStreamARN_
    }

-- | The name of the DynamoDB table.
eksdTableName :: Lens' EnableKinesisStreamingDestination Text
eksdTableName = lens _eksdTableName (\s a -> s {_eksdTableName = a})

-- | The ARN for a Kinesis data stream.
eksdStreamARN :: Lens' EnableKinesisStreamingDestination Text
eksdStreamARN = lens _eksdStreamARN (\s a -> s {_eksdStreamARN = a})

instance AWSRequest EnableKinesisStreamingDestination where
  type
    Rs EnableKinesisStreamingDestination =
      KinesisStreamingDestinationOutput
  request = postJSON dynamoDB
  response = receiveJSON (\s h x -> eitherParseJSON x)

instance Hashable EnableKinesisStreamingDestination

instance NFData EnableKinesisStreamingDestination

instance ToHeaders EnableKinesisStreamingDestination where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "DynamoDB_20120810.EnableKinesisStreamingDestination" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.0" :: ByteString)
          ]
      )

instance ToJSON EnableKinesisStreamingDestination where
  toJSON EnableKinesisStreamingDestination' {..} =
    object
      ( catMaybes
          [ Just ("TableName" .= _eksdTableName),
            Just ("StreamArn" .= _eksdStreamARN)
          ]
      )

instance ToPath EnableKinesisStreamingDestination where
  toPath = const "/"

instance ToQuery EnableKinesisStreamingDestination where
  toQuery = const mempty
