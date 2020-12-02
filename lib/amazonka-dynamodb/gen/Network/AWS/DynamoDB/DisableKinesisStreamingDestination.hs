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
-- Module      : Network.AWS.DynamoDB.DisableKinesisStreamingDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops replication from the DynamoDB table to the Kinesis data stream. This is done without deleting either of the resources.
module Network.AWS.DynamoDB.DisableKinesisStreamingDestination
  ( -- * Creating a Request
    disableKinesisStreamingDestination,
    DisableKinesisStreamingDestination,

    -- * Request Lenses
    dksdTableName,
    dksdStreamARN,

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

-- | /See:/ 'disableKinesisStreamingDestination' smart constructor.
data DisableKinesisStreamingDestination = DisableKinesisStreamingDestination'
  { _dksdTableName ::
      !Text,
    _dksdStreamARN ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisableKinesisStreamingDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dksdTableName' - The name of the DynamoDB table.
--
-- * 'dksdStreamARN' - The ARN for a Kinesis data stream.
disableKinesisStreamingDestination ::
  -- | 'dksdTableName'
  Text ->
  -- | 'dksdStreamARN'
  Text ->
  DisableKinesisStreamingDestination
disableKinesisStreamingDestination pTableName_ pStreamARN_ =
  DisableKinesisStreamingDestination'
    { _dksdTableName = pTableName_,
      _dksdStreamARN = pStreamARN_
    }

-- | The name of the DynamoDB table.
dksdTableName :: Lens' DisableKinesisStreamingDestination Text
dksdTableName = lens _dksdTableName (\s a -> s {_dksdTableName = a})

-- | The ARN for a Kinesis data stream.
dksdStreamARN :: Lens' DisableKinesisStreamingDestination Text
dksdStreamARN = lens _dksdStreamARN (\s a -> s {_dksdStreamARN = a})

instance AWSRequest DisableKinesisStreamingDestination where
  type
    Rs DisableKinesisStreamingDestination =
      KinesisStreamingDestinationOutput
  request = postJSON dynamoDB
  response = receiveJSON (\s h x -> eitherParseJSON x)

instance Hashable DisableKinesisStreamingDestination

instance NFData DisableKinesisStreamingDestination

instance ToHeaders DisableKinesisStreamingDestination where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "DynamoDB_20120810.DisableKinesisStreamingDestination" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.0" :: ByteString)
          ]
      )

instance ToJSON DisableKinesisStreamingDestination where
  toJSON DisableKinesisStreamingDestination' {..} =
    object
      ( catMaybes
          [ Just ("TableName" .= _dksdTableName),
            Just ("StreamArn" .= _dksdStreamARN)
          ]
      )

instance ToPath DisableKinesisStreamingDestination where
  toPath = const "/"

instance ToQuery DisableKinesisStreamingDestination where
  toQuery = const mempty
