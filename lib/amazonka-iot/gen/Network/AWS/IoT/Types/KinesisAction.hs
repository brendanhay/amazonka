{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.KinesisAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.KinesisAction where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an action to write data to an Amazon Kinesis stream.
--
--
--
-- /See:/ 'kinesisAction' smart constructor.
data KinesisAction = KinesisAction'
  { _kaPartitionKey ::
      !(Maybe Text),
    _kaRoleARN :: !Text,
    _kaStreamName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KinesisAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kaPartitionKey' - The partition key.
--
-- * 'kaRoleARN' - The ARN of the IAM role that grants access to the Amazon Kinesis stream.
--
-- * 'kaStreamName' - The name of the Amazon Kinesis stream.
kinesisAction ::
  -- | 'kaRoleARN'
  Text ->
  -- | 'kaStreamName'
  Text ->
  KinesisAction
kinesisAction pRoleARN_ pStreamName_ =
  KinesisAction'
    { _kaPartitionKey = Nothing,
      _kaRoleARN = pRoleARN_,
      _kaStreamName = pStreamName_
    }

-- | The partition key.
kaPartitionKey :: Lens' KinesisAction (Maybe Text)
kaPartitionKey = lens _kaPartitionKey (\s a -> s {_kaPartitionKey = a})

-- | The ARN of the IAM role that grants access to the Amazon Kinesis stream.
kaRoleARN :: Lens' KinesisAction Text
kaRoleARN = lens _kaRoleARN (\s a -> s {_kaRoleARN = a})

-- | The name of the Amazon Kinesis stream.
kaStreamName :: Lens' KinesisAction Text
kaStreamName = lens _kaStreamName (\s a -> s {_kaStreamName = a})

instance FromJSON KinesisAction where
  parseJSON =
    withObject
      "KinesisAction"
      ( \x ->
          KinesisAction'
            <$> (x .:? "partitionKey")
            <*> (x .: "roleArn")
            <*> (x .: "streamName")
      )

instance Hashable KinesisAction

instance NFData KinesisAction

instance ToJSON KinesisAction where
  toJSON KinesisAction' {..} =
    object
      ( catMaybes
          [ ("partitionKey" .=) <$> _kaPartitionKey,
            Just ("roleArn" .= _kaRoleARN),
            Just ("streamName" .= _kaStreamName)
          ]
      )
