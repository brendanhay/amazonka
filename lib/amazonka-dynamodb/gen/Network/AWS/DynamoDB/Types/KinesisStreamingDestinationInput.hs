{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.KinesisStreamingDestinationInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.KinesisStreamingDestinationInput where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | /See:/ 'kinesisStreamingDestinationInput' smart constructor.
data KinesisStreamingDestinationInput = KinesisStreamingDestinationInput'
  { _ksdiTableName ::
      !Text,
    _ksdiStreamARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KinesisStreamingDestinationInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ksdiTableName' - The name of the DynamoDB table.
--
-- * 'ksdiStreamARN' - The ARN for a Kinesis data stream.
kinesisStreamingDestinationInput ::
  -- | 'ksdiTableName'
  Text ->
  -- | 'ksdiStreamARN'
  Text ->
  KinesisStreamingDestinationInput
kinesisStreamingDestinationInput pTableName_ pStreamARN_ =
  KinesisStreamingDestinationInput'
    { _ksdiTableName = pTableName_,
      _ksdiStreamARN = pStreamARN_
    }

-- | The name of the DynamoDB table.
ksdiTableName :: Lens' KinesisStreamingDestinationInput Text
ksdiTableName = lens _ksdiTableName (\s a -> s {_ksdiTableName = a})

-- | The ARN for a Kinesis data stream.
ksdiStreamARN :: Lens' KinesisStreamingDestinationInput Text
ksdiStreamARN = lens _ksdiStreamARN (\s a -> s {_ksdiStreamARN = a})

instance Hashable KinesisStreamingDestinationInput

instance NFData KinesisStreamingDestinationInput

instance ToJSON KinesisStreamingDestinationInput where
  toJSON KinesisStreamingDestinationInput' {..} =
    object
      ( catMaybes
          [ Just ("TableName" .= _ksdiTableName),
            Just ("StreamArn" .= _ksdiStreamARN)
          ]
      )
