{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.Types.Stream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDBStreams.Types.Stream where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents all of the data describing a particular stream.
--
--
--
-- /See:/ 'stream' smart constructor.
data Stream = Stream'
  { _sStreamLabel :: !(Maybe Text),
    _sStreamARN :: !(Maybe Text),
    _sTableName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Stream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sStreamLabel' - A timestamp, in ISO 8601 format, for this stream. Note that @LatestStreamLabel@ is not a unique identifier for the stream, because it is possible that a stream from another table might have the same timestamp. However, the combination of the following three elements is guaranteed to be unique:     * the AWS customer ID.     * the table name     * the @StreamLabel@
--
-- * 'sStreamARN' - The Amazon Resource Name (ARN) for the stream.
--
-- * 'sTableName' - The DynamoDB table with which the stream is associated.
stream ::
  Stream
stream =
  Stream'
    { _sStreamLabel = Nothing,
      _sStreamARN = Nothing,
      _sTableName = Nothing
    }

-- | A timestamp, in ISO 8601 format, for this stream. Note that @LatestStreamLabel@ is not a unique identifier for the stream, because it is possible that a stream from another table might have the same timestamp. However, the combination of the following three elements is guaranteed to be unique:     * the AWS customer ID.     * the table name     * the @StreamLabel@
sStreamLabel :: Lens' Stream (Maybe Text)
sStreamLabel = lens _sStreamLabel (\s a -> s {_sStreamLabel = a})

-- | The Amazon Resource Name (ARN) for the stream.
sStreamARN :: Lens' Stream (Maybe Text)
sStreamARN = lens _sStreamARN (\s a -> s {_sStreamARN = a})

-- | The DynamoDB table with which the stream is associated.
sTableName :: Lens' Stream (Maybe Text)
sTableName = lens _sTableName (\s a -> s {_sTableName = a})

instance FromJSON Stream where
  parseJSON =
    withObject
      "Stream"
      ( \x ->
          Stream'
            <$> (x .:? "StreamLabel")
            <*> (x .:? "StreamArn")
            <*> (x .:? "TableName")
      )

instance Hashable Stream

instance NFData Stream
