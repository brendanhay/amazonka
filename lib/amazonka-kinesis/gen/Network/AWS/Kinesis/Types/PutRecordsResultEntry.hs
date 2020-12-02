{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.PutRecordsResultEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.PutRecordsResultEntry where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the result of an individual record from a @PutRecords@ request. A record that is successfully added to a stream includes @SequenceNumber@ and @ShardId@ in the result. A record that fails to be added to the stream includes @ErrorCode@ and @ErrorMessage@ in the result.
--
--
--
-- /See:/ 'putRecordsResultEntry' smart constructor.
data PutRecordsResultEntry = PutRecordsResultEntry'
  { _prreSequenceNumber ::
      !(Maybe Text),
    _prreErrorCode :: !(Maybe Text),
    _prreErrorMessage :: !(Maybe Text),
    _prreShardId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutRecordsResultEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prreSequenceNumber' - The sequence number for an individual record result.
--
-- * 'prreErrorCode' - The error code for an individual record result. @ErrorCodes@ can be either @ProvisionedThroughputExceededException@ or @InternalFailure@ .
--
-- * 'prreErrorMessage' - The error message for an individual record result. An @ErrorCode@ value of @ProvisionedThroughputExceededException@ has an error message that includes the account ID, stream name, and shard ID. An @ErrorCode@ value of @InternalFailure@ has the error message @"Internal Service Failure"@ .
--
-- * 'prreShardId' - The shard ID for an individual record result.
putRecordsResultEntry ::
  PutRecordsResultEntry
putRecordsResultEntry =
  PutRecordsResultEntry'
    { _prreSequenceNumber = Nothing,
      _prreErrorCode = Nothing,
      _prreErrorMessage = Nothing,
      _prreShardId = Nothing
    }

-- | The sequence number for an individual record result.
prreSequenceNumber :: Lens' PutRecordsResultEntry (Maybe Text)
prreSequenceNumber = lens _prreSequenceNumber (\s a -> s {_prreSequenceNumber = a})

-- | The error code for an individual record result. @ErrorCodes@ can be either @ProvisionedThroughputExceededException@ or @InternalFailure@ .
prreErrorCode :: Lens' PutRecordsResultEntry (Maybe Text)
prreErrorCode = lens _prreErrorCode (\s a -> s {_prreErrorCode = a})

-- | The error message for an individual record result. An @ErrorCode@ value of @ProvisionedThroughputExceededException@ has an error message that includes the account ID, stream name, and shard ID. An @ErrorCode@ value of @InternalFailure@ has the error message @"Internal Service Failure"@ .
prreErrorMessage :: Lens' PutRecordsResultEntry (Maybe Text)
prreErrorMessage = lens _prreErrorMessage (\s a -> s {_prreErrorMessage = a})

-- | The shard ID for an individual record result.
prreShardId :: Lens' PutRecordsResultEntry (Maybe Text)
prreShardId = lens _prreShardId (\s a -> s {_prreShardId = a})

instance FromJSON PutRecordsResultEntry where
  parseJSON =
    withObject
      "PutRecordsResultEntry"
      ( \x ->
          PutRecordsResultEntry'
            <$> (x .:? "SequenceNumber")
            <*> (x .:? "ErrorCode")
            <*> (x .:? "ErrorMessage")
            <*> (x .:? "ShardId")
      )

instance Hashable PutRecordsResultEntry

instance NFData PutRecordsResultEntry
