{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.Types.Record
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDBStreams.Types.Record where

import Network.AWS.DynamoDBStreams.Types.Identity
import Network.AWS.DynamoDBStreams.Types.OperationType
import Network.AWS.DynamoDBStreams.Types.StreamRecord
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A description of a unique event within a stream.
--
--
--
-- /See:/ 'record' smart constructor.
data Record = Record'
  { _rUserIdentity :: !(Maybe Identity),
    _rEventVersion :: !(Maybe Text),
    _rDynamodb :: !(Maybe StreamRecord),
    _rAwsRegion :: !(Maybe Text),
    _rEventName :: !(Maybe OperationType),
    _rEventSource :: !(Maybe Text),
    _rEventId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Record' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rUserIdentity' - Items that are deleted by the Time to Live process after expiration have the following fields:      * Records[].userIdentity.type "Service"     * Records[].userIdentity.principalId "dynamodb.amazonaws.com"
--
-- * 'rEventVersion' - The version number of the stream record format. This number is updated whenever the structure of @Record@ is modified. Client applications must not assume that @eventVersion@ will remain at a particular value, as this number is subject to change at any time. In general, @eventVersion@ will only increase as the low-level DynamoDB Streams API evolves.
--
-- * 'rDynamodb' - The main body of the stream record, containing all of the DynamoDB-specific fields.
--
-- * 'rAwsRegion' - The region in which the @GetRecords@ request was received.
--
-- * 'rEventName' - The type of data modification that was performed on the DynamoDB table:     * @INSERT@ - a new item was added to the table.     * @MODIFY@ - one or more of an existing item's attributes were modified.     * @REMOVE@ - the item was deleted from the table
--
-- * 'rEventSource' - The AWS service from which the stream record originated. For DynamoDB Streams, this is @aws:dynamodb@ .
--
-- * 'rEventId' - A globally unique identifier for the event that was recorded in this stream record.
record ::
  Record
record =
  Record'
    { _rUserIdentity = Nothing,
      _rEventVersion = Nothing,
      _rDynamodb = Nothing,
      _rAwsRegion = Nothing,
      _rEventName = Nothing,
      _rEventSource = Nothing,
      _rEventId = Nothing
    }

-- | Items that are deleted by the Time to Live process after expiration have the following fields:      * Records[].userIdentity.type "Service"     * Records[].userIdentity.principalId "dynamodb.amazonaws.com"
rUserIdentity :: Lens' Record (Maybe Identity)
rUserIdentity = lens _rUserIdentity (\s a -> s {_rUserIdentity = a})

-- | The version number of the stream record format. This number is updated whenever the structure of @Record@ is modified. Client applications must not assume that @eventVersion@ will remain at a particular value, as this number is subject to change at any time. In general, @eventVersion@ will only increase as the low-level DynamoDB Streams API evolves.
rEventVersion :: Lens' Record (Maybe Text)
rEventVersion = lens _rEventVersion (\s a -> s {_rEventVersion = a})

-- | The main body of the stream record, containing all of the DynamoDB-specific fields.
rDynamodb :: Lens' Record (Maybe StreamRecord)
rDynamodb = lens _rDynamodb (\s a -> s {_rDynamodb = a})

-- | The region in which the @GetRecords@ request was received.
rAwsRegion :: Lens' Record (Maybe Text)
rAwsRegion = lens _rAwsRegion (\s a -> s {_rAwsRegion = a})

-- | The type of data modification that was performed on the DynamoDB table:     * @INSERT@ - a new item was added to the table.     * @MODIFY@ - one or more of an existing item's attributes were modified.     * @REMOVE@ - the item was deleted from the table
rEventName :: Lens' Record (Maybe OperationType)
rEventName = lens _rEventName (\s a -> s {_rEventName = a})

-- | The AWS service from which the stream record originated. For DynamoDB Streams, this is @aws:dynamodb@ .
rEventSource :: Lens' Record (Maybe Text)
rEventSource = lens _rEventSource (\s a -> s {_rEventSource = a})

-- | A globally unique identifier for the event that was recorded in this stream record.
rEventId :: Lens' Record (Maybe Text)
rEventId = lens _rEventId (\s a -> s {_rEventId = a})

instance FromJSON Record where
  parseJSON =
    withObject
      "Record"
      ( \x ->
          Record'
            <$> (x .:? "userIdentity")
            <*> (x .:? "eventVersion")
            <*> (x .:? "dynamodb")
            <*> (x .:? "awsRegion")
            <*> (x .:? "eventName")
            <*> (x .:? "eventSource")
            <*> (x .:? "eventID")
      )

instance Hashable Record

instance NFData Record
