{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.Types.StreamRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDBStreams.Types.StreamRecord where

import Network.AWS.DynamoDBStreams.Types.AttributeValue
import Network.AWS.DynamoDBStreams.Types.StreamViewType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A description of a single data modification that was performed on an item in a DynamoDB table.
--
--
--
-- /See:/ 'streamRecord' smart constructor.
data StreamRecord = StreamRecord'
  { _srSizeBytes :: !(Maybe Nat),
    _srSequenceNumber :: !(Maybe Text),
    _srApproximateCreationDateTime :: !(Maybe POSIX),
    _srStreamViewType :: !(Maybe StreamViewType),
    _srKeys :: !(Maybe (Map Text (AttributeValue))),
    _srOldImage :: !(Maybe (Map Text (AttributeValue))),
    _srNewImage :: !(Maybe (Map Text (AttributeValue)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StreamRecord' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srSizeBytes' - The size of the stream record, in bytes.
--
-- * 'srSequenceNumber' - The sequence number of the stream record.
--
-- * 'srApproximateCreationDateTime' - The approximate date and time when the stream record was created, in <http://www.epochconverter.com/ UNIX epoch time> format.
--
-- * 'srStreamViewType' - The type of data from the modified DynamoDB item that was captured in this stream record:     * @KEYS_ONLY@ - only the key attributes of the modified item.     * @NEW_IMAGE@ - the entire item, as it appeared after it was modified.     * @OLD_IMAGE@ - the entire item, as it appeared before it was modified.     * @NEW_AND_OLD_IMAGES@ - both the new and the old item images of the item.
--
-- * 'srKeys' - The primary key attribute(s) for the DynamoDB item that was modified.
--
-- * 'srOldImage' - The item in the DynamoDB table as it appeared before it was modified.
--
-- * 'srNewImage' - The item in the DynamoDB table as it appeared after it was modified.
streamRecord ::
  StreamRecord
streamRecord =
  StreamRecord'
    { _srSizeBytes = Nothing,
      _srSequenceNumber = Nothing,
      _srApproximateCreationDateTime = Nothing,
      _srStreamViewType = Nothing,
      _srKeys = Nothing,
      _srOldImage = Nothing,
      _srNewImage = Nothing
    }

-- | The size of the stream record, in bytes.
srSizeBytes :: Lens' StreamRecord (Maybe Natural)
srSizeBytes = lens _srSizeBytes (\s a -> s {_srSizeBytes = a}) . mapping _Nat

-- | The sequence number of the stream record.
srSequenceNumber :: Lens' StreamRecord (Maybe Text)
srSequenceNumber = lens _srSequenceNumber (\s a -> s {_srSequenceNumber = a})

-- | The approximate date and time when the stream record was created, in <http://www.epochconverter.com/ UNIX epoch time> format.
srApproximateCreationDateTime :: Lens' StreamRecord (Maybe UTCTime)
srApproximateCreationDateTime = lens _srApproximateCreationDateTime (\s a -> s {_srApproximateCreationDateTime = a}) . mapping _Time

-- | The type of data from the modified DynamoDB item that was captured in this stream record:     * @KEYS_ONLY@ - only the key attributes of the modified item.     * @NEW_IMAGE@ - the entire item, as it appeared after it was modified.     * @OLD_IMAGE@ - the entire item, as it appeared before it was modified.     * @NEW_AND_OLD_IMAGES@ - both the new and the old item images of the item.
srStreamViewType :: Lens' StreamRecord (Maybe StreamViewType)
srStreamViewType = lens _srStreamViewType (\s a -> s {_srStreamViewType = a})

-- | The primary key attribute(s) for the DynamoDB item that was modified.
srKeys :: Lens' StreamRecord (HashMap Text (AttributeValue))
srKeys = lens _srKeys (\s a -> s {_srKeys = a}) . _Default . _Map

-- | The item in the DynamoDB table as it appeared before it was modified.
srOldImage :: Lens' StreamRecord (HashMap Text (AttributeValue))
srOldImage = lens _srOldImage (\s a -> s {_srOldImage = a}) . _Default . _Map

-- | The item in the DynamoDB table as it appeared after it was modified.
srNewImage :: Lens' StreamRecord (HashMap Text (AttributeValue))
srNewImage = lens _srNewImage (\s a -> s {_srNewImage = a}) . _Default . _Map

instance FromJSON StreamRecord where
  parseJSON =
    withObject
      "StreamRecord"
      ( \x ->
          StreamRecord'
            <$> (x .:? "SizeBytes")
            <*> (x .:? "SequenceNumber")
            <*> (x .:? "ApproximateCreationDateTime")
            <*> (x .:? "StreamViewType")
            <*> (x .:? "Keys" .!= mempty)
            <*> (x .:? "OldImage" .!= mempty)
            <*> (x .:? "NewImage" .!= mempty)
      )

instance Hashable StreamRecord

instance NFData StreamRecord
