{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EventStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EventStream where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies settings for publishing event data to an Amazon Kinesis data stream or an Amazon Kinesis Data Firehose delivery stream.
--
--
--
-- /See:/ 'eventStream' smart constructor.
data EventStream = EventStream'
  { _esLastUpdatedBy :: !(Maybe Text),
    _esLastModifiedDate :: !(Maybe Text),
    _esExternalId :: !(Maybe Text),
    _esApplicationId :: !Text,
    _esRoleARN :: !Text,
    _esDestinationStreamARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EventStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esLastUpdatedBy' - The IAM user who last modified the event stream.
--
-- * 'esLastModifiedDate' - The date, in ISO 8601 format, when the event stream was last modified.
--
-- * 'esExternalId' - (Deprecated) Your AWS account ID, which you assigned to an external ID key in an IAM trust policy. Amazon Pinpoint previously used this value to assume an IAM role when publishing event data, but we removed this requirement. We don't recommend use of external IDs for IAM roles that are assumed by Amazon Pinpoint.
--
-- * 'esApplicationId' - The unique identifier for the application to publish event data for.
--
-- * 'esRoleARN' - The AWS Identity and Access Management (IAM) role that authorizes Amazon Pinpoint to publish event data to the stream in your AWS account.
--
-- * 'esDestinationStreamARN' - The Amazon Resource Name (ARN) of the Amazon Kinesis data stream or Amazon Kinesis Data Firehose delivery stream to publish event data to. For a Kinesis data stream, the ARN format is: arn:aws:kinesis:<replaceable>region:<replaceable>account-id:stream/<replaceable>stream_name For a Kinesis Data Firehose delivery stream, the ARN format is: arn:aws:firehose:<replaceable>region:<replaceable>account-id:deliverystream/<replaceable>stream_name
eventStream ::
  -- | 'esApplicationId'
  Text ->
  -- | 'esRoleARN'
  Text ->
  -- | 'esDestinationStreamARN'
  Text ->
  EventStream
eventStream pApplicationId_ pRoleARN_ pDestinationStreamARN_ =
  EventStream'
    { _esLastUpdatedBy = Nothing,
      _esLastModifiedDate = Nothing,
      _esExternalId = Nothing,
      _esApplicationId = pApplicationId_,
      _esRoleARN = pRoleARN_,
      _esDestinationStreamARN = pDestinationStreamARN_
    }

-- | The IAM user who last modified the event stream.
esLastUpdatedBy :: Lens' EventStream (Maybe Text)
esLastUpdatedBy = lens _esLastUpdatedBy (\s a -> s {_esLastUpdatedBy = a})

-- | The date, in ISO 8601 format, when the event stream was last modified.
esLastModifiedDate :: Lens' EventStream (Maybe Text)
esLastModifiedDate = lens _esLastModifiedDate (\s a -> s {_esLastModifiedDate = a})

-- | (Deprecated) Your AWS account ID, which you assigned to an external ID key in an IAM trust policy. Amazon Pinpoint previously used this value to assume an IAM role when publishing event data, but we removed this requirement. We don't recommend use of external IDs for IAM roles that are assumed by Amazon Pinpoint.
esExternalId :: Lens' EventStream (Maybe Text)
esExternalId = lens _esExternalId (\s a -> s {_esExternalId = a})

-- | The unique identifier for the application to publish event data for.
esApplicationId :: Lens' EventStream Text
esApplicationId = lens _esApplicationId (\s a -> s {_esApplicationId = a})

-- | The AWS Identity and Access Management (IAM) role that authorizes Amazon Pinpoint to publish event data to the stream in your AWS account.
esRoleARN :: Lens' EventStream Text
esRoleARN = lens _esRoleARN (\s a -> s {_esRoleARN = a})

-- | The Amazon Resource Name (ARN) of the Amazon Kinesis data stream or Amazon Kinesis Data Firehose delivery stream to publish event data to. For a Kinesis data stream, the ARN format is: arn:aws:kinesis:<replaceable>region:<replaceable>account-id:stream/<replaceable>stream_name For a Kinesis Data Firehose delivery stream, the ARN format is: arn:aws:firehose:<replaceable>region:<replaceable>account-id:deliverystream/<replaceable>stream_name
esDestinationStreamARN :: Lens' EventStream Text
esDestinationStreamARN = lens _esDestinationStreamARN (\s a -> s {_esDestinationStreamARN = a})

instance FromJSON EventStream where
  parseJSON =
    withObject
      "EventStream"
      ( \x ->
          EventStream'
            <$> (x .:? "LastUpdatedBy")
            <*> (x .:? "LastModifiedDate")
            <*> (x .:? "ExternalId")
            <*> (x .: "ApplicationId")
            <*> (x .: "RoleArn")
            <*> (x .: "DestinationStreamArn")
      )

instance Hashable EventStream

instance NFData EventStream
