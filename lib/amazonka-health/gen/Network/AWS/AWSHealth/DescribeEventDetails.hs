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
-- Module      : Network.AWS.AWSHealth.DescribeEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed information about one or more specified events. Information includes standard event data (Region, service, and so on, as returned by <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEvents.html DescribeEvents> ), a detailed event description, and possible additional metadata that depends upon the nature of the event. Affected entities are not included. To retrieve those, use the <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeAffectedEntities.html DescribeAffectedEntities> operation.
--
--
-- If a specified event cannot be retrieved, an error message is returned for that event.
module Network.AWS.AWSHealth.DescribeEventDetails
  ( -- * Creating a Request
    describeEventDetails,
    DescribeEventDetails,

    -- * Request Lenses
    dedLocale,
    dedEventARNs,

    -- * Destructuring the Response
    describeEventDetailsResponse,
    DescribeEventDetailsResponse,

    -- * Response Lenses
    dedrsSuccessfulSet,
    dedrsFailedSet,
    dedrsResponseStatus,
  )
where

import Network.AWS.AWSHealth.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeEventDetails' smart constructor.
data DescribeEventDetails = DescribeEventDetails'
  { _dedLocale ::
      !(Maybe Text),
    _dedEventARNs :: !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dedLocale' - The locale (language) to return information in. English (en) is the default and the only supported value at this time.
--
-- * 'dedEventARNs' - A list of event ARNs (unique identifiers). For example: @"arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456", "arn:aws:health:us-west-1::event/EBS/AWS_EBS_LOST_VOLUME/AWS_EBS_LOST_VOLUME_CHI789_JKL101"@
describeEventDetails ::
  -- | 'dedEventARNs'
  NonEmpty Text ->
  DescribeEventDetails
describeEventDetails pEventARNs_ =
  DescribeEventDetails'
    { _dedLocale = Nothing,
      _dedEventARNs = _List1 # pEventARNs_
    }

-- | The locale (language) to return information in. English (en) is the default and the only supported value at this time.
dedLocale :: Lens' DescribeEventDetails (Maybe Text)
dedLocale = lens _dedLocale (\s a -> s {_dedLocale = a})

-- | A list of event ARNs (unique identifiers). For example: @"arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456", "arn:aws:health:us-west-1::event/EBS/AWS_EBS_LOST_VOLUME/AWS_EBS_LOST_VOLUME_CHI789_JKL101"@
dedEventARNs :: Lens' DescribeEventDetails (NonEmpty Text)
dedEventARNs = lens _dedEventARNs (\s a -> s {_dedEventARNs = a}) . _List1

instance AWSRequest DescribeEventDetails where
  type Rs DescribeEventDetails = DescribeEventDetailsResponse
  request = postJSON awsHealth
  response =
    receiveJSON
      ( \s h x ->
          DescribeEventDetailsResponse'
            <$> (x .?> "successfulSet" .!@ mempty)
            <*> (x .?> "failedSet" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeEventDetails

instance NFData DescribeEventDetails

instance ToHeaders DescribeEventDetails where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSHealth_20160804.DescribeEventDetails" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeEventDetails where
  toJSON DescribeEventDetails' {..} =
    object
      ( catMaybes
          [ ("locale" .=) <$> _dedLocale,
            Just ("eventArns" .= _dedEventARNs)
          ]
      )

instance ToPath DescribeEventDetails where
  toPath = const "/"

instance ToQuery DescribeEventDetails where
  toQuery = const mempty

-- | /See:/ 'describeEventDetailsResponse' smart constructor.
data DescribeEventDetailsResponse = DescribeEventDetailsResponse'
  { _dedrsSuccessfulSet ::
      !(Maybe [EventDetails]),
    _dedrsFailedSet ::
      !(Maybe [EventDetailsErrorItem]),
    _dedrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeEventDetailsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dedrsSuccessfulSet' - Information about the events that could be retrieved.
--
-- * 'dedrsFailedSet' - Error messages for any events that could not be retrieved.
--
-- * 'dedrsResponseStatus' - -- | The response status code.
describeEventDetailsResponse ::
  -- | 'dedrsResponseStatus'
  Int ->
  DescribeEventDetailsResponse
describeEventDetailsResponse pResponseStatus_ =
  DescribeEventDetailsResponse'
    { _dedrsSuccessfulSet = Nothing,
      _dedrsFailedSet = Nothing,
      _dedrsResponseStatus = pResponseStatus_
    }

-- | Information about the events that could be retrieved.
dedrsSuccessfulSet :: Lens' DescribeEventDetailsResponse [EventDetails]
dedrsSuccessfulSet = lens _dedrsSuccessfulSet (\s a -> s {_dedrsSuccessfulSet = a}) . _Default . _Coerce

-- | Error messages for any events that could not be retrieved.
dedrsFailedSet :: Lens' DescribeEventDetailsResponse [EventDetailsErrorItem]
dedrsFailedSet = lens _dedrsFailedSet (\s a -> s {_dedrsFailedSet = a}) . _Default . _Coerce

-- | -- | The response status code.
dedrsResponseStatus :: Lens' DescribeEventDetailsResponse Int
dedrsResponseStatus = lens _dedrsResponseStatus (\s a -> s {_dedrsResponseStatus = a})

instance NFData DescribeEventDetailsResponse
