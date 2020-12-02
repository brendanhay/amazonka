{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.DescribeEventSource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation lists details about a partner event source that is shared with your account.
--
--
module Network.AWS.CloudWatchEvents.DescribeEventSource
    (
    -- * Creating a Request
      describeEventSource
    , DescribeEventSource
    -- * Request Lenses
    , deseName

    -- * Destructuring the Response
    , describeEventSourceResponse
    , DescribeEventSourceResponse
    -- * Response Lenses
    , desrsCreationTime
    , desrsState
    , desrsARN
    , desrsCreatedBy
    , desrsName
    , desrsExpirationTime
    , desrsResponseStatus
    ) where

import Network.AWS.CloudWatchEvents.Types
import Network.AWS.CloudWatchEvents.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeEventSource' smart constructor.
newtype DescribeEventSource = DescribeEventSource'
  { _deseName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEventSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deseName' - The name of the partner event source to display the details of.
describeEventSource
    :: Text -- ^ 'deseName'
    -> DescribeEventSource
describeEventSource pName_ = DescribeEventSource' {_deseName = pName_}


-- | The name of the partner event source to display the details of.
deseName :: Lens' DescribeEventSource Text
deseName = lens _deseName (\ s a -> s{_deseName = a})

instance AWSRequest DescribeEventSource where
        type Rs DescribeEventSource =
             DescribeEventSourceResponse
        request = postJSON cloudWatchEvents
        response
          = receiveJSON
              (\ s h x ->
                 DescribeEventSourceResponse' <$>
                   (x .?> "CreationTime") <*> (x .?> "State") <*>
                     (x .?> "Arn")
                     <*> (x .?> "CreatedBy")
                     <*> (x .?> "Name")
                     <*> (x .?> "ExpirationTime")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeEventSource where

instance NFData DescribeEventSource where

instance ToHeaders DescribeEventSource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSEvents.DescribeEventSource" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeEventSource where
        toJSON DescribeEventSource'{..}
          = object (catMaybes [Just ("Name" .= _deseName)])

instance ToPath DescribeEventSource where
        toPath = const "/"

instance ToQuery DescribeEventSource where
        toQuery = const mempty

-- | /See:/ 'describeEventSourceResponse' smart constructor.
data DescribeEventSourceResponse = DescribeEventSourceResponse'
  { _desrsCreationTime   :: !(Maybe POSIX)
  , _desrsState          :: !(Maybe EventSourceState)
  , _desrsARN            :: !(Maybe Text)
  , _desrsCreatedBy      :: !(Maybe Text)
  , _desrsName           :: !(Maybe Text)
  , _desrsExpirationTime :: !(Maybe POSIX)
  , _desrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEventSourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desrsCreationTime' - The date and time that the event source was created.
--
-- * 'desrsState' - The state of the event source. If it's @ACTIVE@ , you have already created a matching event bus for this event source, and that event bus is active. If it's @PENDING@ , either you haven't yet created a matching event bus, or that event bus is deactivated. If it's @DELETED@ , you have created a matching event bus, but the event source has since been deleted.
--
-- * 'desrsARN' - The ARN of the partner event source.
--
-- * 'desrsCreatedBy' - The name of the SaaS partner that created the event source.
--
-- * 'desrsName' - The name of the partner event source.
--
-- * 'desrsExpirationTime' - The date and time that the event source will expire if you don't create a matching event bus.
--
-- * 'desrsResponseStatus' - -- | The response status code.
describeEventSourceResponse
    :: Int -- ^ 'desrsResponseStatus'
    -> DescribeEventSourceResponse
describeEventSourceResponse pResponseStatus_ =
  DescribeEventSourceResponse'
    { _desrsCreationTime = Nothing
    , _desrsState = Nothing
    , _desrsARN = Nothing
    , _desrsCreatedBy = Nothing
    , _desrsName = Nothing
    , _desrsExpirationTime = Nothing
    , _desrsResponseStatus = pResponseStatus_
    }


-- | The date and time that the event source was created.
desrsCreationTime :: Lens' DescribeEventSourceResponse (Maybe UTCTime)
desrsCreationTime = lens _desrsCreationTime (\ s a -> s{_desrsCreationTime = a}) . mapping _Time

-- | The state of the event source. If it's @ACTIVE@ , you have already created a matching event bus for this event source, and that event bus is active. If it's @PENDING@ , either you haven't yet created a matching event bus, or that event bus is deactivated. If it's @DELETED@ , you have created a matching event bus, but the event source has since been deleted.
desrsState :: Lens' DescribeEventSourceResponse (Maybe EventSourceState)
desrsState = lens _desrsState (\ s a -> s{_desrsState = a})

-- | The ARN of the partner event source.
desrsARN :: Lens' DescribeEventSourceResponse (Maybe Text)
desrsARN = lens _desrsARN (\ s a -> s{_desrsARN = a})

-- | The name of the SaaS partner that created the event source.
desrsCreatedBy :: Lens' DescribeEventSourceResponse (Maybe Text)
desrsCreatedBy = lens _desrsCreatedBy (\ s a -> s{_desrsCreatedBy = a})

-- | The name of the partner event source.
desrsName :: Lens' DescribeEventSourceResponse (Maybe Text)
desrsName = lens _desrsName (\ s a -> s{_desrsName = a})

-- | The date and time that the event source will expire if you don't create a matching event bus.
desrsExpirationTime :: Lens' DescribeEventSourceResponse (Maybe UTCTime)
desrsExpirationTime = lens _desrsExpirationTime (\ s a -> s{_desrsExpirationTime = a}) . mapping _Time

-- | -- | The response status code.
desrsResponseStatus :: Lens' DescribeEventSourceResponse Int
desrsResponseStatus = lens _desrsResponseStatus (\ s a -> s{_desrsResponseStatus = a})

instance NFData DescribeEventSourceResponse where
