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
-- Module      : Network.AWS.DirectoryService.DescribeEventTopics
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Obtains information about which SNS topics receive status messages from the specified directory.
--
--
-- If no input parameters are provided, such as DirectoryId or TopicName, this request describes all of the associations in the account.
--
module Network.AWS.DirectoryService.DescribeEventTopics
    (
    -- * Creating a Request
      describeEventTopics
    , DescribeEventTopics
    -- * Request Lenses
    , dDirectoryId
    , dTopicNames

    -- * Destructuring the Response
    , describeEventTopicsResponse
    , DescribeEventTopicsResponse
    -- * Response Lenses
    , detrsEventTopics
    , detrsResponseStatus
    ) where

import Network.AWS.DirectoryService.Types
import Network.AWS.DirectoryService.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Describes event topics.
--
--
--
-- /See:/ 'describeEventTopics' smart constructor.
data DescribeEventTopics = DescribeEventTopics'
  { _dDirectoryId :: !(Maybe Text)
  , _dTopicNames  :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEventTopics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dDirectoryId' - The Directory ID for which to get the list of associated SNS topics. If this member is null, associations for all Directory IDs are returned.
--
-- * 'dTopicNames' - A list of SNS topic names for which to obtain the information. If this member is null, all associations for the specified Directory ID are returned. An empty list results in an @InvalidParameterException@ being thrown.
describeEventTopics
    :: DescribeEventTopics
describeEventTopics =
  DescribeEventTopics' {_dDirectoryId = Nothing, _dTopicNames = Nothing}


-- | The Directory ID for which to get the list of associated SNS topics. If this member is null, associations for all Directory IDs are returned.
dDirectoryId :: Lens' DescribeEventTopics (Maybe Text)
dDirectoryId = lens _dDirectoryId (\ s a -> s{_dDirectoryId = a})

-- | A list of SNS topic names for which to obtain the information. If this member is null, all associations for the specified Directory ID are returned. An empty list results in an @InvalidParameterException@ being thrown.
dTopicNames :: Lens' DescribeEventTopics [Text]
dTopicNames = lens _dTopicNames (\ s a -> s{_dTopicNames = a}) . _Default . _Coerce

instance AWSRequest DescribeEventTopics where
        type Rs DescribeEventTopics =
             DescribeEventTopicsResponse
        request = postJSON directoryService
        response
          = receiveJSON
              (\ s h x ->
                 DescribeEventTopicsResponse' <$>
                   (x .?> "EventTopics" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DescribeEventTopics where

instance NFData DescribeEventTopics where

instance ToHeaders DescribeEventTopics where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.DescribeEventTopics" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeEventTopics where
        toJSON DescribeEventTopics'{..}
          = object
              (catMaybes
                 [("DirectoryId" .=) <$> _dDirectoryId,
                  ("TopicNames" .=) <$> _dTopicNames])

instance ToPath DescribeEventTopics where
        toPath = const "/"

instance ToQuery DescribeEventTopics where
        toQuery = const mempty

-- | The result of a DescribeEventTopic request.
--
--
--
-- /See:/ 'describeEventTopicsResponse' smart constructor.
data DescribeEventTopicsResponse = DescribeEventTopicsResponse'
  { _detrsEventTopics    :: !(Maybe [EventTopic])
  , _detrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEventTopicsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'detrsEventTopics' - A list of SNS topic names that receive status messages from the specified Directory ID.
--
-- * 'detrsResponseStatus' - -- | The response status code.
describeEventTopicsResponse
    :: Int -- ^ 'detrsResponseStatus'
    -> DescribeEventTopicsResponse
describeEventTopicsResponse pResponseStatus_ =
  DescribeEventTopicsResponse'
    {_detrsEventTopics = Nothing, _detrsResponseStatus = pResponseStatus_}


-- | A list of SNS topic names that receive status messages from the specified Directory ID.
detrsEventTopics :: Lens' DescribeEventTopicsResponse [EventTopic]
detrsEventTopics = lens _detrsEventTopics (\ s a -> s{_detrsEventTopics = a}) . _Default . _Coerce

-- | -- | The response status code.
detrsResponseStatus :: Lens' DescribeEventTopicsResponse Int
detrsResponseStatus = lens _detrsResponseStatus (\ s a -> s{_detrsResponseStatus = a})

instance NFData DescribeEventTopicsResponse where
