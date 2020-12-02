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
-- Module      : Network.AWS.DirectoryService.DeregisterEventTopic
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified directory as a publisher to the specified SNS topic.
--
--
module Network.AWS.DirectoryService.DeregisterEventTopic
    (
    -- * Creating a Request
      deregisterEventTopic
    , DeregisterEventTopic
    -- * Request Lenses
    , detDirectoryId
    , detTopicName

    -- * Destructuring the Response
    , deregisterEventTopicResponse
    , DeregisterEventTopicResponse
    -- * Response Lenses
    , derrsResponseStatus
    ) where

import Network.AWS.DirectoryService.Types
import Network.AWS.DirectoryService.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Removes the specified directory as a publisher to the specified SNS topic.
--
--
--
-- /See:/ 'deregisterEventTopic' smart constructor.
data DeregisterEventTopic = DeregisterEventTopic'
  { _detDirectoryId :: !Text
  , _detTopicName   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeregisterEventTopic' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'detDirectoryId' - The Directory ID to remove as a publisher. This directory will no longer send messages to the specified SNS topic.
--
-- * 'detTopicName' - The name of the SNS topic from which to remove the directory as a publisher.
deregisterEventTopic
    :: Text -- ^ 'detDirectoryId'
    -> Text -- ^ 'detTopicName'
    -> DeregisterEventTopic
deregisterEventTopic pDirectoryId_ pTopicName_ =
  DeregisterEventTopic'
    {_detDirectoryId = pDirectoryId_, _detTopicName = pTopicName_}


-- | The Directory ID to remove as a publisher. This directory will no longer send messages to the specified SNS topic.
detDirectoryId :: Lens' DeregisterEventTopic Text
detDirectoryId = lens _detDirectoryId (\ s a -> s{_detDirectoryId = a})

-- | The name of the SNS topic from which to remove the directory as a publisher.
detTopicName :: Lens' DeregisterEventTopic Text
detTopicName = lens _detTopicName (\ s a -> s{_detTopicName = a})

instance AWSRequest DeregisterEventTopic where
        type Rs DeregisterEventTopic =
             DeregisterEventTopicResponse
        request = postJSON directoryService
        response
          = receiveEmpty
              (\ s h x ->
                 DeregisterEventTopicResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeregisterEventTopic where

instance NFData DeregisterEventTopic where

instance ToHeaders DeregisterEventTopic where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.DeregisterEventTopic" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeregisterEventTopic where
        toJSON DeregisterEventTopic'{..}
          = object
              (catMaybes
                 [Just ("DirectoryId" .= _detDirectoryId),
                  Just ("TopicName" .= _detTopicName)])

instance ToPath DeregisterEventTopic where
        toPath = const "/"

instance ToQuery DeregisterEventTopic where
        toQuery = const mempty

-- | The result of a DeregisterEventTopic request.
--
--
--
-- /See:/ 'deregisterEventTopicResponse' smart constructor.
newtype DeregisterEventTopicResponse = DeregisterEventTopicResponse'
  { _derrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeregisterEventTopicResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'derrsResponseStatus' - -- | The response status code.
deregisterEventTopicResponse
    :: Int -- ^ 'derrsResponseStatus'
    -> DeregisterEventTopicResponse
deregisterEventTopicResponse pResponseStatus_ =
  DeregisterEventTopicResponse' {_derrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
derrsResponseStatus :: Lens' DeregisterEventTopicResponse Int
derrsResponseStatus = lens _derrsResponseStatus (\ s a -> s{_derrsResponseStatus = a})

instance NFData DeregisterEventTopicResponse where
