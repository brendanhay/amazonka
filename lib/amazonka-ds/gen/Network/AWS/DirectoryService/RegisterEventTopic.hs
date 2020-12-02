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
-- Module      : Network.AWS.DirectoryService.RegisterEventTopic
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a directory with an SNS topic. This establishes the directory as a publisher to the specified SNS topic. You can then receive email or text (SMS) messages when the status of your directory changes. You get notified if your directory goes from an Active status to an Impaired or Inoperable status. You also receive a notification when the directory returns to an Active status.
--
--
module Network.AWS.DirectoryService.RegisterEventTopic
    (
    -- * Creating a Request
      registerEventTopic
    , RegisterEventTopic
    -- * Request Lenses
    , retDirectoryId
    , retTopicName

    -- * Destructuring the Response
    , registerEventTopicResponse
    , RegisterEventTopicResponse
    -- * Response Lenses
    , retrsResponseStatus
    ) where

import Network.AWS.DirectoryService.Types
import Network.AWS.DirectoryService.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Registers a new event topic.
--
--
--
-- /See:/ 'registerEventTopic' smart constructor.
data RegisterEventTopic = RegisterEventTopic'
  { _retDirectoryId :: !Text
  , _retTopicName   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterEventTopic' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'retDirectoryId' - The Directory ID that will publish status messages to the SNS topic.
--
-- * 'retTopicName' - The SNS topic name to which the directory will publish status messages. This SNS topic must be in the same region as the specified Directory ID.
registerEventTopic
    :: Text -- ^ 'retDirectoryId'
    -> Text -- ^ 'retTopicName'
    -> RegisterEventTopic
registerEventTopic pDirectoryId_ pTopicName_ =
  RegisterEventTopic'
    {_retDirectoryId = pDirectoryId_, _retTopicName = pTopicName_}


-- | The Directory ID that will publish status messages to the SNS topic.
retDirectoryId :: Lens' RegisterEventTopic Text
retDirectoryId = lens _retDirectoryId (\ s a -> s{_retDirectoryId = a})

-- | The SNS topic name to which the directory will publish status messages. This SNS topic must be in the same region as the specified Directory ID.
retTopicName :: Lens' RegisterEventTopic Text
retTopicName = lens _retTopicName (\ s a -> s{_retTopicName = a})

instance AWSRequest RegisterEventTopic where
        type Rs RegisterEventTopic =
             RegisterEventTopicResponse
        request = postJSON directoryService
        response
          = receiveEmpty
              (\ s h x ->
                 RegisterEventTopicResponse' <$> (pure (fromEnum s)))

instance Hashable RegisterEventTopic where

instance NFData RegisterEventTopic where

instance ToHeaders RegisterEventTopic where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.RegisterEventTopic" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RegisterEventTopic where
        toJSON RegisterEventTopic'{..}
          = object
              (catMaybes
                 [Just ("DirectoryId" .= _retDirectoryId),
                  Just ("TopicName" .= _retTopicName)])

instance ToPath RegisterEventTopic where
        toPath = const "/"

instance ToQuery RegisterEventTopic where
        toQuery = const mempty

-- | The result of a RegisterEventTopic request.
--
--
--
-- /See:/ 'registerEventTopicResponse' smart constructor.
newtype RegisterEventTopicResponse = RegisterEventTopicResponse'
  { _retrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterEventTopicResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'retrsResponseStatus' - -- | The response status code.
registerEventTopicResponse
    :: Int -- ^ 'retrsResponseStatus'
    -> RegisterEventTopicResponse
registerEventTopicResponse pResponseStatus_ =
  RegisterEventTopicResponse' {_retrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
retrsResponseStatus :: Lens' RegisterEventTopicResponse Int
retrsResponseStatus = lens _retrsResponseStatus (\ s a -> s{_retrsResponseStatus = a})

instance NFData RegisterEventTopicResponse where
