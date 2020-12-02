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
-- Module      : Network.AWS.CloudWatchLogs.TagLogGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates the specified tags for the specified log group.
--
--
-- To list the tags for a log group, use 'ListTagsLogGroup' . To remove tags, use 'UntagLogGroup' .
--
-- For more information about tags, see <http://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/log-group-tagging.html Tag Log Groups in Amazon CloudWatch Logs> in the /Amazon CloudWatch Logs User Guide/ .
--
module Network.AWS.CloudWatchLogs.TagLogGroup
    (
    -- * Creating a Request
      tagLogGroup
    , TagLogGroup
    -- * Request Lenses
    , tlgLogGroupName
    , tlgTags

    -- * Destructuring the Response
    , tagLogGroupResponse
    , TagLogGroupResponse
    ) where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.CloudWatchLogs.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'tagLogGroup' smart constructor.
data TagLogGroup = TagLogGroup'
  { _tlgLogGroupName :: !Text
  , _tlgTags         :: !(Map Text Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagLogGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tlgLogGroupName' - The name of the log group.
--
-- * 'tlgTags' - The key-value pairs to use for the tags.
tagLogGroup
    :: Text -- ^ 'tlgLogGroupName'
    -> TagLogGroup
tagLogGroup pLogGroupName_ =
  TagLogGroup' {_tlgLogGroupName = pLogGroupName_, _tlgTags = mempty}


-- | The name of the log group.
tlgLogGroupName :: Lens' TagLogGroup Text
tlgLogGroupName = lens _tlgLogGroupName (\ s a -> s{_tlgLogGroupName = a})

-- | The key-value pairs to use for the tags.
tlgTags :: Lens' TagLogGroup (HashMap Text Text)
tlgTags = lens _tlgTags (\ s a -> s{_tlgTags = a}) . _Map

instance AWSRequest TagLogGroup where
        type Rs TagLogGroup = TagLogGroupResponse
        request = postJSON cloudWatchLogs
        response = receiveNull TagLogGroupResponse'

instance Hashable TagLogGroup where

instance NFData TagLogGroup where

instance ToHeaders TagLogGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.TagLogGroup" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON TagLogGroup where
        toJSON TagLogGroup'{..}
          = object
              (catMaybes
                 [Just ("logGroupName" .= _tlgLogGroupName),
                  Just ("tags" .= _tlgTags)])

instance ToPath TagLogGroup where
        toPath = const "/"

instance ToQuery TagLogGroup where
        toQuery = const mempty

-- | /See:/ 'tagLogGroupResponse' smart constructor.
data TagLogGroupResponse =
  TagLogGroupResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagLogGroupResponse' with the minimum fields required to make a request.
--
tagLogGroupResponse
    :: TagLogGroupResponse
tagLogGroupResponse = TagLogGroupResponse'


instance NFData TagLogGroupResponse where
