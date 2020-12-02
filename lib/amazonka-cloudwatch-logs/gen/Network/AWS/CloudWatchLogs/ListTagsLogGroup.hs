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
-- Module      : Network.AWS.CloudWatchLogs.ListTagsLogGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags for the specified log group.
--
--
module Network.AWS.CloudWatchLogs.ListTagsLogGroup
    (
    -- * Creating a Request
      listTagsLogGroup
    , ListTagsLogGroup
    -- * Request Lenses
    , ltlgLogGroupName

    -- * Destructuring the Response
    , listTagsLogGroupResponse
    , ListTagsLogGroupResponse
    -- * Response Lenses
    , ltlgrsTags
    , ltlgrsResponseStatus
    ) where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.CloudWatchLogs.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listTagsLogGroup' smart constructor.
newtype ListTagsLogGroup = ListTagsLogGroup'
  { _ltlgLogGroupName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTagsLogGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltlgLogGroupName' - The name of the log group.
listTagsLogGroup
    :: Text -- ^ 'ltlgLogGroupName'
    -> ListTagsLogGroup
listTagsLogGroup pLogGroupName_ =
  ListTagsLogGroup' {_ltlgLogGroupName = pLogGroupName_}


-- | The name of the log group.
ltlgLogGroupName :: Lens' ListTagsLogGroup Text
ltlgLogGroupName = lens _ltlgLogGroupName (\ s a -> s{_ltlgLogGroupName = a})

instance AWSRequest ListTagsLogGroup where
        type Rs ListTagsLogGroup = ListTagsLogGroupResponse
        request = postJSON cloudWatchLogs
        response
          = receiveJSON
              (\ s h x ->
                 ListTagsLogGroupResponse' <$>
                   (x .?> "tags" .!@ mempty) <*> (pure (fromEnum s)))

instance Hashable ListTagsLogGroup where

instance NFData ListTagsLogGroup where

instance ToHeaders ListTagsLogGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.ListTagsLogGroup" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListTagsLogGroup where
        toJSON ListTagsLogGroup'{..}
          = object
              (catMaybes
                 [Just ("logGroupName" .= _ltlgLogGroupName)])

instance ToPath ListTagsLogGroup where
        toPath = const "/"

instance ToQuery ListTagsLogGroup where
        toQuery = const mempty

-- | /See:/ 'listTagsLogGroupResponse' smart constructor.
data ListTagsLogGroupResponse = ListTagsLogGroupResponse'
  { _ltlgrsTags           :: !(Maybe (Map Text Text))
  , _ltlgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTagsLogGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltlgrsTags' - The tags for the log group.
--
-- * 'ltlgrsResponseStatus' - -- | The response status code.
listTagsLogGroupResponse
    :: Int -- ^ 'ltlgrsResponseStatus'
    -> ListTagsLogGroupResponse
listTagsLogGroupResponse pResponseStatus_ =
  ListTagsLogGroupResponse'
    {_ltlgrsTags = Nothing, _ltlgrsResponseStatus = pResponseStatus_}


-- | The tags for the log group.
ltlgrsTags :: Lens' ListTagsLogGroupResponse (HashMap Text Text)
ltlgrsTags = lens _ltlgrsTags (\ s a -> s{_ltlgrsTags = a}) . _Default . _Map

-- | -- | The response status code.
ltlgrsResponseStatus :: Lens' ListTagsLogGroupResponse Int
ltlgrsResponseStatus = lens _ltlgrsResponseStatus (\ s a -> s{_ltlgrsResponseStatus = a})

instance NFData ListTagsLogGroupResponse where
