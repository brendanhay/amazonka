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
-- Module      : Network.AWS.CloudTrail.RemoveTags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified tags from a trail.
--
--
module Network.AWS.CloudTrail.RemoveTags
    (
    -- * Creating a Request
      removeTags
    , RemoveTags
    -- * Request Lenses
    , rtTagsList
    , rtResourceId

    -- * Destructuring the Response
    , removeTagsResponse
    , RemoveTagsResponse
    -- * Response Lenses
    , rtrsResponseStatus
    ) where

import Network.AWS.CloudTrail.Types
import Network.AWS.CloudTrail.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Specifies the tags to remove from a trail.
--
--
--
-- /See:/ 'removeTags' smart constructor.
data RemoveTags = RemoveTags'
  { _rtTagsList   :: !(Maybe [Tag])
  , _rtResourceId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtTagsList' - Specifies a list of tags to be removed.
--
-- * 'rtResourceId' - Specifies the ARN of the trail from which tags should be removed. The format of a trail ARN is: @arn:aws:cloudtrail:us-east-1:123456789012:trail/MyTrail@
removeTags
    :: Text -- ^ 'rtResourceId'
    -> RemoveTags
removeTags pResourceId_ =
  RemoveTags' {_rtTagsList = Nothing, _rtResourceId = pResourceId_}


-- | Specifies a list of tags to be removed.
rtTagsList :: Lens' RemoveTags [Tag]
rtTagsList = lens _rtTagsList (\ s a -> s{_rtTagsList = a}) . _Default . _Coerce

-- | Specifies the ARN of the trail from which tags should be removed. The format of a trail ARN is: @arn:aws:cloudtrail:us-east-1:123456789012:trail/MyTrail@
rtResourceId :: Lens' RemoveTags Text
rtResourceId = lens _rtResourceId (\ s a -> s{_rtResourceId = a})

instance AWSRequest RemoveTags where
        type Rs RemoveTags = RemoveTagsResponse
        request = postJSON cloudTrail
        response
          = receiveEmpty
              (\ s h x ->
                 RemoveTagsResponse' <$> (pure (fromEnum s)))

instance Hashable RemoveTags where

instance NFData RemoveTags where

instance ToHeaders RemoveTags where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.RemoveTags"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RemoveTags where
        toJSON RemoveTags'{..}
          = object
              (catMaybes
                 [("TagsList" .=) <$> _rtTagsList,
                  Just ("ResourceId" .= _rtResourceId)])

instance ToPath RemoveTags where
        toPath = const "/"

instance ToQuery RemoveTags where
        toQuery = const mempty

-- | Returns the objects or data listed below if successful. Otherwise, returns an error.
--
--
--
-- /See:/ 'removeTagsResponse' smart constructor.
newtype RemoveTagsResponse = RemoveTagsResponse'
  { _rtrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveTagsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtrsResponseStatus' - -- | The response status code.
removeTagsResponse
    :: Int -- ^ 'rtrsResponseStatus'
    -> RemoveTagsResponse
removeTagsResponse pResponseStatus_ =
  RemoveTagsResponse' {_rtrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
rtrsResponseStatus :: Lens' RemoveTagsResponse Int
rtrsResponseStatus = lens _rtrsResponseStatus (\ s a -> s{_rtrsResponseStatus = a})

instance NFData RemoveTagsResponse where
