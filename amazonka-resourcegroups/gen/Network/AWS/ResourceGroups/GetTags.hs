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
-- Module      : Network.AWS.ResourceGroups.GetTags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of tags that are associated with a resource, specified by an ARN.
--
--
module Network.AWS.ResourceGroups.GetTags
    (
    -- * Creating a Request
      getTags
    , GetTags
    -- * Request Lenses
    , gtARN

    -- * Destructuring the Response
    , getTagsResponse
    , GetTagsResponse
    -- * Response Lenses
    , gtrsARN
    , gtrsTags
    , gtrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.ResourceGroups.Types
import Network.AWS.ResourceGroups.Types.Product
import Network.AWS.Response

-- | /See:/ 'getTags' smart constructor.
newtype GetTags = GetTags'
  { _gtARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtARN' - The ARN of the resource for which you want a list of tags. The resource must exist within the account you are using.
getTags
    :: Text -- ^ 'gtARN'
    -> GetTags
getTags pARN_ = GetTags' {_gtARN = pARN_}


-- | The ARN of the resource for which you want a list of tags. The resource must exist within the account you are using.
gtARN :: Lens' GetTags Text
gtARN = lens _gtARN (\ s a -> s{_gtARN = a})

instance AWSRequest GetTags where
        type Rs GetTags = GetTagsResponse
        request = get resourceGroups
        response
          = receiveJSON
              (\ s h x ->
                 GetTagsResponse' <$>
                   (x .?> "Arn") <*> (x .?> "Tags" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable GetTags where

instance NFData GetTags where

instance ToHeaders GetTags where
        toHeaders = const mempty

instance ToPath GetTags where
        toPath GetTags'{..}
          = mconcat ["/resources/", toBS _gtARN, "/tags"]

instance ToQuery GetTags where
        toQuery = const mempty

-- | /See:/ 'getTagsResponse' smart constructor.
data GetTagsResponse = GetTagsResponse'
  { _gtrsARN            :: !(Maybe Text)
  , _gtrsTags           :: !(Maybe (Map Text Text))
  , _gtrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTagsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtrsARN' - The ARN of the tagged resource.
--
-- * 'gtrsTags' - The tags associated with the specified resource.
--
-- * 'gtrsResponseStatus' - -- | The response status code.
getTagsResponse
    :: Int -- ^ 'gtrsResponseStatus'
    -> GetTagsResponse
getTagsResponse pResponseStatus_ =
  GetTagsResponse'
    { _gtrsARN = Nothing
    , _gtrsTags = Nothing
    , _gtrsResponseStatus = pResponseStatus_
    }


-- | The ARN of the tagged resource.
gtrsARN :: Lens' GetTagsResponse (Maybe Text)
gtrsARN = lens _gtrsARN (\ s a -> s{_gtrsARN = a})

-- | The tags associated with the specified resource.
gtrsTags :: Lens' GetTagsResponse (HashMap Text Text)
gtrsTags = lens _gtrsTags (\ s a -> s{_gtrsTags = a}) . _Default . _Map

-- | -- | The response status code.
gtrsResponseStatus :: Lens' GetTagsResponse Int
gtrsResponseStatus = lens _gtrsResponseStatus (\ s a -> s{_gtrsResponseStatus = a})

instance NFData GetTagsResponse where
