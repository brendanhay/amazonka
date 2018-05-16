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
-- Module      : Network.AWS.APIGateway.GetTags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the 'Tags' collection for a given resource.
--
--
module Network.AWS.APIGateway.GetTags
    (
    -- * Creating a Request
      getTags
    , GetTags
    -- * Request Lenses
    , gtLimit
    , gtPosition
    , gtResourceARN

    -- * Destructuring the Response
    , getTagsResponse
    , GetTagsResponse
    -- * Response Lenses
    , gtrsTags
    , gtrsResponseStatus
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Gets the 'Tags' collection for a given resource.
--
--
--
-- /See:/ 'getTags' smart constructor.
data GetTags = GetTags'
  { _gtLimit       :: !(Maybe Int)
  , _gtPosition    :: !(Maybe Text)
  , _gtResourceARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtLimit' - (Not currently supported) The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- * 'gtPosition' - (Not currently supported) The current pagination position in the paged result set.
--
-- * 'gtResourceARN' - [Required] The ARN of a resource that can be tagged. The resource ARN must be URL-encoded. At present, 'Stage' is the only taggable resource.
getTags
    :: Text -- ^ 'gtResourceARN'
    -> GetTags
getTags pResourceARN_ =
  GetTags'
    {_gtLimit = Nothing, _gtPosition = Nothing, _gtResourceARN = pResourceARN_}


-- | (Not currently supported) The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
gtLimit :: Lens' GetTags (Maybe Int)
gtLimit = lens _gtLimit (\ s a -> s{_gtLimit = a})

-- | (Not currently supported) The current pagination position in the paged result set.
gtPosition :: Lens' GetTags (Maybe Text)
gtPosition = lens _gtPosition (\ s a -> s{_gtPosition = a})

-- | [Required] The ARN of a resource that can be tagged. The resource ARN must be URL-encoded. At present, 'Stage' is the only taggable resource.
gtResourceARN :: Lens' GetTags Text
gtResourceARN = lens _gtResourceARN (\ s a -> s{_gtResourceARN = a})

instance AWSRequest GetTags where
        type Rs GetTags = GetTagsResponse
        request = get apiGateway
        response
          = receiveJSON
              (\ s h x ->
                 GetTagsResponse' <$>
                   (x .?> "tags" .!@ mempty) <*> (pure (fromEnum s)))

instance Hashable GetTags where

instance NFData GetTags where

instance ToHeaders GetTags where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetTags where
        toPath GetTags'{..}
          = mconcat ["/tags/", toBS _gtResourceARN]

instance ToQuery GetTags where
        toQuery GetTags'{..}
          = mconcat
              ["limit" =: _gtLimit, "position" =: _gtPosition]

-- | The collection of tags. Each tag element is associated with a given resource.
--
--
--
-- /See:/ 'getTagsResponse' smart constructor.
data GetTagsResponse = GetTagsResponse'
  { _gtrsTags           :: !(Maybe (Map Text Text))
  , _gtrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTagsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtrsTags' - The collection of tags. Each tag element is associated with a given resource.
--
-- * 'gtrsResponseStatus' - -- | The response status code.
getTagsResponse
    :: Int -- ^ 'gtrsResponseStatus'
    -> GetTagsResponse
getTagsResponse pResponseStatus_ =
  GetTagsResponse' {_gtrsTags = Nothing, _gtrsResponseStatus = pResponseStatus_}


-- | The collection of tags. Each tag element is associated with a given resource.
gtrsTags :: Lens' GetTagsResponse (HashMap Text Text)
gtrsTags = lens _gtrsTags (\ s a -> s{_gtrsTags = a}) . _Default . _Map

-- | -- | The response status code.
gtrsResponseStatus :: Lens' GetTagsResponse Int
gtrsResponseStatus = lens _gtrsResponseStatus (\ s a -> s{_gtrsResponseStatus = a})

instance NFData GetTagsResponse where
