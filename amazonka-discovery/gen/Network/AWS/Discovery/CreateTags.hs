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
-- Module      : Network.AWS.Discovery.CreateTags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates one or more tags for configuration items. Tags are metadata that help you categorize IT assets. This API accepts a list of multiple configuration items.
--
--
module Network.AWS.Discovery.CreateTags
    (
    -- * Creating a Request
      createTags
    , CreateTags
    -- * Request Lenses
    , ctConfigurationIds
    , ctTags

    -- * Destructuring the Response
    , createTagsResponse
    , CreateTagsResponse
    -- * Response Lenses
    , ctrsResponseStatus
    ) where

import Network.AWS.Discovery.Types
import Network.AWS.Discovery.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createTags' smart constructor.
data CreateTags = CreateTags'
  { _ctConfigurationIds :: ![Text]
  , _ctTags             :: ![Tag]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctConfigurationIds' - A list of configuration items that you want to tag.
--
-- * 'ctTags' - Tags that you want to associate with one or more configuration items. Specify the tags that you want to create in a /key/ -/value/ format. For example: @{"key": "serverType", "value": "webServer"}@
createTags
    :: CreateTags
createTags = CreateTags' {_ctConfigurationIds = mempty, _ctTags = mempty}


-- | A list of configuration items that you want to tag.
ctConfigurationIds :: Lens' CreateTags [Text]
ctConfigurationIds = lens _ctConfigurationIds (\ s a -> s{_ctConfigurationIds = a}) . _Coerce

-- | Tags that you want to associate with one or more configuration items. Specify the tags that you want to create in a /key/ -/value/ format. For example: @{"key": "serverType", "value": "webServer"}@
ctTags :: Lens' CreateTags [Tag]
ctTags = lens _ctTags (\ s a -> s{_ctTags = a}) . _Coerce

instance AWSRequest CreateTags where
        type Rs CreateTags = CreateTagsResponse
        request = postJSON discovery
        response
          = receiveEmpty
              (\ s h x ->
                 CreateTagsResponse' <$> (pure (fromEnum s)))

instance Hashable CreateTags where

instance NFData CreateTags where

instance ToHeaders CreateTags where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSPoseidonService_V2015_11_01.CreateTags" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateTags where
        toJSON CreateTags'{..}
          = object
              (catMaybes
                 [Just ("configurationIds" .= _ctConfigurationIds),
                  Just ("tags" .= _ctTags)])

instance ToPath CreateTags where
        toPath = const "/"

instance ToQuery CreateTags where
        toQuery = const mempty

-- | /See:/ 'createTagsResponse' smart constructor.
newtype CreateTagsResponse = CreateTagsResponse'
  { _ctrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTagsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctrsResponseStatus' - -- | The response status code.
createTagsResponse
    :: Int -- ^ 'ctrsResponseStatus'
    -> CreateTagsResponse
createTagsResponse pResponseStatus_ =
  CreateTagsResponse' {_ctrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ctrsResponseStatus :: Lens' CreateTagsResponse Int
ctrsResponseStatus = lens _ctrsResponseStatus (\ s a -> s{_ctrsResponseStatus = a})

instance NFData CreateTagsResponse where
