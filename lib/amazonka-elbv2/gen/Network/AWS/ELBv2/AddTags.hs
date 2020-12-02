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
-- Module      : Network.AWS.ELBv2.AddTags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified tags to the specified Elastic Load Balancing resource. You can tag your Application Load Balancers, Network Load Balancers, and your target groups.
--
--
-- Each tag consists of a key and an optional value. If a resource already has a tag with the same key, @AddTags@ updates its value.
--
-- To list the current tags for your resources, use 'DescribeTags' . To remove tags from your resources, use 'RemoveTags' .
--
module Network.AWS.ELBv2.AddTags
    (
    -- * Creating a Request
      addTags
    , AddTags
    -- * Request Lenses
    , atResourceARNs
    , atTags

    -- * Destructuring the Response
    , addTagsResponse
    , AddTagsResponse
    -- * Response Lenses
    , atrsResponseStatus
    ) where

import Network.AWS.ELBv2.Types
import Network.AWS.ELBv2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'addTags' smart constructor.
data AddTags = AddTags'
  { _atResourceARNs :: ![Text]
  , _atTags         :: !(List1 Tag)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atResourceARNs' - The Amazon Resource Name (ARN) of the resource.
--
-- * 'atTags' - The tags. Each resource can have a maximum of 10 tags.
addTags
    :: NonEmpty Tag -- ^ 'atTags'
    -> AddTags
addTags pTags_ = AddTags' {_atResourceARNs = mempty, _atTags = _List1 # pTags_}


-- | The Amazon Resource Name (ARN) of the resource.
atResourceARNs :: Lens' AddTags [Text]
atResourceARNs = lens _atResourceARNs (\ s a -> s{_atResourceARNs = a}) . _Coerce

-- | The tags. Each resource can have a maximum of 10 tags.
atTags :: Lens' AddTags (NonEmpty Tag)
atTags = lens _atTags (\ s a -> s{_atTags = a}) . _List1

instance AWSRequest AddTags where
        type Rs AddTags = AddTagsResponse
        request = postQuery eLBv2
        response
          = receiveXMLWrapper "AddTagsResult"
              (\ s h x -> AddTagsResponse' <$> (pure (fromEnum s)))

instance Hashable AddTags where

instance NFData AddTags where

instance ToHeaders AddTags where
        toHeaders = const mempty

instance ToPath AddTags where
        toPath = const "/"

instance ToQuery AddTags where
        toQuery AddTags'{..}
          = mconcat
              ["Action" =: ("AddTags" :: ByteString),
               "Version" =: ("2015-12-01" :: ByteString),
               "ResourceArns" =:
                 toQueryList "member" _atResourceARNs,
               "Tags" =: toQueryList "member" _atTags]

-- | /See:/ 'addTagsResponse' smart constructor.
newtype AddTagsResponse = AddTagsResponse'
  { _atrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddTagsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atrsResponseStatus' - -- | The response status code.
addTagsResponse
    :: Int -- ^ 'atrsResponseStatus'
    -> AddTagsResponse
addTagsResponse pResponseStatus_ =
  AddTagsResponse' {_atrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
atrsResponseStatus :: Lens' AddTagsResponse Int
atrsResponseStatus = lens _atrsResponseStatus (\ s a -> s{_atrsResponseStatus = a})

instance NFData AddTagsResponse where
