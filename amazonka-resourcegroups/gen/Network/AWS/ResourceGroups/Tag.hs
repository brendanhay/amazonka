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
-- Module      : Network.AWS.ResourceGroups.Tag
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds specified tags to a resource with the specified ARN. Existing tags on a resource are not changed if they are not specified in the request parameters.
--
--
module Network.AWS.ResourceGroups.Tag
    (
    -- * Creating a Request
      tag
    , Tag
    -- * Request Lenses
    , tagARN
    , tagTags

    -- * Destructuring the Response
    , tagResponse
    , TagResponse
    -- * Response Lenses
    , tagrsARN
    , tagrsTags
    , tagrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.ResourceGroups.Types
import Network.AWS.ResourceGroups.Types.Product
import Network.AWS.Response

-- | /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagARN  :: !Text
  , _tagTags :: !(Map Text Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagARN' - The ARN of the resource to which to add tags.
--
-- * 'tagTags' - The tags to add to the specified resource. A tag is a string-to-string map of key-value pairs. Tag keys can have a maximum character length of 127 characters, and tag values can have a maximum length of 255 characters.
tag
    :: Text -- ^ 'tagARN'
    -> Tag
tag pARN_ = Tag' {_tagARN = pARN_, _tagTags = mempty}


-- | The ARN of the resource to which to add tags.
tagARN :: Lens' Tag Text
tagARN = lens _tagARN (\ s a -> s{_tagARN = a})

-- | The tags to add to the specified resource. A tag is a string-to-string map of key-value pairs. Tag keys can have a maximum character length of 127 characters, and tag values can have a maximum length of 255 characters.
tagTags :: Lens' Tag (HashMap Text Text)
tagTags = lens _tagTags (\ s a -> s{_tagTags = a}) . _Map

instance AWSRequest Tag where
        type Rs Tag = TagResponse
        request = putJSON resourceGroups
        response
          = receiveJSON
              (\ s h x ->
                 TagResponse' <$>
                   (x .?> "Arn") <*> (x .?> "Tags" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable Tag where

instance NFData Tag where

instance ToHeaders Tag where
        toHeaders = const mempty

instance ToJSON Tag where
        toJSON Tag'{..}
          = object (catMaybes [Just ("Tags" .= _tagTags)])

instance ToPath Tag where
        toPath Tag'{..}
          = mconcat ["/resources/", toBS _tagARN, "/tags"]

instance ToQuery Tag where
        toQuery = const mempty

-- | /See:/ 'tagResponse' smart constructor.
data TagResponse = TagResponse'
  { _tagrsARN            :: !(Maybe Text)
  , _tagrsTags           :: !(Maybe (Map Text Text))
  , _tagrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagrsARN' - The ARN of the tagged resource.
--
-- * 'tagrsTags' - The tags that have been added to the specified resource.
--
-- * 'tagrsResponseStatus' - -- | The response status code.
tagResponse
    :: Int -- ^ 'tagrsResponseStatus'
    -> TagResponse
tagResponse pResponseStatus_ =
  TagResponse'
    { _tagrsARN = Nothing
    , _tagrsTags = Nothing
    , _tagrsResponseStatus = pResponseStatus_
    }


-- | The ARN of the tagged resource.
tagrsARN :: Lens' TagResponse (Maybe Text)
tagrsARN = lens _tagrsARN (\ s a -> s{_tagrsARN = a})

-- | The tags that have been added to the specified resource.
tagrsTags :: Lens' TagResponse (HashMap Text Text)
tagrsTags = lens _tagrsTags (\ s a -> s{_tagrsTags = a}) . _Default . _Map

-- | -- | The response status code.
tagrsResponseStatus :: Lens' TagResponse Int
tagrsResponseStatus = lens _tagrsResponseStatus (\ s a -> s{_tagrsResponseStatus = a})

instance NFData TagResponse where
