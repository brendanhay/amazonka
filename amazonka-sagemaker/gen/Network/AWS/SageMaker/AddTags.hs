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
-- Module      : Network.AWS.SageMaker.AddTags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or overwrites one or more tags for the specified Amazon SageMaker resource. You can add tags to notebook instances, training jobs, models, endpoint configurations, and endpoints.
--
--
-- Each tag consists of a key and an optional value. Tag keys must be unique per resource. For more information about tags, see <http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
module Network.AWS.SageMaker.AddTags
    (
    -- * Creating a Request
      addTags
    , AddTags
    -- * Request Lenses
    , atResourceARN
    , atTags

    -- * Destructuring the Response
    , addTagsResponse
    , AddTagsResponse
    -- * Response Lenses
    , atrsTags
    , atrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'addTags' smart constructor.
data AddTags = AddTags'
  { _atResourceARN :: !Text
  , _atTags        :: ![Tag]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atResourceARN' - The Amazon Resource Name (ARN) of the resource that you want to tag.
--
-- * 'atTags' - An array of @Tag@ objects. Each tag is a key-value pair. Only the @key@ parameter is required. If you don't specify a value, Amazon SageMaker sets the value to an empty string.
addTags
    :: Text -- ^ 'atResourceARN'
    -> AddTags
addTags pResourceARN_ =
  AddTags' {_atResourceARN = pResourceARN_, _atTags = mempty}


-- | The Amazon Resource Name (ARN) of the resource that you want to tag.
atResourceARN :: Lens' AddTags Text
atResourceARN = lens _atResourceARN (\ s a -> s{_atResourceARN = a})

-- | An array of @Tag@ objects. Each tag is a key-value pair. Only the @key@ parameter is required. If you don't specify a value, Amazon SageMaker sets the value to an empty string.
atTags :: Lens' AddTags [Tag]
atTags = lens _atTags (\ s a -> s{_atTags = a}) . _Coerce

instance AWSRequest AddTags where
        type Rs AddTags = AddTagsResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 AddTagsResponse' <$>
                   (x .?> "Tags" .!@ mempty) <*> (pure (fromEnum s)))

instance Hashable AddTags where

instance NFData AddTags where

instance ToHeaders AddTags where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.AddTags" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AddTags where
        toJSON AddTags'{..}
          = object
              (catMaybes
                 [Just ("ResourceArn" .= _atResourceARN),
                  Just ("Tags" .= _atTags)])

instance ToPath AddTags where
        toPath = const "/"

instance ToQuery AddTags where
        toQuery = const mempty

-- | /See:/ 'addTagsResponse' smart constructor.
data AddTagsResponse = AddTagsResponse'
  { _atrsTags           :: !(Maybe [Tag])
  , _atrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddTagsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atrsTags' - A list of tags associated with the Amazon SageMaker resource.
--
-- * 'atrsResponseStatus' - -- | The response status code.
addTagsResponse
    :: Int -- ^ 'atrsResponseStatus'
    -> AddTagsResponse
addTagsResponse pResponseStatus_ =
  AddTagsResponse' {_atrsTags = Nothing, _atrsResponseStatus = pResponseStatus_}


-- | A list of tags associated with the Amazon SageMaker resource.
atrsTags :: Lens' AddTagsResponse [Tag]
atrsTags = lens _atrsTags (\ s a -> s{_atrsTags = a}) . _Default . _Coerce

-- | -- | The response status code.
atrsResponseStatus :: Lens' AddTagsResponse Int
atrsResponseStatus = lens _atrsResponseStatus (\ s a -> s{_atrsResponseStatus = a})

instance NFData AddTagsResponse where
