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
-- Module      : Network.AWS.Lambda.TagResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a list of tags (key-value pairs) on the Lambda function. Requires the Lambda function ARN (Amazon Resource Name). If a key is specified without a value, Lambda creates a tag with the specified key and a value of null. For more information, see <http://docs.aws.amazon.com/lambda/latest/dg/tagging.html Tagging Lambda Functions> in the __AWS Lambda Developer Guide__ .
--
--
module Network.AWS.Lambda.TagResource
    (
    -- * Creating a Request
      tagResource
    , TagResource
    -- * Request Lenses
    , trResource
    , trTags

    -- * Destructuring the Response
    , tagResourceResponse
    , TagResourceResponse
    ) where

import Network.AWS.Lambda.Types
import Network.AWS.Lambda.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'tagResource' smart constructor.
data TagResource = TagResource'
  { _trResource :: !Text
  , _trTags     :: !(Map Text Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trResource' - The ARN (Amazon Resource Name) of the Lambda function. For more information, see <http://docs.aws.amazon.com/lambda/latest/dg/tagging.html Tagging Lambda Functions> in the __AWS Lambda Developer Guide__ .
--
-- * 'trTags' - The list of tags (key-value pairs) you are assigning to the Lambda function. For more information, see <http://docs.aws.amazon.com/lambda/latest/dg/tagging.html Tagging Lambda Functions> in the __AWS Lambda Developer Guide__ .
tagResource
    :: Text -- ^ 'trResource'
    -> TagResource
tagResource pResource_ =
  TagResource' {_trResource = pResource_, _trTags = mempty}


-- | The ARN (Amazon Resource Name) of the Lambda function. For more information, see <http://docs.aws.amazon.com/lambda/latest/dg/tagging.html Tagging Lambda Functions> in the __AWS Lambda Developer Guide__ .
trResource :: Lens' TagResource Text
trResource = lens _trResource (\ s a -> s{_trResource = a})

-- | The list of tags (key-value pairs) you are assigning to the Lambda function. For more information, see <http://docs.aws.amazon.com/lambda/latest/dg/tagging.html Tagging Lambda Functions> in the __AWS Lambda Developer Guide__ .
trTags :: Lens' TagResource (HashMap Text Text)
trTags = lens _trTags (\ s a -> s{_trTags = a}) . _Map

instance AWSRequest TagResource where
        type Rs TagResource = TagResourceResponse
        request = postJSON lambda
        response = receiveNull TagResourceResponse'

instance Hashable TagResource where

instance NFData TagResource where

instance ToHeaders TagResource where
        toHeaders = const mempty

instance ToJSON TagResource where
        toJSON TagResource'{..}
          = object (catMaybes [Just ("Tags" .= _trTags)])

instance ToPath TagResource where
        toPath TagResource'{..}
          = mconcat ["/2017-03-31/tags/", toBS _trResource]

instance ToQuery TagResource where
        toQuery = const mempty

-- | /See:/ 'tagResourceResponse' smart constructor.
data TagResourceResponse =
  TagResourceResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagResourceResponse' with the minimum fields required to make a request.
--
tagResourceResponse
    :: TagResourceResponse
tagResourceResponse = TagResourceResponse'


instance NFData TagResourceResponse where
