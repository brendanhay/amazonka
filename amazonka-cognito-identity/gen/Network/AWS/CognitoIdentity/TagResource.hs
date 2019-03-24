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
-- Module      : Network.AWS.CognitoIdentity.TagResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns a set of tags to an Amazon Cognito identity pool. A tag is a label that you can use to categorize and manage identity pools in different ways, such as by purpose, owner, environment, or other criteria.
--
--
-- Each tag consists of a key and value, both of which you define. A key is a general category for more specific values. For example, if you have two versions of an identity pool, one for testing and another for production, you might assign an @Environment@ tag key to both identity pools. The value of this key might be @Test@ for one identity pool and @Production@ for the other.
--
-- Tags are useful for cost tracking and access control. You can activate your tags so that they appear on the Billing and Cost Management console, where you can track the costs associated with your identity pools. In an IAM policy, you can constrain permissions for identity pools based on specific tags or tag values.
--
-- You can use this action up to 5 times per second, per account. An identity pool can have as many as 50 tags.
--
module Network.AWS.CognitoIdentity.TagResource
    (
    -- * Creating a Request
      tagResource
    , TagResource
    -- * Request Lenses
    , trTags
    , trResourceARN

    -- * Destructuring the Response
    , tagResourceResponse
    , TagResourceResponse
    -- * Response Lenses
    , trrsResponseStatus
    ) where

import Network.AWS.CognitoIdentity.Types
import Network.AWS.CognitoIdentity.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'tagResource' smart constructor.
data TagResource = TagResource'
  { _trTags        :: !(Maybe (Map Text Text))
  , _trResourceARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trTags' - The tags to assign to the identity pool.
--
-- * 'trResourceARN' - The Amazon Resource Name (ARN) of the identity pool to assign the tags to.
tagResource
    :: Text -- ^ 'trResourceARN'
    -> TagResource
tagResource pResourceARN_ =
  TagResource' {_trTags = Nothing, _trResourceARN = pResourceARN_}


-- | The tags to assign to the identity pool.
trTags :: Lens' TagResource (HashMap Text Text)
trTags = lens _trTags (\ s a -> s{_trTags = a}) . _Default . _Map

-- | The Amazon Resource Name (ARN) of the identity pool to assign the tags to.
trResourceARN :: Lens' TagResource Text
trResourceARN = lens _trResourceARN (\ s a -> s{_trResourceARN = a})

instance AWSRequest TagResource where
        type Rs TagResource = TagResourceResponse
        request = postJSON cognitoIdentity
        response
          = receiveEmpty
              (\ s h x ->
                 TagResourceResponse' <$> (pure (fromEnum s)))

instance Hashable TagResource where

instance NFData TagResource where

instance ToHeaders TagResource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityService.TagResource" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON TagResource where
        toJSON TagResource'{..}
          = object
              (catMaybes
                 [("Tags" .=) <$> _trTags,
                  Just ("ResourceArn" .= _trResourceARN)])

instance ToPath TagResource where
        toPath = const "/"

instance ToQuery TagResource where
        toQuery = const mempty

-- | /See:/ 'tagResourceResponse' smart constructor.
newtype TagResourceResponse = TagResourceResponse'
  { _trrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trrsResponseStatus' - -- | The response status code.
tagResourceResponse
    :: Int -- ^ 'trrsResponseStatus'
    -> TagResourceResponse
tagResourceResponse pResponseStatus_ =
  TagResourceResponse' {_trrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
trrsResponseStatus :: Lens' TagResourceResponse Int
trrsResponseStatus = lens _trrsResponseStatus (\ s a -> s{_trrsResponseStatus = a})

instance NFData TagResourceResponse where
