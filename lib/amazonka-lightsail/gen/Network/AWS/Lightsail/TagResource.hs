{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.TagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to the specified Amazon Lightsail resource. Each resource can have a maximum of 50 tags. Each tag consists of a key and an optional value. Tag keys must be unique per resource. For more information about tags, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
--
--
-- The @tag resource@ operation supports tag-based access control via request tags and resource tags applied to the resource identified by @resource name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.TagResource
  ( -- * Creating a Request
    tagResource,
    TagResource,

    -- * Request Lenses
    trResourceARN,
    trResourceName,
    trTags,

    -- * Destructuring the Response
    tagResourceResponse,
    TagResourceResponse,

    -- * Response Lenses
    trrsOperations,
    trrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'tagResource' smart constructor.
data TagResource = TagResource'
  { _trResourceARN :: !(Maybe Text),
    _trResourceName :: !Text,
    _trTags :: ![Tag]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TagResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trResourceARN' - The Amazon Resource Name (ARN) of the resource to which you want to add a tag.
--
-- * 'trResourceName' - The name of the resource to which you are adding tags.
--
-- * 'trTags' - The tag key and optional value.
tagResource ::
  -- | 'trResourceName'
  Text ->
  TagResource
tagResource pResourceName_ =
  TagResource'
    { _trResourceARN = Nothing,
      _trResourceName = pResourceName_,
      _trTags = mempty
    }

-- | The Amazon Resource Name (ARN) of the resource to which you want to add a tag.
trResourceARN :: Lens' TagResource (Maybe Text)
trResourceARN = lens _trResourceARN (\s a -> s {_trResourceARN = a})

-- | The name of the resource to which you are adding tags.
trResourceName :: Lens' TagResource Text
trResourceName = lens _trResourceName (\s a -> s {_trResourceName = a})

-- | The tag key and optional value.
trTags :: Lens' TagResource [Tag]
trTags = lens _trTags (\s a -> s {_trTags = a}) . _Coerce

instance AWSRequest TagResource where
  type Rs TagResource = TagResourceResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          TagResourceResponse'
            <$> (x .?> "operations" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable TagResource

instance NFData TagResource

instance ToHeaders TagResource where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Lightsail_20161128.TagResource" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON TagResource where
  toJSON TagResource' {..} =
    object
      ( catMaybes
          [ ("resourceArn" .=) <$> _trResourceARN,
            Just ("resourceName" .= _trResourceName),
            Just ("tags" .= _trTags)
          ]
      )

instance ToPath TagResource where
  toPath = const "/"

instance ToQuery TagResource where
  toQuery = const mempty

-- | /See:/ 'tagResourceResponse' smart constructor.
data TagResourceResponse = TagResourceResponse'
  { _trrsOperations ::
      !(Maybe [Operation]),
    _trrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TagResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trrsOperations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- * 'trrsResponseStatus' - -- | The response status code.
tagResourceResponse ::
  -- | 'trrsResponseStatus'
  Int ->
  TagResourceResponse
tagResourceResponse pResponseStatus_ =
  TagResourceResponse'
    { _trrsOperations = Nothing,
      _trrsResponseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
trrsOperations :: Lens' TagResourceResponse [Operation]
trrsOperations = lens _trrsOperations (\s a -> s {_trrsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
trrsResponseStatus :: Lens' TagResourceResponse Int
trrsResponseStatus = lens _trrsResponseStatus (\s a -> s {_trrsResponseStatus = a})

instance NFData TagResourceResponse
