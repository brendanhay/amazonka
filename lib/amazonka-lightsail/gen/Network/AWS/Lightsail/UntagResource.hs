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
-- Module      : Network.AWS.Lightsail.UntagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified set of tag keys and their values from the specified Amazon Lightsail resource.
--
--
-- The @untag resource@ operation supports tag-based access control via request tags and resource tags applied to the resource identified by @resource name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.UntagResource
  ( -- * Creating a Request
    untagResource,
    UntagResource,

    -- * Request Lenses
    urResourceARN,
    urResourceName,
    urTagKeys,

    -- * Destructuring the Response
    untagResourceResponse,
    UntagResourceResponse,

    -- * Response Lenses
    urrsOperations,
    urrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'untagResource' smart constructor.
data UntagResource = UntagResource'
  { _urResourceARN ::
      !(Maybe Text),
    _urResourceName :: !Text,
    _urTagKeys :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UntagResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urResourceARN' - The Amazon Resource Name (ARN) of the resource from which you want to remove a tag.
--
-- * 'urResourceName' - The name of the resource from which you are removing a tag.
--
-- * 'urTagKeys' - The tag keys to delete from the specified resource.
untagResource ::
  -- | 'urResourceName'
  Text ->
  UntagResource
untagResource pResourceName_ =
  UntagResource'
    { _urResourceARN = Nothing,
      _urResourceName = pResourceName_,
      _urTagKeys = mempty
    }

-- | The Amazon Resource Name (ARN) of the resource from which you want to remove a tag.
urResourceARN :: Lens' UntagResource (Maybe Text)
urResourceARN = lens _urResourceARN (\s a -> s {_urResourceARN = a})

-- | The name of the resource from which you are removing a tag.
urResourceName :: Lens' UntagResource Text
urResourceName = lens _urResourceName (\s a -> s {_urResourceName = a})

-- | The tag keys to delete from the specified resource.
urTagKeys :: Lens' UntagResource [Text]
urTagKeys = lens _urTagKeys (\s a -> s {_urTagKeys = a}) . _Coerce

instance AWSRequest UntagResource where
  type Rs UntagResource = UntagResourceResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          UntagResourceResponse'
            <$> (x .?> "operations" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable UntagResource

instance NFData UntagResource

instance ToHeaders UntagResource where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Lightsail_20161128.UntagResource" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UntagResource where
  toJSON UntagResource' {..} =
    object
      ( catMaybes
          [ ("resourceArn" .=) <$> _urResourceARN,
            Just ("resourceName" .= _urResourceName),
            Just ("tagKeys" .= _urTagKeys)
          ]
      )

instance ToPath UntagResource where
  toPath = const "/"

instance ToQuery UntagResource where
  toQuery = const mempty

-- | /See:/ 'untagResourceResponse' smart constructor.
data UntagResourceResponse = UntagResourceResponse'
  { _urrsOperations ::
      !(Maybe [Operation]),
    _urrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UntagResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urrsOperations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- * 'urrsResponseStatus' - -- | The response status code.
untagResourceResponse ::
  -- | 'urrsResponseStatus'
  Int ->
  UntagResourceResponse
untagResourceResponse pResponseStatus_ =
  UntagResourceResponse'
    { _urrsOperations = Nothing,
      _urrsResponseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
urrsOperations :: Lens' UntagResourceResponse [Operation]
urrsOperations = lens _urrsOperations (\s a -> s {_urrsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
urrsResponseStatus :: Lens' UntagResourceResponse Int
urrsResponseStatus = lens _urrsResponseStatus (\s a -> s {_urrsResponseStatus = a})

instance NFData UntagResourceResponse
