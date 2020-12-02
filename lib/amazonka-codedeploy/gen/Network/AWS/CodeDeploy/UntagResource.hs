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
-- Module      : Network.AWS.CodeDeploy.UntagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a resource from a list of tags. The resource is identified by the @ResourceArn@ input parameter. The tags are identified by the list of keys in the @TagKeys@ input parameter.
module Network.AWS.CodeDeploy.UntagResource
  ( -- * Creating a Request
    untagResource,
    UntagResource,

    -- * Request Lenses
    urResourceARN,
    urTagKeys,

    -- * Destructuring the Response
    untagResourceResponse,
    UntagResourceResponse,

    -- * Response Lenses
    urrsResponseStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'untagResource' smart constructor.
data UntagResource = UntagResource'
  { _urResourceARN :: !Text,
    _urTagKeys :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UntagResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urResourceARN' - The Amazon Resource Name (ARN) that specifies from which resource to disassociate the tags with the keys in the @TagKeys@ input parameter.
--
-- * 'urTagKeys' - A list of keys of @Tag@ objects. The @Tag@ objects identified by the keys are disassociated from the resource specified by the @ResourceArn@ input parameter.
untagResource ::
  -- | 'urResourceARN'
  Text ->
  UntagResource
untagResource pResourceARN_ =
  UntagResource'
    { _urResourceARN = pResourceARN_,
      _urTagKeys = mempty
    }

-- | The Amazon Resource Name (ARN) that specifies from which resource to disassociate the tags with the keys in the @TagKeys@ input parameter.
urResourceARN :: Lens' UntagResource Text
urResourceARN = lens _urResourceARN (\s a -> s {_urResourceARN = a})

-- | A list of keys of @Tag@ objects. The @Tag@ objects identified by the keys are disassociated from the resource specified by the @ResourceArn@ input parameter.
urTagKeys :: Lens' UntagResource [Text]
urTagKeys = lens _urTagKeys (\s a -> s {_urTagKeys = a}) . _Coerce

instance AWSRequest UntagResource where
  type Rs UntagResource = UntagResourceResponse
  request = postJSON codeDeploy
  response =
    receiveEmpty
      (\s h x -> UntagResourceResponse' <$> (pure (fromEnum s)))

instance Hashable UntagResource

instance NFData UntagResource

instance ToHeaders UntagResource where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeDeploy_20141006.UntagResource" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UntagResource where
  toJSON UntagResource' {..} =
    object
      ( catMaybes
          [ Just ("ResourceArn" .= _urResourceARN),
            Just ("TagKeys" .= _urTagKeys)
          ]
      )

instance ToPath UntagResource where
  toPath = const "/"

instance ToQuery UntagResource where
  toQuery = const mempty

-- | /See:/ 'untagResourceResponse' smart constructor.
newtype UntagResourceResponse = UntagResourceResponse'
  { _urrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UntagResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urrsResponseStatus' - -- | The response status code.
untagResourceResponse ::
  -- | 'urrsResponseStatus'
  Int ->
  UntagResourceResponse
untagResourceResponse pResponseStatus_ =
  UntagResourceResponse' {_urrsResponseStatus = pResponseStatus_}

-- | -- | The response status code.
urrsResponseStatus :: Lens' UntagResourceResponse Int
urrsResponseStatus = lens _urrsResponseStatus (\s a -> s {_urrsResponseStatus = a})

instance NFData UntagResourceResponse
