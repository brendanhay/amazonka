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
-- Module      : Network.AWS.Glue.UntagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes tags from a resource.
module Network.AWS.Glue.UntagResource
  ( -- * Creating a Request
    untagResource,
    UntagResource,

    -- * Request Lenses
    urResourceARN,
    urTagsToRemove,

    -- * Destructuring the Response
    untagResourceResponse,
    UntagResourceResponse,

    -- * Response Lenses
    untrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'untagResource' smart constructor.
data UntagResource = UntagResource'
  { _urResourceARN :: !Text,
    _urTagsToRemove :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UntagResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urResourceARN' - The Amazon Resource Name (ARN) of the resource from which to remove the tags.
--
-- * 'urTagsToRemove' - Tags to remove from this resource.
untagResource ::
  -- | 'urResourceARN'
  Text ->
  UntagResource
untagResource pResourceARN_ =
  UntagResource'
    { _urResourceARN = pResourceARN_,
      _urTagsToRemove = mempty
    }

-- | The Amazon Resource Name (ARN) of the resource from which to remove the tags.
urResourceARN :: Lens' UntagResource Text
urResourceARN = lens _urResourceARN (\s a -> s {_urResourceARN = a})

-- | Tags to remove from this resource.
urTagsToRemove :: Lens' UntagResource [Text]
urTagsToRemove = lens _urTagsToRemove (\s a -> s {_urTagsToRemove = a}) . _Coerce

instance AWSRequest UntagResource where
  type Rs UntagResource = UntagResourceResponse
  request = postJSON glue
  response =
    receiveEmpty
      (\s h x -> UntagResourceResponse' <$> (pure (fromEnum s)))

instance Hashable UntagResource

instance NFData UntagResource

instance ToHeaders UntagResource where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.UntagResource" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UntagResource where
  toJSON UntagResource' {..} =
    object
      ( catMaybes
          [ Just ("ResourceArn" .= _urResourceARN),
            Just ("TagsToRemove" .= _urTagsToRemove)
          ]
      )

instance ToPath UntagResource where
  toPath = const "/"

instance ToQuery UntagResource where
  toQuery = const mempty

-- | /See:/ 'untagResourceResponse' smart constructor.
newtype UntagResourceResponse = UntagResourceResponse'
  { _untrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UntagResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'untrsResponseStatus' - -- | The response status code.
untagResourceResponse ::
  -- | 'untrsResponseStatus'
  Int ->
  UntagResourceResponse
untagResourceResponse pResponseStatus_ =
  UntagResourceResponse' {_untrsResponseStatus = pResponseStatus_}

-- | -- | The response status code.
untrsResponseStatus :: Lens' UntagResourceResponse Int
untrsResponseStatus = lens _untrsResponseStatus (\s a -> s {_untrsResponseStatus = a})

instance NFData UntagResourceResponse
