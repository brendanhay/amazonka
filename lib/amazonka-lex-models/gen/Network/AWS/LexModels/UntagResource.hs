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
-- Module      : Network.AWS.LexModels.UntagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes tags from a bot, bot alias or bot channel.
module Network.AWS.LexModels.UntagResource
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

import Network.AWS.Lens
import Network.AWS.LexModels.Types
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
-- * 'urResourceARN' - The Amazon Resource Name (ARN) of the resource to remove the tags from.
--
-- * 'urTagKeys' - A list of tag keys to remove from the resource. If a tag key does not exist on the resource, it is ignored.
untagResource ::
  -- | 'urResourceARN'
  Text ->
  UntagResource
untagResource pResourceARN_ =
  UntagResource'
    { _urResourceARN = pResourceARN_,
      _urTagKeys = mempty
    }

-- | The Amazon Resource Name (ARN) of the resource to remove the tags from.
urResourceARN :: Lens' UntagResource Text
urResourceARN = lens _urResourceARN (\s a -> s {_urResourceARN = a})

-- | A list of tag keys to remove from the resource. If a tag key does not exist on the resource, it is ignored.
urTagKeys :: Lens' UntagResource [Text]
urTagKeys = lens _urTagKeys (\s a -> s {_urTagKeys = a}) . _Coerce

instance AWSRequest UntagResource where
  type Rs UntagResource = UntagResourceResponse
  request = delete lexModels
  response =
    receiveEmpty
      (\s h x -> UntagResourceResponse' <$> (pure (fromEnum s)))

instance Hashable UntagResource

instance NFData UntagResource

instance ToHeaders UntagResource where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath UntagResource where
  toPath UntagResource' {..} = mconcat ["/tags/", toBS _urResourceARN]

instance ToQuery UntagResource where
  toQuery UntagResource' {..} =
    mconcat ["tagKeys" =: toQueryList "member" _urTagKeys]

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
