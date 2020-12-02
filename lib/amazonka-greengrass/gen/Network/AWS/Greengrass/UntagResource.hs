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
-- Module      : Network.AWS.Greengrass.UntagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove resource tags from a Greengrass Resource.
module Network.AWS.Greengrass.UntagResource
  ( -- * Creating a Request
    untagResource,
    UntagResource,

    -- * Request Lenses
    urTagKeys,
    urResourceARN,

    -- * Destructuring the Response
    untagResourceResponse,
    UntagResourceResponse,
  )
where

import Network.AWS.Greengrass.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'untagResource' smart constructor.
data UntagResource = UntagResource'
  { _urTagKeys :: ![Text],
    _urResourceARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UntagResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urTagKeys' - An array of tag keys to delete
--
-- * 'urResourceARN' - The Amazon Resource Name (ARN) of the resource.
untagResource ::
  -- | 'urResourceARN'
  Text ->
  UntagResource
untagResource pResourceARN_ =
  UntagResource'
    { _urTagKeys = mempty,
      _urResourceARN = pResourceARN_
    }

-- | An array of tag keys to delete
urTagKeys :: Lens' UntagResource [Text]
urTagKeys = lens _urTagKeys (\s a -> s {_urTagKeys = a}) . _Coerce

-- | The Amazon Resource Name (ARN) of the resource.
urResourceARN :: Lens' UntagResource Text
urResourceARN = lens _urResourceARN (\s a -> s {_urResourceARN = a})

instance AWSRequest UntagResource where
  type Rs UntagResource = UntagResourceResponse
  request = delete greengrass
  response = receiveNull UntagResourceResponse'

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
data UntagResourceResponse = UntagResourceResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UntagResourceResponse' with the minimum fields required to make a request.
untagResourceResponse ::
  UntagResourceResponse
untagResourceResponse = UntagResourceResponse'

instance NFData UntagResourceResponse
