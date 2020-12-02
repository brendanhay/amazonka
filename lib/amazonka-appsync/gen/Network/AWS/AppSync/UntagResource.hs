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
-- Module      : Network.AWS.AppSync.UntagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Untags a resource.
module Network.AWS.AppSync.UntagResource
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
    ursResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'untagResource' smart constructor.
data UntagResource = UntagResource'
  { _urResourceARN :: !Text,
    _urTagKeys :: !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UntagResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urResourceARN' - The @GraphqlApi@ ARN.
--
-- * 'urTagKeys' - A list of @TagKey@ objects.
untagResource ::
  -- | 'urResourceARN'
  Text ->
  -- | 'urTagKeys'
  NonEmpty Text ->
  UntagResource
untagResource pResourceARN_ pTagKeys_ =
  UntagResource'
    { _urResourceARN = pResourceARN_,
      _urTagKeys = _List1 # pTagKeys_
    }

-- | The @GraphqlApi@ ARN.
urResourceARN :: Lens' UntagResource Text
urResourceARN = lens _urResourceARN (\s a -> s {_urResourceARN = a})

-- | A list of @TagKey@ objects.
urTagKeys :: Lens' UntagResource (NonEmpty Text)
urTagKeys = lens _urTagKeys (\s a -> s {_urTagKeys = a}) . _List1

instance AWSRequest UntagResource where
  type Rs UntagResource = UntagResourceResponse
  request = delete appSync
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
  toPath UntagResource' {..} =
    mconcat ["/v1/tags/", toBS _urResourceARN]

instance ToQuery UntagResource where
  toQuery UntagResource' {..} =
    mconcat ["tagKeys" =: toQueryList "member" _urTagKeys]

-- | /See:/ 'untagResourceResponse' smart constructor.
newtype UntagResourceResponse = UntagResourceResponse'
  { _ursResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UntagResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ursResponseStatus' - -- | The response status code.
untagResourceResponse ::
  -- | 'ursResponseStatus'
  Int ->
  UntagResourceResponse
untagResourceResponse pResponseStatus_ =
  UntagResourceResponse' {_ursResponseStatus = pResponseStatus_}

-- | -- | The response status code.
ursResponseStatus :: Lens' UntagResourceResponse Int
ursResponseStatus = lens _ursResponseStatus (\s a -> s {_ursResponseStatus = a})

instance NFData UntagResourceResponse
