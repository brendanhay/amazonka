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
-- Module      : Network.AWS.EFS.UntagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes tags from an EFS resource. You can remove tags from EFS file systems and access points using this API operation.
--
--
-- This operation requires permissions for the @elasticfilesystem:UntagResource@ action.
module Network.AWS.EFS.UntagResource
  ( -- * Creating a Request
    untagResource,
    UntagResource,

    -- * Request Lenses
    urResourceId,
    urTagKeys,

    -- * Destructuring the Response
    untagResourceResponse,
    UntagResourceResponse,
  )
where

import Network.AWS.EFS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'untagResource' smart constructor.
data UntagResource = UntagResource'
  { _urResourceId :: !Text,
    _urTagKeys :: !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UntagResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urResourceId' - Specifies the EFS resource that you want to remove tags from.
--
-- * 'urTagKeys' - The keys of the key:value tag pairs that you want to remove from the specified EFS resource.
untagResource ::
  -- | 'urResourceId'
  Text ->
  -- | 'urTagKeys'
  NonEmpty Text ->
  UntagResource
untagResource pResourceId_ pTagKeys_ =
  UntagResource'
    { _urResourceId = pResourceId_,
      _urTagKeys = _List1 # pTagKeys_
    }

-- | Specifies the EFS resource that you want to remove tags from.
urResourceId :: Lens' UntagResource Text
urResourceId = lens _urResourceId (\s a -> s {_urResourceId = a})

-- | The keys of the key:value tag pairs that you want to remove from the specified EFS resource.
urTagKeys :: Lens' UntagResource (NonEmpty Text)
urTagKeys = lens _urTagKeys (\s a -> s {_urTagKeys = a}) . _List1

instance AWSRequest UntagResource where
  type Rs UntagResource = UntagResourceResponse
  request = delete efs
  response = receiveNull UntagResourceResponse'

instance Hashable UntagResource

instance NFData UntagResource

instance ToHeaders UntagResource where
  toHeaders = const mempty

instance ToPath UntagResource where
  toPath UntagResource' {..} =
    mconcat ["/2015-02-01/resource-tags/", toBS _urResourceId]

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
