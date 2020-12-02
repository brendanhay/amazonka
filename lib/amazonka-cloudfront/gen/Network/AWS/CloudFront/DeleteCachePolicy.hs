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
-- Module      : Network.AWS.CloudFront.DeleteCachePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a cache policy.
--
--
-- You cannot delete a cache policy if it’s attached to a cache behavior. First update your distributions to remove the cache policy from all cache behaviors, then delete the cache policy.
--
-- To delete a cache policy, you must provide the policy’s identifier and version. To get these values, you can use @ListCachePolicies@ or @GetCachePolicy@ .
module Network.AWS.CloudFront.DeleteCachePolicy
  ( -- * Creating a Request
    deleteCachePolicy,
    DeleteCachePolicy,

    -- * Request Lenses
    dcpIfMatch,
    dcpId,

    -- * Destructuring the Response
    deleteCachePolicyResponse,
    DeleteCachePolicyResponse,
  )
where

import Network.AWS.CloudFront.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteCachePolicy' smart constructor.
data DeleteCachePolicy = DeleteCachePolicy'
  { _dcpIfMatch ::
      !(Maybe Text),
    _dcpId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteCachePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcpIfMatch' - The version of the cache policy that you are deleting. The version is the cache policy’s @ETag@ value, which you can get using @ListCachePolicies@ , @GetCachePolicy@ , or @GetCachePolicyConfig@ .
--
-- * 'dcpId' - The unique identifier for the cache policy that you are deleting. To get the identifier, you can use @ListCachePolicies@ .
deleteCachePolicy ::
  -- | 'dcpId'
  Text ->
  DeleteCachePolicy
deleteCachePolicy pId_ =
  DeleteCachePolicy' {_dcpIfMatch = Nothing, _dcpId = pId_}

-- | The version of the cache policy that you are deleting. The version is the cache policy’s @ETag@ value, which you can get using @ListCachePolicies@ , @GetCachePolicy@ , or @GetCachePolicyConfig@ .
dcpIfMatch :: Lens' DeleteCachePolicy (Maybe Text)
dcpIfMatch = lens _dcpIfMatch (\s a -> s {_dcpIfMatch = a})

-- | The unique identifier for the cache policy that you are deleting. To get the identifier, you can use @ListCachePolicies@ .
dcpId :: Lens' DeleteCachePolicy Text
dcpId = lens _dcpId (\s a -> s {_dcpId = a})

instance AWSRequest DeleteCachePolicy where
  type Rs DeleteCachePolicy = DeleteCachePolicyResponse
  request = delete cloudFront
  response = receiveNull DeleteCachePolicyResponse'

instance Hashable DeleteCachePolicy

instance NFData DeleteCachePolicy

instance ToHeaders DeleteCachePolicy where
  toHeaders DeleteCachePolicy' {..} =
    mconcat ["If-Match" =# _dcpIfMatch]

instance ToPath DeleteCachePolicy where
  toPath DeleteCachePolicy' {..} =
    mconcat ["/2020-05-31/cache-policy/", toBS _dcpId]

instance ToQuery DeleteCachePolicy where
  toQuery = const mempty

-- | /See:/ 'deleteCachePolicyResponse' smart constructor.
data DeleteCachePolicyResponse = DeleteCachePolicyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteCachePolicyResponse' with the minimum fields required to make a request.
deleteCachePolicyResponse ::
  DeleteCachePolicyResponse
deleteCachePolicyResponse = DeleteCachePolicyResponse'

instance NFData DeleteCachePolicyResponse
