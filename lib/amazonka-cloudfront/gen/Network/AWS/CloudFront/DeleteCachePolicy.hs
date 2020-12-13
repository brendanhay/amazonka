{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
-- You cannot delete a cache policy if it’s attached to a cache behavior. First update your distributions to remove the cache policy from all cache behaviors, then delete the cache policy.
-- To delete a cache policy, you must provide the policy’s identifier and version. To get these values, you can use @ListCachePolicies@ or @GetCachePolicy@ .
module Network.AWS.CloudFront.DeleteCachePolicy
  ( -- * Creating a request
    DeleteCachePolicy (..),
    mkDeleteCachePolicy,

    -- ** Request lenses
    dcpIfMatch,
    dcpId,

    -- * Destructuring the response
    DeleteCachePolicyResponse (..),
    mkDeleteCachePolicyResponse,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteCachePolicy' smart constructor.
data DeleteCachePolicy = DeleteCachePolicy'
  { -- | The version of the cache policy that you are deleting. The version is the cache policy’s @ETag@ value, which you can get using @ListCachePolicies@ , @GetCachePolicy@ , or @GetCachePolicyConfig@ .
    ifMatch :: Lude.Maybe Lude.Text,
    -- | The unique identifier for the cache policy that you are deleting. To get the identifier, you can use @ListCachePolicies@ .
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCachePolicy' with the minimum fields required to make a request.
--
-- * 'ifMatch' - The version of the cache policy that you are deleting. The version is the cache policy’s @ETag@ value, which you can get using @ListCachePolicies@ , @GetCachePolicy@ , or @GetCachePolicyConfig@ .
-- * 'id' - The unique identifier for the cache policy that you are deleting. To get the identifier, you can use @ListCachePolicies@ .
mkDeleteCachePolicy ::
  -- | 'id'
  Lude.Text ->
  DeleteCachePolicy
mkDeleteCachePolicy pId_ =
  DeleteCachePolicy' {ifMatch = Lude.Nothing, id = pId_}

-- | The version of the cache policy that you are deleting. The version is the cache policy’s @ETag@ value, which you can get using @ListCachePolicies@ , @GetCachePolicy@ , or @GetCachePolicyConfig@ .
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpIfMatch :: Lens.Lens' DeleteCachePolicy (Lude.Maybe Lude.Text)
dcpIfMatch = Lens.lens (ifMatch :: DeleteCachePolicy -> Lude.Maybe Lude.Text) (\s a -> s {ifMatch = a} :: DeleteCachePolicy)
{-# DEPRECATED dcpIfMatch "Use generic-lens or generic-optics with 'ifMatch' instead." #-}

-- | The unique identifier for the cache policy that you are deleting. To get the identifier, you can use @ListCachePolicies@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpId :: Lens.Lens' DeleteCachePolicy Lude.Text
dcpId = Lens.lens (id :: DeleteCachePolicy -> Lude.Text) (\s a -> s {id = a} :: DeleteCachePolicy)
{-# DEPRECATED dcpId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest DeleteCachePolicy where
  type Rs DeleteCachePolicy = DeleteCachePolicyResponse
  request = Req.delete cloudFrontService
  response = Res.receiveNull DeleteCachePolicyResponse'

instance Lude.ToHeaders DeleteCachePolicy where
  toHeaders DeleteCachePolicy' {..} =
    Lude.mconcat ["If-Match" Lude.=# ifMatch]

instance Lude.ToPath DeleteCachePolicy where
  toPath DeleteCachePolicy' {..} =
    Lude.mconcat ["/2020-05-31/cache-policy/", Lude.toBS id]

instance Lude.ToQuery DeleteCachePolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteCachePolicyResponse' smart constructor.
data DeleteCachePolicyResponse = DeleteCachePolicyResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCachePolicyResponse' with the minimum fields required to make a request.
mkDeleteCachePolicyResponse ::
  DeleteCachePolicyResponse
mkDeleteCachePolicyResponse = DeleteCachePolicyResponse'
