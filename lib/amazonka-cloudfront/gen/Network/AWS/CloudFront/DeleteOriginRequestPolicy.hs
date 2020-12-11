{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.DeleteOriginRequestPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an origin request policy.
--
-- You cannot delete an origin request policy if it’s attached to any cache behaviors. First update your distributions to remove the origin request policy from all cache behaviors, then delete the origin request policy.
-- To delete an origin request policy, you must provide the policy’s identifier and version. To get the identifier, you can use @ListOriginRequestPolicies@ or @GetOriginRequestPolicy@ .
module Network.AWS.CloudFront.DeleteOriginRequestPolicy
  ( -- * Creating a request
    DeleteOriginRequestPolicy (..),
    mkDeleteOriginRequestPolicy,

    -- ** Request lenses
    dorpIfMatch,
    dorpId,

    -- * Destructuring the response
    DeleteOriginRequestPolicyResponse (..),
    mkDeleteOriginRequestPolicyResponse,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteOriginRequestPolicy' smart constructor.
data DeleteOriginRequestPolicy = DeleteOriginRequestPolicy'
  { ifMatch ::
      Lude.Maybe Lude.Text,
    id :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteOriginRequestPolicy' with the minimum fields required to make a request.
--
-- * 'id' - The unique identifier for the origin request policy that you are deleting. To get the identifier, you can use @ListOriginRequestPolicies@ .
-- * 'ifMatch' - The version of the origin request policy that you are deleting. The version is the origin request policy’s @ETag@ value, which you can get using @ListOriginRequestPolicies@ , @GetOriginRequestPolicy@ , or @GetOriginRequestPolicyConfig@ .
mkDeleteOriginRequestPolicy ::
  -- | 'id'
  Lude.Text ->
  DeleteOriginRequestPolicy
mkDeleteOriginRequestPolicy pId_ =
  DeleteOriginRequestPolicy' {ifMatch = Lude.Nothing, id = pId_}

-- | The version of the origin request policy that you are deleting. The version is the origin request policy’s @ETag@ value, which you can get using @ListOriginRequestPolicies@ , @GetOriginRequestPolicy@ , or @GetOriginRequestPolicyConfig@ .
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorpIfMatch :: Lens.Lens' DeleteOriginRequestPolicy (Lude.Maybe Lude.Text)
dorpIfMatch = Lens.lens (ifMatch :: DeleteOriginRequestPolicy -> Lude.Maybe Lude.Text) (\s a -> s {ifMatch = a} :: DeleteOriginRequestPolicy)
{-# DEPRECATED dorpIfMatch "Use generic-lens or generic-optics with 'ifMatch' instead." #-}

-- | The unique identifier for the origin request policy that you are deleting. To get the identifier, you can use @ListOriginRequestPolicies@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorpId :: Lens.Lens' DeleteOriginRequestPolicy Lude.Text
dorpId = Lens.lens (id :: DeleteOriginRequestPolicy -> Lude.Text) (\s a -> s {id = a} :: DeleteOriginRequestPolicy)
{-# DEPRECATED dorpId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest DeleteOriginRequestPolicy where
  type
    Rs DeleteOriginRequestPolicy =
      DeleteOriginRequestPolicyResponse
  request = Req.delete cloudFrontService
  response = Res.receiveNull DeleteOriginRequestPolicyResponse'

instance Lude.ToHeaders DeleteOriginRequestPolicy where
  toHeaders DeleteOriginRequestPolicy' {..} =
    Lude.mconcat ["If-Match" Lude.=# ifMatch]

instance Lude.ToPath DeleteOriginRequestPolicy where
  toPath DeleteOriginRequestPolicy' {..} =
    Lude.mconcat ["/2020-05-31/origin-request-policy/", Lude.toBS id]

instance Lude.ToQuery DeleteOriginRequestPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteOriginRequestPolicyResponse' smart constructor.
data DeleteOriginRequestPolicyResponse = DeleteOriginRequestPolicyResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteOriginRequestPolicyResponse' with the minimum fields required to make a request.
mkDeleteOriginRequestPolicyResponse ::
  DeleteOriginRequestPolicyResponse
mkDeleteOriginRequestPolicyResponse =
  DeleteOriginRequestPolicyResponse'
