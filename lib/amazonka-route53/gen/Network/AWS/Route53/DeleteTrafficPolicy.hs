{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.DeleteTrafficPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a traffic policy.
--
-- When you delete a traffic policy, Route 53 sets a flag on the policy to indicate that it has been deleted. However, Route 53 never fully deletes the traffic policy. Note the following:
--
--     * Deleted traffic policies aren't listed if you run <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ListTrafficPolicies.html ListTrafficPolicies> .
--
--
--     * There's no way to get a list of deleted policies.
--
--
--     * If you retain the ID of the policy, you can get information about the policy, including the traffic policy document, by running <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetTrafficPolicy.html GetTrafficPolicy> .
module Network.AWS.Route53.DeleteTrafficPolicy
  ( -- * Creating a request
    DeleteTrafficPolicy (..),
    mkDeleteTrafficPolicy,

    -- ** Request lenses
    dtpId,
    dtpVersion,

    -- * Destructuring the response
    DeleteTrafficPolicyResponse (..),
    mkDeleteTrafficPolicyResponse,

    -- ** Response lenses
    dtprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A request to delete a specified traffic policy version.
--
-- /See:/ 'mkDeleteTrafficPolicy' smart constructor.
data DeleteTrafficPolicy = DeleteTrafficPolicy'
  { id :: Lude.Text,
    version :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTrafficPolicy' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the traffic policy that you want to delete.
-- * 'version' - The version number of the traffic policy that you want to delete.
mkDeleteTrafficPolicy ::
  -- | 'id'
  Lude.Text ->
  -- | 'version'
  Lude.Natural ->
  DeleteTrafficPolicy
mkDeleteTrafficPolicy pId_ pVersion_ =
  DeleteTrafficPolicy' {id = pId_, version = pVersion_}

-- | The ID of the traffic policy that you want to delete.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtpId :: Lens.Lens' DeleteTrafficPolicy Lude.Text
dtpId = Lens.lens (id :: DeleteTrafficPolicy -> Lude.Text) (\s a -> s {id = a} :: DeleteTrafficPolicy)
{-# DEPRECATED dtpId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The version number of the traffic policy that you want to delete.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtpVersion :: Lens.Lens' DeleteTrafficPolicy Lude.Natural
dtpVersion = Lens.lens (version :: DeleteTrafficPolicy -> Lude.Natural) (\s a -> s {version = a} :: DeleteTrafficPolicy)
{-# DEPRECATED dtpVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.AWSRequest DeleteTrafficPolicy where
  type Rs DeleteTrafficPolicy = DeleteTrafficPolicyResponse
  request = Req.delete route53Service
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteTrafficPolicyResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteTrafficPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteTrafficPolicy where
  toPath DeleteTrafficPolicy' {..} =
    Lude.mconcat
      [ "/2013-04-01/trafficpolicy/",
        Lude.toBS id,
        "/",
        Lude.toBS version
      ]

instance Lude.ToQuery DeleteTrafficPolicy where
  toQuery = Lude.const Lude.mempty

-- | An empty element.
--
-- /See:/ 'mkDeleteTrafficPolicyResponse' smart constructor.
newtype DeleteTrafficPolicyResponse = DeleteTrafficPolicyResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTrafficPolicyResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteTrafficPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteTrafficPolicyResponse
mkDeleteTrafficPolicyResponse pResponseStatus_ =
  DeleteTrafficPolicyResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtprsResponseStatus :: Lens.Lens' DeleteTrafficPolicyResponse Lude.Int
dtprsResponseStatus = Lens.lens (responseStatus :: DeleteTrafficPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteTrafficPolicyResponse)
{-# DEPRECATED dtprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
