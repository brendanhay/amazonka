{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    dtprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A request to delete a specified traffic policy version.
--
-- /See:/ 'mkDeleteTrafficPolicy' smart constructor.
data DeleteTrafficPolicy = DeleteTrafficPolicy'
  { -- | The ID of the traffic policy that you want to delete.
    id :: Types.Id,
    -- | The version number of the traffic policy that you want to delete.
    version :: Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTrafficPolicy' value with any optional fields omitted.
mkDeleteTrafficPolicy ::
  -- | 'id'
  Types.Id ->
  -- | 'version'
  Core.Natural ->
  DeleteTrafficPolicy
mkDeleteTrafficPolicy id version =
  DeleteTrafficPolicy' {id, version}

-- | The ID of the traffic policy that you want to delete.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtpId :: Lens.Lens' DeleteTrafficPolicy Types.Id
dtpId = Lens.field @"id"
{-# DEPRECATED dtpId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The version number of the traffic policy that you want to delete.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtpVersion :: Lens.Lens' DeleteTrafficPolicy Core.Natural
dtpVersion = Lens.field @"version"
{-# DEPRECATED dtpVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.AWSRequest DeleteTrafficPolicy where
  type Rs DeleteTrafficPolicy = DeleteTrafficPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/2013-04-01/trafficpolicy/" Core.<> (Core.toText id)
                Core.<> ("/")
                Core.<> (Core.toText version)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteTrafficPolicyResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | An empty element.
--
-- /See:/ 'mkDeleteTrafficPolicyResponse' smart constructor.
newtype DeleteTrafficPolicyResponse = DeleteTrafficPolicyResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTrafficPolicyResponse' value with any optional fields omitted.
mkDeleteTrafficPolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteTrafficPolicyResponse
mkDeleteTrafficPolicyResponse responseStatus =
  DeleteTrafficPolicyResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtprrsResponseStatus :: Lens.Lens' DeleteTrafficPolicyResponse Core.Int
dtprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
