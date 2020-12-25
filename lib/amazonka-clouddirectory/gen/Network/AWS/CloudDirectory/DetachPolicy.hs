{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.DetachPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a policy from an object.
module Network.AWS.CloudDirectory.DetachPolicy
  ( -- * Creating a request
    DetachPolicy (..),
    mkDetachPolicy,

    -- ** Request lenses
    dpDirectoryArn,
    dpPolicyReference,
    dpObjectReference,

    -- * Destructuring the response
    DetachPolicyResponse (..),
    mkDetachPolicyResponse,

    -- ** Response lenses
    dprrsResponseStatus,
  )
where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDetachPolicy' smart constructor.
data DetachPolicy = DetachPolicy'
  { -- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where both objects reside. For more information, see 'arns' .
    directoryArn :: Types.Arn,
    -- | Reference that identifies the policy object.
    policyReference :: Types.ObjectReference,
    -- | Reference that identifies the object whose policy object will be detached.
    objectReference :: Types.ObjectReference
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetachPolicy' value with any optional fields omitted.
mkDetachPolicy ::
  -- | 'directoryArn'
  Types.Arn ->
  -- | 'policyReference'
  Types.ObjectReference ->
  -- | 'objectReference'
  Types.ObjectReference ->
  DetachPolicy
mkDetachPolicy directoryArn policyReference objectReference =
  DetachPolicy' {directoryArn, policyReference, objectReference}

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where both objects reside. For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpDirectoryArn :: Lens.Lens' DetachPolicy Types.Arn
dpDirectoryArn = Lens.field @"directoryArn"
{-# DEPRECATED dpDirectoryArn "Use generic-lens or generic-optics with 'directoryArn' instead." #-}

-- | Reference that identifies the policy object.
--
-- /Note:/ Consider using 'policyReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpPolicyReference :: Lens.Lens' DetachPolicy Types.ObjectReference
dpPolicyReference = Lens.field @"policyReference"
{-# DEPRECATED dpPolicyReference "Use generic-lens or generic-optics with 'policyReference' instead." #-}

-- | Reference that identifies the object whose policy object will be detached.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpObjectReference :: Lens.Lens' DetachPolicy Types.ObjectReference
dpObjectReference = Lens.field @"objectReference"
{-# DEPRECATED dpObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

instance Core.FromJSON DetachPolicy where
  toJSON DetachPolicy {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("PolicyReference" Core..= policyReference),
            Core.Just ("ObjectReference" Core..= objectReference)
          ]
      )

instance Core.AWSRequest DetachPolicy where
  type Rs DetachPolicy = DetachPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath "/amazonclouddirectory/2017-01-11/policy/detach",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "x-amz-data-partition" directoryArn,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DetachPolicyResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDetachPolicyResponse' smart constructor.
newtype DetachPolicyResponse = DetachPolicyResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DetachPolicyResponse' value with any optional fields omitted.
mkDetachPolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DetachPolicyResponse
mkDetachPolicyResponse responseStatus =
  DetachPolicyResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsResponseStatus :: Lens.Lens' DetachPolicyResponse Core.Int
dprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
