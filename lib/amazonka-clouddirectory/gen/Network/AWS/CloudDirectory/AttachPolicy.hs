{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.AttachPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a policy object to a regular object. An object can have a limited number of attached policies.
module Network.AWS.CloudDirectory.AttachPolicy
  ( -- * Creating a request
    AttachPolicy (..),
    mkAttachPolicy,

    -- ** Request lenses
    apDirectoryArn,
    apPolicyReference,
    apObjectReference,

    -- * Destructuring the response
    AttachPolicyResponse (..),
    mkAttachPolicyResponse,

    -- ** Response lenses
    aprrsResponseStatus,
  )
where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAttachPolicy' smart constructor.
data AttachPolicy = AttachPolicy'
  { -- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where both objects reside. For more information, see 'arns' .
    directoryArn :: Types.Arn,
    -- | The reference that is associated with the policy object.
    policyReference :: Types.ObjectReference,
    -- | The reference that identifies the object to which the policy will be attached.
    objectReference :: Types.ObjectReference
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachPolicy' value with any optional fields omitted.
mkAttachPolicy ::
  -- | 'directoryArn'
  Types.Arn ->
  -- | 'policyReference'
  Types.ObjectReference ->
  -- | 'objectReference'
  Types.ObjectReference ->
  AttachPolicy
mkAttachPolicy directoryArn policyReference objectReference =
  AttachPolicy' {directoryArn, policyReference, objectReference}

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where both objects reside. For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apDirectoryArn :: Lens.Lens' AttachPolicy Types.Arn
apDirectoryArn = Lens.field @"directoryArn"
{-# DEPRECATED apDirectoryArn "Use generic-lens or generic-optics with 'directoryArn' instead." #-}

-- | The reference that is associated with the policy object.
--
-- /Note:/ Consider using 'policyReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apPolicyReference :: Lens.Lens' AttachPolicy Types.ObjectReference
apPolicyReference = Lens.field @"policyReference"
{-# DEPRECATED apPolicyReference "Use generic-lens or generic-optics with 'policyReference' instead." #-}

-- | The reference that identifies the object to which the policy will be attached.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apObjectReference :: Lens.Lens' AttachPolicy Types.ObjectReference
apObjectReference = Lens.field @"objectReference"
{-# DEPRECATED apObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

instance Core.FromJSON AttachPolicy where
  toJSON AttachPolicy {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("PolicyReference" Core..= policyReference),
            Core.Just ("ObjectReference" Core..= objectReference)
          ]
      )

instance Core.AWSRequest AttachPolicy where
  type Rs AttachPolicy = AttachPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath "/amazonclouddirectory/2017-01-11/policy/attach",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "x-amz-data-partition" directoryArn,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          AttachPolicyResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAttachPolicyResponse' smart constructor.
newtype AttachPolicyResponse = AttachPolicyResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AttachPolicyResponse' value with any optional fields omitted.
mkAttachPolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AttachPolicyResponse
mkAttachPolicyResponse responseStatus =
  AttachPolicyResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aprrsResponseStatus :: Lens.Lens' AttachPolicyResponse Core.Int
aprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED aprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
