{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeletePolicyVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified version of the specified policy. You cannot delete the default version of a policy using this API. To delete the default version of a policy, use 'DeletePolicy' . To find out which version of a policy is marked as the default version, use ListPolicyVersions.
module Network.AWS.IoT.DeletePolicyVersion
  ( -- * Creating a request
    DeletePolicyVersion (..),
    mkDeletePolicyVersion,

    -- ** Request lenses
    dpvPolicyName,
    dpvPolicyVersionId,

    -- * Destructuring the response
    DeletePolicyVersionResponse (..),
    mkDeletePolicyVersionResponse,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DeletePolicyVersion operation.
--
-- /See:/ 'mkDeletePolicyVersion' smart constructor.
data DeletePolicyVersion = DeletePolicyVersion'
  { -- | The name of the policy.
    policyName :: Types.PolicyName,
    -- | The policy version ID.
    policyVersionId :: Types.PolicyVersionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePolicyVersion' value with any optional fields omitted.
mkDeletePolicyVersion ::
  -- | 'policyName'
  Types.PolicyName ->
  -- | 'policyVersionId'
  Types.PolicyVersionId ->
  DeletePolicyVersion
mkDeletePolicyVersion policyName policyVersionId =
  DeletePolicyVersion' {policyName, policyVersionId}

-- | The name of the policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvPolicyName :: Lens.Lens' DeletePolicyVersion Types.PolicyName
dpvPolicyName = Lens.field @"policyName"
{-# DEPRECATED dpvPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The policy version ID.
--
-- /Note:/ Consider using 'policyVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvPolicyVersionId :: Lens.Lens' DeletePolicyVersion Types.PolicyVersionId
dpvPolicyVersionId = Lens.field @"policyVersionId"
{-# DEPRECATED dpvPolicyVersionId "Use generic-lens or generic-optics with 'policyVersionId' instead." #-}

instance Core.AWSRequest DeletePolicyVersion where
  type Rs DeletePolicyVersion = DeletePolicyVersionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/policies/" Core.<> (Core.toText policyName)
                Core.<> ("/version/")
                Core.<> (Core.toText policyVersionId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response = Response.receiveNull DeletePolicyVersionResponse'

-- | /See:/ 'mkDeletePolicyVersionResponse' smart constructor.
data DeletePolicyVersionResponse = DeletePolicyVersionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePolicyVersionResponse' value with any optional fields omitted.
mkDeletePolicyVersionResponse ::
  DeletePolicyVersionResponse
mkDeletePolicyVersionResponse = DeletePolicyVersionResponse'
