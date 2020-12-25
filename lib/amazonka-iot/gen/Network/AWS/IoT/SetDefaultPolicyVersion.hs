{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.SetDefaultPolicyVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the specified version of the specified policy as the policy's default (operative) version. This action affects all certificates to which the policy is attached. To list the principals the policy is attached to, use the ListPrincipalPolicy API.
module Network.AWS.IoT.SetDefaultPolicyVersion
  ( -- * Creating a request
    SetDefaultPolicyVersion (..),
    mkSetDefaultPolicyVersion,

    -- ** Request lenses
    sdpvPolicyName,
    sdpvPolicyVersionId,

    -- * Destructuring the response
    SetDefaultPolicyVersionResponse (..),
    mkSetDefaultPolicyVersionResponse,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the SetDefaultPolicyVersion operation.
--
-- /See:/ 'mkSetDefaultPolicyVersion' smart constructor.
data SetDefaultPolicyVersion = SetDefaultPolicyVersion'
  { -- | The policy name.
    policyName :: Types.PolicyName,
    -- | The policy version ID.
    policyVersionId :: Types.PolicyVersionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetDefaultPolicyVersion' value with any optional fields omitted.
mkSetDefaultPolicyVersion ::
  -- | 'policyName'
  Types.PolicyName ->
  -- | 'policyVersionId'
  Types.PolicyVersionId ->
  SetDefaultPolicyVersion
mkSetDefaultPolicyVersion policyName policyVersionId =
  SetDefaultPolicyVersion' {policyName, policyVersionId}

-- | The policy name.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdpvPolicyName :: Lens.Lens' SetDefaultPolicyVersion Types.PolicyName
sdpvPolicyName = Lens.field @"policyName"
{-# DEPRECATED sdpvPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The policy version ID.
--
-- /Note:/ Consider using 'policyVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdpvPolicyVersionId :: Lens.Lens' SetDefaultPolicyVersion Types.PolicyVersionId
sdpvPolicyVersionId = Lens.field @"policyVersionId"
{-# DEPRECATED sdpvPolicyVersionId "Use generic-lens or generic-optics with 'policyVersionId' instead." #-}

instance Core.FromJSON SetDefaultPolicyVersion where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest SetDefaultPolicyVersion where
  type Rs SetDefaultPolicyVersion = SetDefaultPolicyVersionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PATCH,
        Core._rqPath =
          Core.rawPath
            ( "/policies/" Core.<> (Core.toText policyName)
                Core.<> ("/version/")
                Core.<> (Core.toText policyVersionId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull SetDefaultPolicyVersionResponse'

-- | /See:/ 'mkSetDefaultPolicyVersionResponse' smart constructor.
data SetDefaultPolicyVersionResponse = SetDefaultPolicyVersionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetDefaultPolicyVersionResponse' value with any optional fields omitted.
mkSetDefaultPolicyVersionResponse ::
  SetDefaultPolicyVersionResponse
mkSetDefaultPolicyVersionResponse =
  SetDefaultPolicyVersionResponse'
