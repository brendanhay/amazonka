{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.RegisterCrossAccountAccessRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers the IAM role that grants Amazon Inspector access to AWS Services needed to perform security assessments.
module Network.AWS.Inspector.RegisterCrossAccountAccessRole
  ( -- * Creating a request
    RegisterCrossAccountAccessRole (..),
    mkRegisterCrossAccountAccessRole,

    -- ** Request lenses
    rcaarRoleArn,

    -- * Destructuring the response
    RegisterCrossAccountAccessRoleResponse (..),
    mkRegisterCrossAccountAccessRoleResponse,
  )
where

import qualified Network.AWS.Inspector.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRegisterCrossAccountAccessRole' smart constructor.
newtype RegisterCrossAccountAccessRole = RegisterCrossAccountAccessRole'
  { -- | The ARN of the IAM role that grants Amazon Inspector access to AWS Services needed to perform security assessments.
    roleArn :: Types.RoleArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterCrossAccountAccessRole' value with any optional fields omitted.
mkRegisterCrossAccountAccessRole ::
  -- | 'roleArn'
  Types.RoleArn ->
  RegisterCrossAccountAccessRole
mkRegisterCrossAccountAccessRole roleArn =
  RegisterCrossAccountAccessRole' {roleArn}

-- | The ARN of the IAM role that grants Amazon Inspector access to AWS Services needed to perform security assessments.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcaarRoleArn :: Lens.Lens' RegisterCrossAccountAccessRole Types.RoleArn
rcaarRoleArn = Lens.field @"roleArn"
{-# DEPRECATED rcaarRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

instance Core.FromJSON RegisterCrossAccountAccessRole where
  toJSON RegisterCrossAccountAccessRole {..} =
    Core.object
      (Core.catMaybes [Core.Just ("roleArn" Core..= roleArn)])

instance Core.AWSRequest RegisterCrossAccountAccessRole where
  type
    Rs RegisterCrossAccountAccessRole =
      RegisterCrossAccountAccessRoleResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "InspectorService.RegisterCrossAccountAccessRole")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveNull RegisterCrossAccountAccessRoleResponse'

-- | /See:/ 'mkRegisterCrossAccountAccessRoleResponse' smart constructor.
data RegisterCrossAccountAccessRoleResponse = RegisterCrossAccountAccessRoleResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterCrossAccountAccessRoleResponse' value with any optional fields omitted.
mkRegisterCrossAccountAccessRoleResponse ::
  RegisterCrossAccountAccessRoleResponse
mkRegisterCrossAccountAccessRoleResponse =
  RegisterCrossAccountAccessRoleResponse'
