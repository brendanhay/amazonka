{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.RegisterOnPremisesInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an on-premises instance.
module Network.AWS.CodeDeploy.RegisterOnPremisesInstance
  ( -- * Creating a request
    RegisterOnPremisesInstance (..),
    mkRegisterOnPremisesInstance,

    -- ** Request lenses
    ropiInstanceName,
    ropiIamSessionArn,
    ropiIamUserArn,

    -- * Destructuring the response
    RegisterOnPremisesInstanceResponse (..),
    mkRegisterOnPremisesInstanceResponse,
  )
where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of the register on-premises instance operation.
--
-- /See:/ 'mkRegisterOnPremisesInstance' smart constructor.
data RegisterOnPremisesInstance = RegisterOnPremisesInstance'
  { -- | The name of the on-premises instance to register.
    instanceName :: Types.InstanceName,
    -- | The ARN of the IAM session to associate with the on-premises instance.
    iamSessionArn :: Core.Maybe Types.IamSessionArn,
    -- | The ARN of the IAM user to associate with the on-premises instance.
    iamUserArn :: Core.Maybe Types.IamUserArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterOnPremisesInstance' value with any optional fields omitted.
mkRegisterOnPremisesInstance ::
  -- | 'instanceName'
  Types.InstanceName ->
  RegisterOnPremisesInstance
mkRegisterOnPremisesInstance instanceName =
  RegisterOnPremisesInstance'
    { instanceName,
      iamSessionArn = Core.Nothing,
      iamUserArn = Core.Nothing
    }

-- | The name of the on-premises instance to register.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ropiInstanceName :: Lens.Lens' RegisterOnPremisesInstance Types.InstanceName
ropiInstanceName = Lens.field @"instanceName"
{-# DEPRECATED ropiInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

-- | The ARN of the IAM session to associate with the on-premises instance.
--
-- /Note:/ Consider using 'iamSessionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ropiIamSessionArn :: Lens.Lens' RegisterOnPremisesInstance (Core.Maybe Types.IamSessionArn)
ropiIamSessionArn = Lens.field @"iamSessionArn"
{-# DEPRECATED ropiIamSessionArn "Use generic-lens or generic-optics with 'iamSessionArn' instead." #-}

-- | The ARN of the IAM user to associate with the on-premises instance.
--
-- /Note:/ Consider using 'iamUserArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ropiIamUserArn :: Lens.Lens' RegisterOnPremisesInstance (Core.Maybe Types.IamUserArn)
ropiIamUserArn = Lens.field @"iamUserArn"
{-# DEPRECATED ropiIamUserArn "Use generic-lens or generic-optics with 'iamUserArn' instead." #-}

instance Core.FromJSON RegisterOnPremisesInstance where
  toJSON RegisterOnPremisesInstance {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("instanceName" Core..= instanceName),
            ("iamSessionArn" Core..=) Core.<$> iamSessionArn,
            ("iamUserArn" Core..=) Core.<$> iamUserArn
          ]
      )

instance Core.AWSRequest RegisterOnPremisesInstance where
  type
    Rs RegisterOnPremisesInstance =
      RegisterOnPremisesInstanceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeDeploy_20141006.RegisterOnPremisesInstance")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull RegisterOnPremisesInstanceResponse'

-- | /See:/ 'mkRegisterOnPremisesInstanceResponse' smart constructor.
data RegisterOnPremisesInstanceResponse = RegisterOnPremisesInstanceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterOnPremisesInstanceResponse' value with any optional fields omitted.
mkRegisterOnPremisesInstanceResponse ::
  RegisterOnPremisesInstanceResponse
mkRegisterOnPremisesInstanceResponse =
  RegisterOnPremisesInstanceResponse'
