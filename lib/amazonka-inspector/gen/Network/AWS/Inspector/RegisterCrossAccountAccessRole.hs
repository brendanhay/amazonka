{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      RegisterCrossAccountAccessRole (..)
    , mkRegisterCrossAccountAccessRole
    -- ** Request lenses
    , rcaarRoleArn

    -- * Destructuring the response
    , RegisterCrossAccountAccessRoleResponse (..)
    , mkRegisterCrossAccountAccessRoleResponse
    ) where

import qualified Network.AWS.Inspector.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRegisterCrossAccountAccessRole' smart constructor.
newtype RegisterCrossAccountAccessRole = RegisterCrossAccountAccessRole'
  { roleArn :: Types.RoleArn
    -- ^ The ARN of the IAM role that grants Amazon Inspector access to AWS Services needed to perform security assessments. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterCrossAccountAccessRole' value with any optional fields omitted.
mkRegisterCrossAccountAccessRole
    :: Types.RoleArn -- ^ 'roleArn'
    -> RegisterCrossAccountAccessRole
mkRegisterCrossAccountAccessRole roleArn
  = RegisterCrossAccountAccessRole'{roleArn}

-- | The ARN of the IAM role that grants Amazon Inspector access to AWS Services needed to perform security assessments. 
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcaarRoleArn :: Lens.Lens' RegisterCrossAccountAccessRole Types.RoleArn
rcaarRoleArn = Lens.field @"roleArn"
{-# INLINEABLE rcaarRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

instance Core.ToQuery RegisterCrossAccountAccessRole where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RegisterCrossAccountAccessRole where
        toHeaders RegisterCrossAccountAccessRole{..}
          = Core.pure
              ("X-Amz-Target", "InspectorService.RegisterCrossAccountAccessRole")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RegisterCrossAccountAccessRole where
        toJSON RegisterCrossAccountAccessRole{..}
          = Core.object
              (Core.catMaybes [Core.Just ("roleArn" Core..= roleArn)])

instance Core.AWSRequest RegisterCrossAccountAccessRole where
        type Rs RegisterCrossAccountAccessRole =
             RegisterCrossAccountAccessRoleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull RegisterCrossAccountAccessRoleResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRegisterCrossAccountAccessRoleResponse' smart constructor.
data RegisterCrossAccountAccessRoleResponse = RegisterCrossAccountAccessRoleResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterCrossAccountAccessRoleResponse' value with any optional fields omitted.
mkRegisterCrossAccountAccessRoleResponse
    :: RegisterCrossAccountAccessRoleResponse
mkRegisterCrossAccountAccessRoleResponse
  = RegisterCrossAccountAccessRoleResponse'
