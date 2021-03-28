{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.DeleteDeploymentConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a deployment configuration.
module Network.AWS.CodeDeploy.DeleteDeploymentConfig
    (
    -- * Creating a request
      DeleteDeploymentConfig (..)
    , mkDeleteDeploymentConfig
    -- ** Request lenses
    , ddcDeploymentConfigName

    -- * Destructuring the response
    , DeleteDeploymentConfigResponse (..)
    , mkDeleteDeploymentConfigResponse
    ) where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DeleteDeploymentConfig@ operation.
--
-- /See:/ 'mkDeleteDeploymentConfig' smart constructor.
newtype DeleteDeploymentConfig = DeleteDeploymentConfig'
  { deploymentConfigName :: Types.DeploymentConfigName
    -- ^ The name of a deployment configuration associated with the IAM user or AWS account.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDeploymentConfig' value with any optional fields omitted.
mkDeleteDeploymentConfig
    :: Types.DeploymentConfigName -- ^ 'deploymentConfigName'
    -> DeleteDeploymentConfig
mkDeleteDeploymentConfig deploymentConfigName
  = DeleteDeploymentConfig'{deploymentConfigName}

-- | The name of a deployment configuration associated with the IAM user or AWS account.
--
-- /Note:/ Consider using 'deploymentConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcDeploymentConfigName :: Lens.Lens' DeleteDeploymentConfig Types.DeploymentConfigName
ddcDeploymentConfigName = Lens.field @"deploymentConfigName"
{-# INLINEABLE ddcDeploymentConfigName #-}
{-# DEPRECATED deploymentConfigName "Use generic-lens or generic-optics with 'deploymentConfigName' instead"  #-}

instance Core.ToQuery DeleteDeploymentConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteDeploymentConfig where
        toHeaders DeleteDeploymentConfig{..}
          = Core.pure
              ("X-Amz-Target", "CodeDeploy_20141006.DeleteDeploymentConfig")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteDeploymentConfig where
        toJSON DeleteDeploymentConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("deploymentConfigName" Core..= deploymentConfigName)])

instance Core.AWSRequest DeleteDeploymentConfig where
        type Rs DeleteDeploymentConfig = DeleteDeploymentConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DeleteDeploymentConfigResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteDeploymentConfigResponse' smart constructor.
data DeleteDeploymentConfigResponse = DeleteDeploymentConfigResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDeploymentConfigResponse' value with any optional fields omitted.
mkDeleteDeploymentConfigResponse
    :: DeleteDeploymentConfigResponse
mkDeleteDeploymentConfigResponse = DeleteDeploymentConfigResponse'
