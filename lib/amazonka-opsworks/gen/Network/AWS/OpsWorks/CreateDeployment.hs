{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.CreateDeployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs deployment or stack commands. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-deploying.html Deploying Apps> and <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-commands.html Run Stack Commands> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Deploy or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.CreateDeployment
    (
    -- * Creating a request
      CreateDeployment (..)
    , mkCreateDeployment
    -- ** Request lenses
    , cdStackId
    , cdCommand
    , cdAppId
    , cdComment
    , cdCustomJson
    , cdInstanceIds
    , cdLayerIds

    -- * Destructuring the response
    , CreateDeploymentResponse (..)
    , mkCreateDeploymentResponse
    -- ** Response lenses
    , cdrrsDeploymentId
    , cdrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDeployment' smart constructor.
data CreateDeployment = CreateDeployment'
  { stackId :: Core.Text
    -- ^ The stack ID.
  , command :: Types.DeploymentCommand
    -- ^ A @DeploymentCommand@ object that specifies the deployment command and any associated arguments.
  , appId :: Core.Maybe Core.Text
    -- ^ The app ID. This parameter is required for app deployments, but not for other deployment commands.
  , comment :: Core.Maybe Core.Text
    -- ^ A user-defined comment.
  , customJson :: Core.Maybe Core.Text
    -- ^ A string that contains user-defined, custom JSON. You can use this parameter to override some corresponding default stack configuration JSON values. The string should be in the following format:
--
-- @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@ 
-- For more information about custom JSON, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes> and <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-json-override.html Overriding Attributes With Custom JSON> .
  , instanceIds :: Core.Maybe [Core.Text]
    -- ^ The instance IDs for the deployment targets.
  , layerIds :: Core.Maybe [Core.Text]
    -- ^ The layer IDs for the deployment targets.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDeployment' value with any optional fields omitted.
mkCreateDeployment
    :: Core.Text -- ^ 'stackId'
    -> Types.DeploymentCommand -- ^ 'command'
    -> CreateDeployment
mkCreateDeployment stackId command
  = CreateDeployment'{stackId, command, appId = Core.Nothing,
                      comment = Core.Nothing, customJson = Core.Nothing,
                      instanceIds = Core.Nothing, layerIds = Core.Nothing}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdStackId :: Lens.Lens' CreateDeployment Core.Text
cdStackId = Lens.field @"stackId"
{-# INLINEABLE cdStackId #-}
{-# DEPRECATED stackId "Use generic-lens or generic-optics with 'stackId' instead"  #-}

-- | A @DeploymentCommand@ object that specifies the deployment command and any associated arguments.
--
-- /Note:/ Consider using 'command' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCommand :: Lens.Lens' CreateDeployment Types.DeploymentCommand
cdCommand = Lens.field @"command"
{-# INLINEABLE cdCommand #-}
{-# DEPRECATED command "Use generic-lens or generic-optics with 'command' instead"  #-}

-- | The app ID. This parameter is required for app deployments, but not for other deployment commands.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdAppId :: Lens.Lens' CreateDeployment (Core.Maybe Core.Text)
cdAppId = Lens.field @"appId"
{-# INLINEABLE cdAppId #-}
{-# DEPRECATED appId "Use generic-lens or generic-optics with 'appId' instead"  #-}

-- | A user-defined comment.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdComment :: Lens.Lens' CreateDeployment (Core.Maybe Core.Text)
cdComment = Lens.field @"comment"
{-# INLINEABLE cdComment #-}
{-# DEPRECATED comment "Use generic-lens or generic-optics with 'comment' instead"  #-}

-- | A string that contains user-defined, custom JSON. You can use this parameter to override some corresponding default stack configuration JSON values. The string should be in the following format:
--
-- @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@ 
-- For more information about custom JSON, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes> and <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-json-override.html Overriding Attributes With Custom JSON> .
--
-- /Note:/ Consider using 'customJson' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCustomJson :: Lens.Lens' CreateDeployment (Core.Maybe Core.Text)
cdCustomJson = Lens.field @"customJson"
{-# INLINEABLE cdCustomJson #-}
{-# DEPRECATED customJson "Use generic-lens or generic-optics with 'customJson' instead"  #-}

-- | The instance IDs for the deployment targets.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdInstanceIds :: Lens.Lens' CreateDeployment (Core.Maybe [Core.Text])
cdInstanceIds = Lens.field @"instanceIds"
{-# INLINEABLE cdInstanceIds #-}
{-# DEPRECATED instanceIds "Use generic-lens or generic-optics with 'instanceIds' instead"  #-}

-- | The layer IDs for the deployment targets.
--
-- /Note:/ Consider using 'layerIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdLayerIds :: Lens.Lens' CreateDeployment (Core.Maybe [Core.Text])
cdLayerIds = Lens.field @"layerIds"
{-# INLINEABLE cdLayerIds #-}
{-# DEPRECATED layerIds "Use generic-lens or generic-optics with 'layerIds' instead"  #-}

instance Core.ToQuery CreateDeployment where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateDeployment where
        toHeaders CreateDeployment{..}
          = Core.pure ("X-Amz-Target", "OpsWorks_20130218.CreateDeployment")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateDeployment where
        toJSON CreateDeployment{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("StackId" Core..= stackId),
                  Core.Just ("Command" Core..= command),
                  ("AppId" Core..=) Core.<$> appId,
                  ("Comment" Core..=) Core.<$> comment,
                  ("CustomJson" Core..=) Core.<$> customJson,
                  ("InstanceIds" Core..=) Core.<$> instanceIds,
                  ("LayerIds" Core..=) Core.<$> layerIds])

instance Core.AWSRequest CreateDeployment where
        type Rs CreateDeployment = CreateDeploymentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateDeploymentResponse' Core.<$>
                   (x Core..:? "DeploymentId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a @CreateDeployment@ request.
--
-- /See:/ 'mkCreateDeploymentResponse' smart constructor.
data CreateDeploymentResponse = CreateDeploymentResponse'
  { deploymentId :: Core.Maybe Core.Text
    -- ^ The deployment ID, which can be used with other requests to identify the deployment.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDeploymentResponse' value with any optional fields omitted.
mkCreateDeploymentResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateDeploymentResponse
mkCreateDeploymentResponse responseStatus
  = CreateDeploymentResponse'{deploymentId = Core.Nothing,
                              responseStatus}

-- | The deployment ID, which can be used with other requests to identify the deployment.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsDeploymentId :: Lens.Lens' CreateDeploymentResponse (Core.Maybe Core.Text)
cdrrsDeploymentId = Lens.field @"deploymentId"
{-# INLINEABLE cdrrsDeploymentId #-}
{-# DEPRECATED deploymentId "Use generic-lens or generic-optics with 'deploymentId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsResponseStatus :: Lens.Lens' CreateDeploymentResponse Core.Int
cdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
