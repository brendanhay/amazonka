{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.CreateDeployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deploys an application revision through the specified deployment group.
module Network.AWS.CodeDeploy.CreateDeployment
    (
    -- * Creating a request
      CreateDeployment (..)
    , mkCreateDeployment
    -- ** Request lenses
    , cdApplicationName
    , cdAutoRollbackConfiguration
    , cdDeploymentConfigName
    , cdDeploymentGroupName
    , cdDescription
    , cdFileExistsBehavior
    , cdIgnoreApplicationStopFailures
    , cdRevision
    , cdTargetInstances
    , cdUpdateOutdatedInstancesOnly

    -- * Destructuring the response
    , CreateDeploymentResponse (..)
    , mkCreateDeploymentResponse
    -- ** Response lenses
    , cdrrsDeploymentId
    , cdrrsResponseStatus
    ) where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @CreateDeployment@ operation.
--
-- /See:/ 'mkCreateDeployment' smart constructor.
data CreateDeployment = CreateDeployment'
  { applicationName :: Types.ApplicationName
    -- ^ The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
  , autoRollbackConfiguration :: Core.Maybe Types.AutoRollbackConfiguration
    -- ^ Configuration information for an automatic rollback that is added when a deployment is created.
  , deploymentConfigName :: Core.Maybe Types.DeploymentConfigName
    -- ^ The name of a deployment configuration associated with the IAM user or AWS account.
--
-- If not specified, the value configured in the deployment group is used as the default. If the deployment group does not have a deployment configuration associated with it, @CodeDeployDefault@ .@OneAtATime@ is used by default.
  , deploymentGroupName :: Core.Maybe Types.DeploymentGroupName
    -- ^ The name of the deployment group.
  , description :: Core.Maybe Types.Description
    -- ^ A comment about the deployment.
  , fileExistsBehavior :: Core.Maybe Types.FileExistsBehavior
    -- ^ Information about how AWS CodeDeploy handles files that already exist in a deployment target location but weren't part of the previous successful deployment.
--
-- The @fileExistsBehavior@ parameter takes any of the following values:
--
--     * DISALLOW: The deployment fails. This is also the default behavior if no option is specified.
--
--
--     * OVERWRITE: The version of the file from the application revision currently being deployed replaces the version already on the instance.
--
--
--     * RETAIN: The version of the file already on the instance is kept and used as part of the new deployment.
--
--
  , ignoreApplicationStopFailures :: Core.Maybe Core.Bool
    -- ^ If true, then if an @ApplicationStop@ , @BeforeBlockTraffic@ , or @AfterBlockTraffic@ deployment lifecycle event to an instance fails, then the deployment continues to the next deployment lifecycle event. For example, if @ApplicationStop@ fails, the deployment continues with @DownloadBundle@ . If @BeforeBlockTraffic@ fails, the deployment continues with @BlockTraffic@ . If @AfterBlockTraffic@ fails, the deployment continues with @ApplicationStop@ . 
--
-- If false or not specified, then if a lifecycle event fails during a deployment to an instance, that deployment fails. If deployment to that instance is part of an overall deployment and the number of healthy hosts is not less than the minimum number of healthy hosts, then a deployment to the next instance is attempted. 
-- During a deployment, the AWS CodeDeploy agent runs the scripts specified for @ApplicationStop@ , @BeforeBlockTraffic@ , and @AfterBlockTraffic@ in the AppSpec file from the previous successful deployment. (All other scripts are run from the AppSpec file in the current deployment.) If one of these scripts contains an error and does not run successfully, the deployment can fail. 
-- If the cause of the failure is a script from the last successful deployment that will never run successfully, create a new deployment and use @ignoreApplicationStopFailures@ to specify that the @ApplicationStop@ , @BeforeBlockTraffic@ , and @AfterBlockTraffic@ failures should be ignored. 
  , revision :: Core.Maybe Types.RevisionLocation
    -- ^ The type and location of the revision to deploy. 
  , targetInstances :: Core.Maybe Types.TargetInstances
    -- ^ Information about the instances that belong to the replacement environment in a blue/green deployment. 
  , updateOutdatedInstancesOnly :: Core.Maybe Core.Bool
    -- ^ Indicates whether to deploy to all instances or only to instances that are not running the latest application revision. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDeployment' value with any optional fields omitted.
mkCreateDeployment
    :: Types.ApplicationName -- ^ 'applicationName'
    -> CreateDeployment
mkCreateDeployment applicationName
  = CreateDeployment'{applicationName,
                      autoRollbackConfiguration = Core.Nothing,
                      deploymentConfigName = Core.Nothing,
                      deploymentGroupName = Core.Nothing, description = Core.Nothing,
                      fileExistsBehavior = Core.Nothing,
                      ignoreApplicationStopFailures = Core.Nothing,
                      revision = Core.Nothing, targetInstances = Core.Nothing,
                      updateOutdatedInstancesOnly = Core.Nothing}

-- | The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdApplicationName :: Lens.Lens' CreateDeployment Types.ApplicationName
cdApplicationName = Lens.field @"applicationName"
{-# INLINEABLE cdApplicationName #-}
{-# DEPRECATED applicationName "Use generic-lens or generic-optics with 'applicationName' instead"  #-}

-- | Configuration information for an automatic rollback that is added when a deployment is created.
--
-- /Note:/ Consider using 'autoRollbackConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdAutoRollbackConfiguration :: Lens.Lens' CreateDeployment (Core.Maybe Types.AutoRollbackConfiguration)
cdAutoRollbackConfiguration = Lens.field @"autoRollbackConfiguration"
{-# INLINEABLE cdAutoRollbackConfiguration #-}
{-# DEPRECATED autoRollbackConfiguration "Use generic-lens or generic-optics with 'autoRollbackConfiguration' instead"  #-}

-- | The name of a deployment configuration associated with the IAM user or AWS account.
--
-- If not specified, the value configured in the deployment group is used as the default. If the deployment group does not have a deployment configuration associated with it, @CodeDeployDefault@ .@OneAtATime@ is used by default.
--
-- /Note:/ Consider using 'deploymentConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDeploymentConfigName :: Lens.Lens' CreateDeployment (Core.Maybe Types.DeploymentConfigName)
cdDeploymentConfigName = Lens.field @"deploymentConfigName"
{-# INLINEABLE cdDeploymentConfigName #-}
{-# DEPRECATED deploymentConfigName "Use generic-lens or generic-optics with 'deploymentConfigName' instead"  #-}

-- | The name of the deployment group.
--
-- /Note:/ Consider using 'deploymentGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDeploymentGroupName :: Lens.Lens' CreateDeployment (Core.Maybe Types.DeploymentGroupName)
cdDeploymentGroupName = Lens.field @"deploymentGroupName"
{-# INLINEABLE cdDeploymentGroupName #-}
{-# DEPRECATED deploymentGroupName "Use generic-lens or generic-optics with 'deploymentGroupName' instead"  #-}

-- | A comment about the deployment.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDescription :: Lens.Lens' CreateDeployment (Core.Maybe Types.Description)
cdDescription = Lens.field @"description"
{-# INLINEABLE cdDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Information about how AWS CodeDeploy handles files that already exist in a deployment target location but weren't part of the previous successful deployment.
--
-- The @fileExistsBehavior@ parameter takes any of the following values:
--
--     * DISALLOW: The deployment fails. This is also the default behavior if no option is specified.
--
--
--     * OVERWRITE: The version of the file from the application revision currently being deployed replaces the version already on the instance.
--
--
--     * RETAIN: The version of the file already on the instance is kept and used as part of the new deployment.
--
--
--
-- /Note:/ Consider using 'fileExistsBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdFileExistsBehavior :: Lens.Lens' CreateDeployment (Core.Maybe Types.FileExistsBehavior)
cdFileExistsBehavior = Lens.field @"fileExistsBehavior"
{-# INLINEABLE cdFileExistsBehavior #-}
{-# DEPRECATED fileExistsBehavior "Use generic-lens or generic-optics with 'fileExistsBehavior' instead"  #-}

-- | If true, then if an @ApplicationStop@ , @BeforeBlockTraffic@ , or @AfterBlockTraffic@ deployment lifecycle event to an instance fails, then the deployment continues to the next deployment lifecycle event. For example, if @ApplicationStop@ fails, the deployment continues with @DownloadBundle@ . If @BeforeBlockTraffic@ fails, the deployment continues with @BlockTraffic@ . If @AfterBlockTraffic@ fails, the deployment continues with @ApplicationStop@ . 
--
-- If false or not specified, then if a lifecycle event fails during a deployment to an instance, that deployment fails. If deployment to that instance is part of an overall deployment and the number of healthy hosts is not less than the minimum number of healthy hosts, then a deployment to the next instance is attempted. 
-- During a deployment, the AWS CodeDeploy agent runs the scripts specified for @ApplicationStop@ , @BeforeBlockTraffic@ , and @AfterBlockTraffic@ in the AppSpec file from the previous successful deployment. (All other scripts are run from the AppSpec file in the current deployment.) If one of these scripts contains an error and does not run successfully, the deployment can fail. 
-- If the cause of the failure is a script from the last successful deployment that will never run successfully, create a new deployment and use @ignoreApplicationStopFailures@ to specify that the @ApplicationStop@ , @BeforeBlockTraffic@ , and @AfterBlockTraffic@ failures should be ignored. 
--
-- /Note:/ Consider using 'ignoreApplicationStopFailures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdIgnoreApplicationStopFailures :: Lens.Lens' CreateDeployment (Core.Maybe Core.Bool)
cdIgnoreApplicationStopFailures = Lens.field @"ignoreApplicationStopFailures"
{-# INLINEABLE cdIgnoreApplicationStopFailures #-}
{-# DEPRECATED ignoreApplicationStopFailures "Use generic-lens or generic-optics with 'ignoreApplicationStopFailures' instead"  #-}

-- | The type and location of the revision to deploy. 
--
-- /Note:/ Consider using 'revision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdRevision :: Lens.Lens' CreateDeployment (Core.Maybe Types.RevisionLocation)
cdRevision = Lens.field @"revision"
{-# INLINEABLE cdRevision #-}
{-# DEPRECATED revision "Use generic-lens or generic-optics with 'revision' instead"  #-}

-- | Information about the instances that belong to the replacement environment in a blue/green deployment. 
--
-- /Note:/ Consider using 'targetInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdTargetInstances :: Lens.Lens' CreateDeployment (Core.Maybe Types.TargetInstances)
cdTargetInstances = Lens.field @"targetInstances"
{-# INLINEABLE cdTargetInstances #-}
{-# DEPRECATED targetInstances "Use generic-lens or generic-optics with 'targetInstances' instead"  #-}

-- | Indicates whether to deploy to all instances or only to instances that are not running the latest application revision. 
--
-- /Note:/ Consider using 'updateOutdatedInstancesOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdUpdateOutdatedInstancesOnly :: Lens.Lens' CreateDeployment (Core.Maybe Core.Bool)
cdUpdateOutdatedInstancesOnly = Lens.field @"updateOutdatedInstancesOnly"
{-# INLINEABLE cdUpdateOutdatedInstancesOnly #-}
{-# DEPRECATED updateOutdatedInstancesOnly "Use generic-lens or generic-optics with 'updateOutdatedInstancesOnly' instead"  #-}

instance Core.ToQuery CreateDeployment where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateDeployment where
        toHeaders CreateDeployment{..}
          = Core.pure
              ("X-Amz-Target", "CodeDeploy_20141006.CreateDeployment")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateDeployment where
        toJSON CreateDeployment{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("applicationName" Core..= applicationName),
                  ("autoRollbackConfiguration" Core..=) Core.<$>
                    autoRollbackConfiguration,
                  ("deploymentConfigName" Core..=) Core.<$> deploymentConfigName,
                  ("deploymentGroupName" Core..=) Core.<$> deploymentGroupName,
                  ("description" Core..=) Core.<$> description,
                  ("fileExistsBehavior" Core..=) Core.<$> fileExistsBehavior,
                  ("ignoreApplicationStopFailures" Core..=) Core.<$>
                    ignoreApplicationStopFailures,
                  ("revision" Core..=) Core.<$> revision,
                  ("targetInstances" Core..=) Core.<$> targetInstances,
                  ("updateOutdatedInstancesOnly" Core..=) Core.<$>
                    updateOutdatedInstancesOnly])

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
                   (x Core..:? "deploymentId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a @CreateDeployment@ operation. 
--
-- /See:/ 'mkCreateDeploymentResponse' smart constructor.
data CreateDeploymentResponse = CreateDeploymentResponse'
  { deploymentId :: Core.Maybe Types.DeploymentId
    -- ^ The unique ID of a deployment. 
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

-- | The unique ID of a deployment. 
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsDeploymentId :: Lens.Lens' CreateDeploymentResponse (Core.Maybe Types.DeploymentId)
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
