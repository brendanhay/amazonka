{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateDeployment (..),
    mkCreateDeployment,

    -- ** Request lenses
    cdDeploymentConfigName,
    cdFileExistsBehavior,
    cdTargetInstances,
    cdRevision,
    cdDescription,
    cdAutoRollbackConfiguration,
    cdUpdateOutdatedInstancesOnly,
    cdDeploymentGroupName,
    cdIgnoreApplicationStopFailures,
    cdApplicationName,

    -- * Destructuring the response
    CreateDeploymentResponse (..),
    mkCreateDeploymentResponse,

    -- ** Response lenses
    cdrsDeploymentId,
    cdrsResponseStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @CreateDeployment@ operation.
--
-- /See:/ 'mkCreateDeployment' smart constructor.
data CreateDeployment = CreateDeployment'
  { deploymentConfigName ::
      Lude.Maybe Lude.Text,
    fileExistsBehavior :: Lude.Maybe FileExistsBehavior,
    targetInstances :: Lude.Maybe TargetInstances,
    revision :: Lude.Maybe RevisionLocation,
    description :: Lude.Maybe Lude.Text,
    autoRollbackConfiguration ::
      Lude.Maybe AutoRollbackConfiguration,
    updateOutdatedInstancesOnly :: Lude.Maybe Lude.Bool,
    deploymentGroupName :: Lude.Maybe Lude.Text,
    ignoreApplicationStopFailures :: Lude.Maybe Lude.Bool,
    applicationName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDeployment' with the minimum fields required to make a request.
--
-- * 'applicationName' - The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
-- * 'autoRollbackConfiguration' - Configuration information for an automatic rollback that is added when a deployment is created.
-- * 'deploymentConfigName' - The name of a deployment configuration associated with the IAM user or AWS account.
--
-- If not specified, the value configured in the deployment group is used as the default. If the deployment group does not have a deployment configuration associated with it, @CodeDeployDefault@ .@OneAtATime@ is used by default.
-- * 'deploymentGroupName' - The name of the deployment group.
-- * 'description' - A comment about the deployment.
-- * 'fileExistsBehavior' - Information about how AWS CodeDeploy handles files that already exist in a deployment target location but weren't part of the previous successful deployment.
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
-- * 'ignoreApplicationStopFailures' - If true, then if an @ApplicationStop@ , @BeforeBlockTraffic@ , or @AfterBlockTraffic@ deployment lifecycle event to an instance fails, then the deployment continues to the next deployment lifecycle event. For example, if @ApplicationStop@ fails, the deployment continues with @DownloadBundle@ . If @BeforeBlockTraffic@ fails, the deployment continues with @BlockTraffic@ . If @AfterBlockTraffic@ fails, the deployment continues with @ApplicationStop@ .
--
-- If false or not specified, then if a lifecycle event fails during a deployment to an instance, that deployment fails. If deployment to that instance is part of an overall deployment and the number of healthy hosts is not less than the minimum number of healthy hosts, then a deployment to the next instance is attempted.
-- During a deployment, the AWS CodeDeploy agent runs the scripts specified for @ApplicationStop@ , @BeforeBlockTraffic@ , and @AfterBlockTraffic@ in the AppSpec file from the previous successful deployment. (All other scripts are run from the AppSpec file in the current deployment.) If one of these scripts contains an error and does not run successfully, the deployment can fail.
-- If the cause of the failure is a script from the last successful deployment that will never run successfully, create a new deployment and use @ignoreApplicationStopFailures@ to specify that the @ApplicationStop@ , @BeforeBlockTraffic@ , and @AfterBlockTraffic@ failures should be ignored.
-- * 'revision' - The type and location of the revision to deploy.
-- * 'targetInstances' - Information about the instances that belong to the replacement environment in a blue/green deployment.
-- * 'updateOutdatedInstancesOnly' - Indicates whether to deploy to all instances or only to instances that are not running the latest application revision.
mkCreateDeployment ::
  -- | 'applicationName'
  Lude.Text ->
  CreateDeployment
mkCreateDeployment pApplicationName_ =
  CreateDeployment'
    { deploymentConfigName = Lude.Nothing,
      fileExistsBehavior = Lude.Nothing,
      targetInstances = Lude.Nothing,
      revision = Lude.Nothing,
      description = Lude.Nothing,
      autoRollbackConfiguration = Lude.Nothing,
      updateOutdatedInstancesOnly = Lude.Nothing,
      deploymentGroupName = Lude.Nothing,
      ignoreApplicationStopFailures = Lude.Nothing,
      applicationName = pApplicationName_
    }

-- | The name of a deployment configuration associated with the IAM user or AWS account.
--
-- If not specified, the value configured in the deployment group is used as the default. If the deployment group does not have a deployment configuration associated with it, @CodeDeployDefault@ .@OneAtATime@ is used by default.
--
-- /Note:/ Consider using 'deploymentConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDeploymentConfigName :: Lens.Lens' CreateDeployment (Lude.Maybe Lude.Text)
cdDeploymentConfigName = Lens.lens (deploymentConfigName :: CreateDeployment -> Lude.Maybe Lude.Text) (\s a -> s {deploymentConfigName = a} :: CreateDeployment)
{-# DEPRECATED cdDeploymentConfigName "Use generic-lens or generic-optics with 'deploymentConfigName' instead." #-}

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
cdFileExistsBehavior :: Lens.Lens' CreateDeployment (Lude.Maybe FileExistsBehavior)
cdFileExistsBehavior = Lens.lens (fileExistsBehavior :: CreateDeployment -> Lude.Maybe FileExistsBehavior) (\s a -> s {fileExistsBehavior = a} :: CreateDeployment)
{-# DEPRECATED cdFileExistsBehavior "Use generic-lens or generic-optics with 'fileExistsBehavior' instead." #-}

-- | Information about the instances that belong to the replacement environment in a blue/green deployment.
--
-- /Note:/ Consider using 'targetInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdTargetInstances :: Lens.Lens' CreateDeployment (Lude.Maybe TargetInstances)
cdTargetInstances = Lens.lens (targetInstances :: CreateDeployment -> Lude.Maybe TargetInstances) (\s a -> s {targetInstances = a} :: CreateDeployment)
{-# DEPRECATED cdTargetInstances "Use generic-lens or generic-optics with 'targetInstances' instead." #-}

-- | The type and location of the revision to deploy.
--
-- /Note:/ Consider using 'revision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdRevision :: Lens.Lens' CreateDeployment (Lude.Maybe RevisionLocation)
cdRevision = Lens.lens (revision :: CreateDeployment -> Lude.Maybe RevisionLocation) (\s a -> s {revision = a} :: CreateDeployment)
{-# DEPRECATED cdRevision "Use generic-lens or generic-optics with 'revision' instead." #-}

-- | A comment about the deployment.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDescription :: Lens.Lens' CreateDeployment (Lude.Maybe Lude.Text)
cdDescription = Lens.lens (description :: CreateDeployment -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateDeployment)
{-# DEPRECATED cdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Configuration information for an automatic rollback that is added when a deployment is created.
--
-- /Note:/ Consider using 'autoRollbackConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdAutoRollbackConfiguration :: Lens.Lens' CreateDeployment (Lude.Maybe AutoRollbackConfiguration)
cdAutoRollbackConfiguration = Lens.lens (autoRollbackConfiguration :: CreateDeployment -> Lude.Maybe AutoRollbackConfiguration) (\s a -> s {autoRollbackConfiguration = a} :: CreateDeployment)
{-# DEPRECATED cdAutoRollbackConfiguration "Use generic-lens or generic-optics with 'autoRollbackConfiguration' instead." #-}

-- | Indicates whether to deploy to all instances or only to instances that are not running the latest application revision.
--
-- /Note:/ Consider using 'updateOutdatedInstancesOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdUpdateOutdatedInstancesOnly :: Lens.Lens' CreateDeployment (Lude.Maybe Lude.Bool)
cdUpdateOutdatedInstancesOnly = Lens.lens (updateOutdatedInstancesOnly :: CreateDeployment -> Lude.Maybe Lude.Bool) (\s a -> s {updateOutdatedInstancesOnly = a} :: CreateDeployment)
{-# DEPRECATED cdUpdateOutdatedInstancesOnly "Use generic-lens or generic-optics with 'updateOutdatedInstancesOnly' instead." #-}

-- | The name of the deployment group.
--
-- /Note:/ Consider using 'deploymentGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDeploymentGroupName :: Lens.Lens' CreateDeployment (Lude.Maybe Lude.Text)
cdDeploymentGroupName = Lens.lens (deploymentGroupName :: CreateDeployment -> Lude.Maybe Lude.Text) (\s a -> s {deploymentGroupName = a} :: CreateDeployment)
{-# DEPRECATED cdDeploymentGroupName "Use generic-lens or generic-optics with 'deploymentGroupName' instead." #-}

-- | If true, then if an @ApplicationStop@ , @BeforeBlockTraffic@ , or @AfterBlockTraffic@ deployment lifecycle event to an instance fails, then the deployment continues to the next deployment lifecycle event. For example, if @ApplicationStop@ fails, the deployment continues with @DownloadBundle@ . If @BeforeBlockTraffic@ fails, the deployment continues with @BlockTraffic@ . If @AfterBlockTraffic@ fails, the deployment continues with @ApplicationStop@ .
--
-- If false or not specified, then if a lifecycle event fails during a deployment to an instance, that deployment fails. If deployment to that instance is part of an overall deployment and the number of healthy hosts is not less than the minimum number of healthy hosts, then a deployment to the next instance is attempted.
-- During a deployment, the AWS CodeDeploy agent runs the scripts specified for @ApplicationStop@ , @BeforeBlockTraffic@ , and @AfterBlockTraffic@ in the AppSpec file from the previous successful deployment. (All other scripts are run from the AppSpec file in the current deployment.) If one of these scripts contains an error and does not run successfully, the deployment can fail.
-- If the cause of the failure is a script from the last successful deployment that will never run successfully, create a new deployment and use @ignoreApplicationStopFailures@ to specify that the @ApplicationStop@ , @BeforeBlockTraffic@ , and @AfterBlockTraffic@ failures should be ignored.
--
-- /Note:/ Consider using 'ignoreApplicationStopFailures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdIgnoreApplicationStopFailures :: Lens.Lens' CreateDeployment (Lude.Maybe Lude.Bool)
cdIgnoreApplicationStopFailures = Lens.lens (ignoreApplicationStopFailures :: CreateDeployment -> Lude.Maybe Lude.Bool) (\s a -> s {ignoreApplicationStopFailures = a} :: CreateDeployment)
{-# DEPRECATED cdIgnoreApplicationStopFailures "Use generic-lens or generic-optics with 'ignoreApplicationStopFailures' instead." #-}

-- | The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdApplicationName :: Lens.Lens' CreateDeployment Lude.Text
cdApplicationName = Lens.lens (applicationName :: CreateDeployment -> Lude.Text) (\s a -> s {applicationName = a} :: CreateDeployment)
{-# DEPRECATED cdApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

instance Lude.AWSRequest CreateDeployment where
  type Rs CreateDeployment = CreateDeploymentResponse
  request = Req.postJSON codeDeployService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateDeploymentResponse'
            Lude.<$> (x Lude..?> "deploymentId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDeployment where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeDeploy_20141006.CreateDeployment" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateDeployment where
  toJSON CreateDeployment' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("deploymentConfigName" Lude..=) Lude.<$> deploymentConfigName,
            ("fileExistsBehavior" Lude..=) Lude.<$> fileExistsBehavior,
            ("targetInstances" Lude..=) Lude.<$> targetInstances,
            ("revision" Lude..=) Lude.<$> revision,
            ("description" Lude..=) Lude.<$> description,
            ("autoRollbackConfiguration" Lude..=)
              Lude.<$> autoRollbackConfiguration,
            ("updateOutdatedInstancesOnly" Lude..=)
              Lude.<$> updateOutdatedInstancesOnly,
            ("deploymentGroupName" Lude..=) Lude.<$> deploymentGroupName,
            ("ignoreApplicationStopFailures" Lude..=)
              Lude.<$> ignoreApplicationStopFailures,
            Lude.Just ("applicationName" Lude..= applicationName)
          ]
      )

instance Lude.ToPath CreateDeployment where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDeployment where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @CreateDeployment@ operation.
--
-- /See:/ 'mkCreateDeploymentResponse' smart constructor.
data CreateDeploymentResponse = CreateDeploymentResponse'
  { deploymentId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDeploymentResponse' with the minimum fields required to make a request.
--
-- * 'deploymentId' - The unique ID of a deployment.
-- * 'responseStatus' - The response status code.
mkCreateDeploymentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDeploymentResponse
mkCreateDeploymentResponse pResponseStatus_ =
  CreateDeploymentResponse'
    { deploymentId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique ID of a deployment.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsDeploymentId :: Lens.Lens' CreateDeploymentResponse (Lude.Maybe Lude.Text)
cdrsDeploymentId = Lens.lens (deploymentId :: CreateDeploymentResponse -> Lude.Maybe Lude.Text) (\s a -> s {deploymentId = a} :: CreateDeploymentResponse)
{-# DEPRECATED cdrsDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsResponseStatus :: Lens.Lens' CreateDeploymentResponse Lude.Int
cdrsResponseStatus = Lens.lens (responseStatus :: CreateDeploymentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDeploymentResponse)
{-# DEPRECATED cdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
