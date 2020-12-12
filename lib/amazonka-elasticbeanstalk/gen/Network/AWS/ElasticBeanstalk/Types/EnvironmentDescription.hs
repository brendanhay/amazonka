{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.EnvironmentDescription
  ( EnvironmentDescription (..),

    -- * Smart constructor
    mkEnvironmentDescription,

    -- * Lenses
    eStatus,
    eCNAME,
    eTemplateName,
    eAbortableOperationInProgress,
    eEndpointURL,
    eResources,
    eDateUpdated,
    eDateCreated,
    eHealth,
    eVersionLabel,
    eOperationsRole,
    ePlatformARN,
    eTier,
    eEnvironmentName,
    eApplicationName,
    eEnvironmentARN,
    eSolutionStackName,
    eEnvironmentId,
    eHealthStatus,
    eEnvironmentLinks,
    eDescription,
  )
where

import Network.AWS.ElasticBeanstalk.Types.EnvironmentHealth
import Network.AWS.ElasticBeanstalk.Types.EnvironmentHealthStatus
import Network.AWS.ElasticBeanstalk.Types.EnvironmentLink
import Network.AWS.ElasticBeanstalk.Types.EnvironmentResourcesDescription
import Network.AWS.ElasticBeanstalk.Types.EnvironmentStatus
import Network.AWS.ElasticBeanstalk.Types.EnvironmentTier
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the properties of an environment.
--
-- /See:/ 'mkEnvironmentDescription' smart constructor.
data EnvironmentDescription = EnvironmentDescription'
  { status ::
      Lude.Maybe EnvironmentStatus,
    cNAME :: Lude.Maybe Lude.Text,
    templateName :: Lude.Maybe Lude.Text,
    abortableOperationInProgress ::
      Lude.Maybe Lude.Bool,
    endpointURL :: Lude.Maybe Lude.Text,
    resources ::
      Lude.Maybe EnvironmentResourcesDescription,
    dateUpdated :: Lude.Maybe Lude.DateTime,
    dateCreated :: Lude.Maybe Lude.DateTime,
    health :: Lude.Maybe EnvironmentHealth,
    versionLabel :: Lude.Maybe Lude.Text,
    operationsRole :: Lude.Maybe Lude.Text,
    platformARN :: Lude.Maybe Lude.Text,
    tier :: Lude.Maybe EnvironmentTier,
    environmentName :: Lude.Maybe Lude.Text,
    applicationName :: Lude.Maybe Lude.Text,
    environmentARN :: Lude.Maybe Lude.Text,
    solutionStackName :: Lude.Maybe Lude.Text,
    environmentId :: Lude.Maybe Lude.Text,
    healthStatus ::
      Lude.Maybe EnvironmentHealthStatus,
    environmentLinks ::
      Lude.Maybe [EnvironmentLink],
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnvironmentDescription' with the minimum fields required to make a request.
--
-- * 'abortableOperationInProgress' - Indicates if there is an in-progress environment configuration update or application version deployment that you can cancel.
--
-- @true:@ There is an update in progress.
-- @false:@ There are no updates currently in progress.
-- * 'applicationName' - The name of the application associated with this environment.
-- * 'cNAME' - The URL to the CNAME for this environment.
-- * 'dateCreated' - The creation date for this environment.
-- * 'dateUpdated' - The last modified date for this environment.
-- * 'description' - Describes this environment.
-- * 'endpointURL' - For load-balanced, autoscaling environments, the URL to the LoadBalancer. For single-instance environments, the IP address of the instance.
-- * 'environmentARN' - The environment's Amazon Resource Name (ARN), which can be used in other API requests that require an ARN.
-- * 'environmentId' - The ID of this environment.
-- * 'environmentLinks' - A list of links to other environments in the same group.
-- * 'environmentName' - The name of this environment.
-- * 'health' - Describes the health status of the environment. AWS Elastic Beanstalk indicates the failure levels for a running environment:
--
--
--     * @Red@ : Indicates the environment is not responsive. Occurs when three or more consecutive failures occur for an environment.
--
--
--     * @Yellow@ : Indicates that something is wrong. Occurs when two consecutive failures occur for an environment.
--
--
--     * @Green@ : Indicates the environment is healthy and fully functional.
--
--
--     * @Grey@ : Default health for a new environment. The environment is not fully launched and health checks have not started or health checks are suspended during an @UpdateEnvironment@ or @RestartEnvironment@ request.
--
--
-- Default: @Grey@
-- * 'healthStatus' - Returns the health status of the application running in your environment. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses> .
-- * 'operationsRole' - The Amazon Resource Name (ARN) of the environment's operations role. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/iam-operationsrole.html Operations roles> in the /AWS Elastic Beanstalk Developer Guide/ .
-- * 'platformARN' - The ARN of the platform version.
-- * 'resources' - The description of the AWS resources used by this environment.
-- * 'solutionStackName' - The name of the @SolutionStack@ deployed with this environment.
-- * 'status' - The current operational status of the environment:
--
--
--     * @Launching@ : Environment is in the process of initial deployment.
--
--
--     * @Updating@ : Environment is in the process of updating its configuration settings or application version.
--
--
--     * @Ready@ : Environment is available to have an action performed on it, such as update or terminate.
--
--
--     * @Terminating@ : Environment is in the shut-down process.
--
--
--     * @Terminated@ : Environment is not running.
--
--
-- * 'templateName' - The name of the configuration template used to originally launch this environment.
-- * 'tier' - Describes the current tier of this environment.
-- * 'versionLabel' - The application version deployed in this environment.
mkEnvironmentDescription ::
  EnvironmentDescription
mkEnvironmentDescription =
  EnvironmentDescription'
    { status = Lude.Nothing,
      cNAME = Lude.Nothing,
      templateName = Lude.Nothing,
      abortableOperationInProgress = Lude.Nothing,
      endpointURL = Lude.Nothing,
      resources = Lude.Nothing,
      dateUpdated = Lude.Nothing,
      dateCreated = Lude.Nothing,
      health = Lude.Nothing,
      versionLabel = Lude.Nothing,
      operationsRole = Lude.Nothing,
      platformARN = Lude.Nothing,
      tier = Lude.Nothing,
      environmentName = Lude.Nothing,
      applicationName = Lude.Nothing,
      environmentARN = Lude.Nothing,
      solutionStackName = Lude.Nothing,
      environmentId = Lude.Nothing,
      healthStatus = Lude.Nothing,
      environmentLinks = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The current operational status of the environment:
--
--
--     * @Launching@ : Environment is in the process of initial deployment.
--
--
--     * @Updating@ : Environment is in the process of updating its configuration settings or application version.
--
--
--     * @Ready@ : Environment is available to have an action performed on it, such as update or terminate.
--
--
--     * @Terminating@ : Environment is in the shut-down process.
--
--
--     * @Terminated@ : Environment is not running.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eStatus :: Lens.Lens' EnvironmentDescription (Lude.Maybe EnvironmentStatus)
eStatus = Lens.lens (status :: EnvironmentDescription -> Lude.Maybe EnvironmentStatus) (\s a -> s {status = a} :: EnvironmentDescription)
{-# DEPRECATED eStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The URL to the CNAME for this environment.
--
-- /Note:/ Consider using 'cNAME' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eCNAME :: Lens.Lens' EnvironmentDescription (Lude.Maybe Lude.Text)
eCNAME = Lens.lens (cNAME :: EnvironmentDescription -> Lude.Maybe Lude.Text) (\s a -> s {cNAME = a} :: EnvironmentDescription)
{-# DEPRECATED eCNAME "Use generic-lens or generic-optics with 'cNAME' instead." #-}

-- | The name of the configuration template used to originally launch this environment.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eTemplateName :: Lens.Lens' EnvironmentDescription (Lude.Maybe Lude.Text)
eTemplateName = Lens.lens (templateName :: EnvironmentDescription -> Lude.Maybe Lude.Text) (\s a -> s {templateName = a} :: EnvironmentDescription)
{-# DEPRECATED eTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | Indicates if there is an in-progress environment configuration update or application version deployment that you can cancel.
--
-- @true:@ There is an update in progress.
-- @false:@ There are no updates currently in progress.
--
-- /Note:/ Consider using 'abortableOperationInProgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eAbortableOperationInProgress :: Lens.Lens' EnvironmentDescription (Lude.Maybe Lude.Bool)
eAbortableOperationInProgress = Lens.lens (abortableOperationInProgress :: EnvironmentDescription -> Lude.Maybe Lude.Bool) (\s a -> s {abortableOperationInProgress = a} :: EnvironmentDescription)
{-# DEPRECATED eAbortableOperationInProgress "Use generic-lens or generic-optics with 'abortableOperationInProgress' instead." #-}

-- | For load-balanced, autoscaling environments, the URL to the LoadBalancer. For single-instance environments, the IP address of the instance.
--
-- /Note:/ Consider using 'endpointURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEndpointURL :: Lens.Lens' EnvironmentDescription (Lude.Maybe Lude.Text)
eEndpointURL = Lens.lens (endpointURL :: EnvironmentDescription -> Lude.Maybe Lude.Text) (\s a -> s {endpointURL = a} :: EnvironmentDescription)
{-# DEPRECATED eEndpointURL "Use generic-lens or generic-optics with 'endpointURL' instead." #-}

-- | The description of the AWS resources used by this environment.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eResources :: Lens.Lens' EnvironmentDescription (Lude.Maybe EnvironmentResourcesDescription)
eResources = Lens.lens (resources :: EnvironmentDescription -> Lude.Maybe EnvironmentResourcesDescription) (\s a -> s {resources = a} :: EnvironmentDescription)
{-# DEPRECATED eResources "Use generic-lens or generic-optics with 'resources' instead." #-}

-- | The last modified date for this environment.
--
-- /Note:/ Consider using 'dateUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eDateUpdated :: Lens.Lens' EnvironmentDescription (Lude.Maybe Lude.DateTime)
eDateUpdated = Lens.lens (dateUpdated :: EnvironmentDescription -> Lude.Maybe Lude.DateTime) (\s a -> s {dateUpdated = a} :: EnvironmentDescription)
{-# DEPRECATED eDateUpdated "Use generic-lens or generic-optics with 'dateUpdated' instead." #-}

-- | The creation date for this environment.
--
-- /Note:/ Consider using 'dateCreated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eDateCreated :: Lens.Lens' EnvironmentDescription (Lude.Maybe Lude.DateTime)
eDateCreated = Lens.lens (dateCreated :: EnvironmentDescription -> Lude.Maybe Lude.DateTime) (\s a -> s {dateCreated = a} :: EnvironmentDescription)
{-# DEPRECATED eDateCreated "Use generic-lens or generic-optics with 'dateCreated' instead." #-}

-- | Describes the health status of the environment. AWS Elastic Beanstalk indicates the failure levels for a running environment:
--
--
--     * @Red@ : Indicates the environment is not responsive. Occurs when three or more consecutive failures occur for an environment.
--
--
--     * @Yellow@ : Indicates that something is wrong. Occurs when two consecutive failures occur for an environment.
--
--
--     * @Green@ : Indicates the environment is healthy and fully functional.
--
--
--     * @Grey@ : Default health for a new environment. The environment is not fully launched and health checks have not started or health checks are suspended during an @UpdateEnvironment@ or @RestartEnvironment@ request.
--
--
-- Default: @Grey@
--
-- /Note:/ Consider using 'health' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eHealth :: Lens.Lens' EnvironmentDescription (Lude.Maybe EnvironmentHealth)
eHealth = Lens.lens (health :: EnvironmentDescription -> Lude.Maybe EnvironmentHealth) (\s a -> s {health = a} :: EnvironmentDescription)
{-# DEPRECATED eHealth "Use generic-lens or generic-optics with 'health' instead." #-}

-- | The application version deployed in this environment.
--
-- /Note:/ Consider using 'versionLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eVersionLabel :: Lens.Lens' EnvironmentDescription (Lude.Maybe Lude.Text)
eVersionLabel = Lens.lens (versionLabel :: EnvironmentDescription -> Lude.Maybe Lude.Text) (\s a -> s {versionLabel = a} :: EnvironmentDescription)
{-# DEPRECATED eVersionLabel "Use generic-lens or generic-optics with 'versionLabel' instead." #-}

-- | The Amazon Resource Name (ARN) of the environment's operations role. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/iam-operationsrole.html Operations roles> in the /AWS Elastic Beanstalk Developer Guide/ .
--
-- /Note:/ Consider using 'operationsRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eOperationsRole :: Lens.Lens' EnvironmentDescription (Lude.Maybe Lude.Text)
eOperationsRole = Lens.lens (operationsRole :: EnvironmentDescription -> Lude.Maybe Lude.Text) (\s a -> s {operationsRole = a} :: EnvironmentDescription)
{-# DEPRECATED eOperationsRole "Use generic-lens or generic-optics with 'operationsRole' instead." #-}

-- | The ARN of the platform version.
--
-- /Note:/ Consider using 'platformARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ePlatformARN :: Lens.Lens' EnvironmentDescription (Lude.Maybe Lude.Text)
ePlatformARN = Lens.lens (platformARN :: EnvironmentDescription -> Lude.Maybe Lude.Text) (\s a -> s {platformARN = a} :: EnvironmentDescription)
{-# DEPRECATED ePlatformARN "Use generic-lens or generic-optics with 'platformARN' instead." #-}

-- | Describes the current tier of this environment.
--
-- /Note:/ Consider using 'tier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eTier :: Lens.Lens' EnvironmentDescription (Lude.Maybe EnvironmentTier)
eTier = Lens.lens (tier :: EnvironmentDescription -> Lude.Maybe EnvironmentTier) (\s a -> s {tier = a} :: EnvironmentDescription)
{-# DEPRECATED eTier "Use generic-lens or generic-optics with 'tier' instead." #-}

-- | The name of this environment.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEnvironmentName :: Lens.Lens' EnvironmentDescription (Lude.Maybe Lude.Text)
eEnvironmentName = Lens.lens (environmentName :: EnvironmentDescription -> Lude.Maybe Lude.Text) (\s a -> s {environmentName = a} :: EnvironmentDescription)
{-# DEPRECATED eEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

-- | The name of the application associated with this environment.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eApplicationName :: Lens.Lens' EnvironmentDescription (Lude.Maybe Lude.Text)
eApplicationName = Lens.lens (applicationName :: EnvironmentDescription -> Lude.Maybe Lude.Text) (\s a -> s {applicationName = a} :: EnvironmentDescription)
{-# DEPRECATED eApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | The environment's Amazon Resource Name (ARN), which can be used in other API requests that require an ARN.
--
-- /Note:/ Consider using 'environmentARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEnvironmentARN :: Lens.Lens' EnvironmentDescription (Lude.Maybe Lude.Text)
eEnvironmentARN = Lens.lens (environmentARN :: EnvironmentDescription -> Lude.Maybe Lude.Text) (\s a -> s {environmentARN = a} :: EnvironmentDescription)
{-# DEPRECATED eEnvironmentARN "Use generic-lens or generic-optics with 'environmentARN' instead." #-}

-- | The name of the @SolutionStack@ deployed with this environment.
--
-- /Note:/ Consider using 'solutionStackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSolutionStackName :: Lens.Lens' EnvironmentDescription (Lude.Maybe Lude.Text)
eSolutionStackName = Lens.lens (solutionStackName :: EnvironmentDescription -> Lude.Maybe Lude.Text) (\s a -> s {solutionStackName = a} :: EnvironmentDescription)
{-# DEPRECATED eSolutionStackName "Use generic-lens or generic-optics with 'solutionStackName' instead." #-}

-- | The ID of this environment.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEnvironmentId :: Lens.Lens' EnvironmentDescription (Lude.Maybe Lude.Text)
eEnvironmentId = Lens.lens (environmentId :: EnvironmentDescription -> Lude.Maybe Lude.Text) (\s a -> s {environmentId = a} :: EnvironmentDescription)
{-# DEPRECATED eEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

-- | Returns the health status of the application running in your environment. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses> .
--
-- /Note:/ Consider using 'healthStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eHealthStatus :: Lens.Lens' EnvironmentDescription (Lude.Maybe EnvironmentHealthStatus)
eHealthStatus = Lens.lens (healthStatus :: EnvironmentDescription -> Lude.Maybe EnvironmentHealthStatus) (\s a -> s {healthStatus = a} :: EnvironmentDescription)
{-# DEPRECATED eHealthStatus "Use generic-lens or generic-optics with 'healthStatus' instead." #-}

-- | A list of links to other environments in the same group.
--
-- /Note:/ Consider using 'environmentLinks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEnvironmentLinks :: Lens.Lens' EnvironmentDescription (Lude.Maybe [EnvironmentLink])
eEnvironmentLinks = Lens.lens (environmentLinks :: EnvironmentDescription -> Lude.Maybe [EnvironmentLink]) (\s a -> s {environmentLinks = a} :: EnvironmentDescription)
{-# DEPRECATED eEnvironmentLinks "Use generic-lens or generic-optics with 'environmentLinks' instead." #-}

-- | Describes this environment.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eDescription :: Lens.Lens' EnvironmentDescription (Lude.Maybe Lude.Text)
eDescription = Lens.lens (description :: EnvironmentDescription -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: EnvironmentDescription)
{-# DEPRECATED eDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromXML EnvironmentDescription where
  parseXML x =
    EnvironmentDescription'
      Lude.<$> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "CNAME")
      Lude.<*> (x Lude..@? "TemplateName")
      Lude.<*> (x Lude..@? "AbortableOperationInProgress")
      Lude.<*> (x Lude..@? "EndpointURL")
      Lude.<*> (x Lude..@? "Resources")
      Lude.<*> (x Lude..@? "DateUpdated")
      Lude.<*> (x Lude..@? "DateCreated")
      Lude.<*> (x Lude..@? "Health")
      Lude.<*> (x Lude..@? "VersionLabel")
      Lude.<*> (x Lude..@? "OperationsRole")
      Lude.<*> (x Lude..@? "PlatformArn")
      Lude.<*> (x Lude..@? "Tier")
      Lude.<*> (x Lude..@? "EnvironmentName")
      Lude.<*> (x Lude..@? "ApplicationName")
      Lude.<*> (x Lude..@? "EnvironmentArn")
      Lude.<*> (x Lude..@? "SolutionStackName")
      Lude.<*> (x Lude..@? "EnvironmentId")
      Lude.<*> (x Lude..@? "HealthStatus")
      Lude.<*> ( x Lude..@? "EnvironmentLinks" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "Description")
