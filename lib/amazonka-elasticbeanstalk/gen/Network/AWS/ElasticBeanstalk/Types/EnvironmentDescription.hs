{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.EnvironmentDescription where

import Network.AWS.ElasticBeanstalk.Types.EnvironmentHealth
import Network.AWS.ElasticBeanstalk.Types.EnvironmentHealthStatus
import Network.AWS.ElasticBeanstalk.Types.EnvironmentLink
import Network.AWS.ElasticBeanstalk.Types.EnvironmentResourcesDescription
import Network.AWS.ElasticBeanstalk.Types.EnvironmentStatus
import Network.AWS.ElasticBeanstalk.Types.EnvironmentTier
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the properties of an environment.
--
--
--
-- /See:/ 'environmentDescription' smart constructor.
data EnvironmentDescription = EnvironmentDescription'
  { _eStatus ::
      !(Maybe EnvironmentStatus),
    _eCNAME :: !(Maybe Text),
    _eTemplateName :: !(Maybe Text),
    _eAbortableOperationInProgress ::
      !(Maybe Bool),
    _eEndpointURL :: !(Maybe Text),
    _eResources ::
      !(Maybe EnvironmentResourcesDescription),
    _eDateUpdated :: !(Maybe ISO8601),
    _eDateCreated :: !(Maybe ISO8601),
    _eHealth :: !(Maybe EnvironmentHealth),
    _eVersionLabel :: !(Maybe Text),
    _eOperationsRole :: !(Maybe Text),
    _ePlatformARN :: !(Maybe Text),
    _eTier :: !(Maybe EnvironmentTier),
    _eEnvironmentName :: !(Maybe Text),
    _eApplicationName :: !(Maybe Text),
    _eEnvironmentARN :: !(Maybe Text),
    _eSolutionStackName :: !(Maybe Text),
    _eEnvironmentId :: !(Maybe Text),
    _eHealthStatus ::
      !(Maybe EnvironmentHealthStatus),
    _eEnvironmentLinks ::
      !(Maybe [EnvironmentLink]),
    _eDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnvironmentDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eStatus' - The current operational status of the environment:     * @Launching@ : Environment is in the process of initial deployment.     * @Updating@ : Environment is in the process of updating its configuration settings or application version.     * @Ready@ : Environment is available to have an action performed on it, such as update or terminate.     * @Terminating@ : Environment is in the shut-down process.     * @Terminated@ : Environment is not running.
--
-- * 'eCNAME' - The URL to the CNAME for this environment.
--
-- * 'eTemplateName' - The name of the configuration template used to originally launch this environment.
--
-- * 'eAbortableOperationInProgress' - Indicates if there is an in-progress environment configuration update or application version deployment that you can cancel. @true:@ There is an update in progress.  @false:@ There are no updates currently in progress.
--
-- * 'eEndpointURL' - For load-balanced, autoscaling environments, the URL to the LoadBalancer. For single-instance environments, the IP address of the instance.
--
-- * 'eResources' - The description of the AWS resources used by this environment.
--
-- * 'eDateUpdated' - The last modified date for this environment.
--
-- * 'eDateCreated' - The creation date for this environment.
--
-- * 'eHealth' - Describes the health status of the environment. AWS Elastic Beanstalk indicates the failure levels for a running environment:     * @Red@ : Indicates the environment is not responsive. Occurs when three or more consecutive failures occur for an environment.     * @Yellow@ : Indicates that something is wrong. Occurs when two consecutive failures occur for an environment.     * @Green@ : Indicates the environment is healthy and fully functional.     * @Grey@ : Default health for a new environment. The environment is not fully launched and health checks have not started or health checks are suspended during an @UpdateEnvironment@ or @RestartEnvironment@ request. Default: @Grey@
--
-- * 'eVersionLabel' - The application version deployed in this environment.
--
-- * 'eOperationsRole' - The Amazon Resource Name (ARN) of the environment's operations role. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/iam-operationsrole.html Operations roles> in the /AWS Elastic Beanstalk Developer Guide/ .
--
-- * 'ePlatformARN' - The ARN of the platform version.
--
-- * 'eTier' - Describes the current tier of this environment.
--
-- * 'eEnvironmentName' - The name of this environment.
--
-- * 'eApplicationName' - The name of the application associated with this environment.
--
-- * 'eEnvironmentARN' - The environment's Amazon Resource Name (ARN), which can be used in other API requests that require an ARN.
--
-- * 'eSolutionStackName' - The name of the @SolutionStack@ deployed with this environment.
--
-- * 'eEnvironmentId' - The ID of this environment.
--
-- * 'eHealthStatus' - Returns the health status of the application running in your environment. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses> .
--
-- * 'eEnvironmentLinks' - A list of links to other environments in the same group.
--
-- * 'eDescription' - Describes this environment.
environmentDescription ::
  EnvironmentDescription
environmentDescription =
  EnvironmentDescription'
    { _eStatus = Nothing,
      _eCNAME = Nothing,
      _eTemplateName = Nothing,
      _eAbortableOperationInProgress = Nothing,
      _eEndpointURL = Nothing,
      _eResources = Nothing,
      _eDateUpdated = Nothing,
      _eDateCreated = Nothing,
      _eHealth = Nothing,
      _eVersionLabel = Nothing,
      _eOperationsRole = Nothing,
      _ePlatformARN = Nothing,
      _eTier = Nothing,
      _eEnvironmentName = Nothing,
      _eApplicationName = Nothing,
      _eEnvironmentARN = Nothing,
      _eSolutionStackName = Nothing,
      _eEnvironmentId = Nothing,
      _eHealthStatus = Nothing,
      _eEnvironmentLinks = Nothing,
      _eDescription = Nothing
    }

-- | The current operational status of the environment:     * @Launching@ : Environment is in the process of initial deployment.     * @Updating@ : Environment is in the process of updating its configuration settings or application version.     * @Ready@ : Environment is available to have an action performed on it, such as update or terminate.     * @Terminating@ : Environment is in the shut-down process.     * @Terminated@ : Environment is not running.
eStatus :: Lens' EnvironmentDescription (Maybe EnvironmentStatus)
eStatus = lens _eStatus (\s a -> s {_eStatus = a})

-- | The URL to the CNAME for this environment.
eCNAME :: Lens' EnvironmentDescription (Maybe Text)
eCNAME = lens _eCNAME (\s a -> s {_eCNAME = a})

-- | The name of the configuration template used to originally launch this environment.
eTemplateName :: Lens' EnvironmentDescription (Maybe Text)
eTemplateName = lens _eTemplateName (\s a -> s {_eTemplateName = a})

-- | Indicates if there is an in-progress environment configuration update or application version deployment that you can cancel. @true:@ There is an update in progress.  @false:@ There are no updates currently in progress.
eAbortableOperationInProgress :: Lens' EnvironmentDescription (Maybe Bool)
eAbortableOperationInProgress = lens _eAbortableOperationInProgress (\s a -> s {_eAbortableOperationInProgress = a})

-- | For load-balanced, autoscaling environments, the URL to the LoadBalancer. For single-instance environments, the IP address of the instance.
eEndpointURL :: Lens' EnvironmentDescription (Maybe Text)
eEndpointURL = lens _eEndpointURL (\s a -> s {_eEndpointURL = a})

-- | The description of the AWS resources used by this environment.
eResources :: Lens' EnvironmentDescription (Maybe EnvironmentResourcesDescription)
eResources = lens _eResources (\s a -> s {_eResources = a})

-- | The last modified date for this environment.
eDateUpdated :: Lens' EnvironmentDescription (Maybe UTCTime)
eDateUpdated = lens _eDateUpdated (\s a -> s {_eDateUpdated = a}) . mapping _Time

-- | The creation date for this environment.
eDateCreated :: Lens' EnvironmentDescription (Maybe UTCTime)
eDateCreated = lens _eDateCreated (\s a -> s {_eDateCreated = a}) . mapping _Time

-- | Describes the health status of the environment. AWS Elastic Beanstalk indicates the failure levels for a running environment:     * @Red@ : Indicates the environment is not responsive. Occurs when three or more consecutive failures occur for an environment.     * @Yellow@ : Indicates that something is wrong. Occurs when two consecutive failures occur for an environment.     * @Green@ : Indicates the environment is healthy and fully functional.     * @Grey@ : Default health for a new environment. The environment is not fully launched and health checks have not started or health checks are suspended during an @UpdateEnvironment@ or @RestartEnvironment@ request. Default: @Grey@
eHealth :: Lens' EnvironmentDescription (Maybe EnvironmentHealth)
eHealth = lens _eHealth (\s a -> s {_eHealth = a})

-- | The application version deployed in this environment.
eVersionLabel :: Lens' EnvironmentDescription (Maybe Text)
eVersionLabel = lens _eVersionLabel (\s a -> s {_eVersionLabel = a})

-- | The Amazon Resource Name (ARN) of the environment's operations role. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/iam-operationsrole.html Operations roles> in the /AWS Elastic Beanstalk Developer Guide/ .
eOperationsRole :: Lens' EnvironmentDescription (Maybe Text)
eOperationsRole = lens _eOperationsRole (\s a -> s {_eOperationsRole = a})

-- | The ARN of the platform version.
ePlatformARN :: Lens' EnvironmentDescription (Maybe Text)
ePlatformARN = lens _ePlatformARN (\s a -> s {_ePlatformARN = a})

-- | Describes the current tier of this environment.
eTier :: Lens' EnvironmentDescription (Maybe EnvironmentTier)
eTier = lens _eTier (\s a -> s {_eTier = a})

-- | The name of this environment.
eEnvironmentName :: Lens' EnvironmentDescription (Maybe Text)
eEnvironmentName = lens _eEnvironmentName (\s a -> s {_eEnvironmentName = a})

-- | The name of the application associated with this environment.
eApplicationName :: Lens' EnvironmentDescription (Maybe Text)
eApplicationName = lens _eApplicationName (\s a -> s {_eApplicationName = a})

-- | The environment's Amazon Resource Name (ARN), which can be used in other API requests that require an ARN.
eEnvironmentARN :: Lens' EnvironmentDescription (Maybe Text)
eEnvironmentARN = lens _eEnvironmentARN (\s a -> s {_eEnvironmentARN = a})

-- | The name of the @SolutionStack@ deployed with this environment.
eSolutionStackName :: Lens' EnvironmentDescription (Maybe Text)
eSolutionStackName = lens _eSolutionStackName (\s a -> s {_eSolutionStackName = a})

-- | The ID of this environment.
eEnvironmentId :: Lens' EnvironmentDescription (Maybe Text)
eEnvironmentId = lens _eEnvironmentId (\s a -> s {_eEnvironmentId = a})

-- | Returns the health status of the application running in your environment. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses> .
eHealthStatus :: Lens' EnvironmentDescription (Maybe EnvironmentHealthStatus)
eHealthStatus = lens _eHealthStatus (\s a -> s {_eHealthStatus = a})

-- | A list of links to other environments in the same group.
eEnvironmentLinks :: Lens' EnvironmentDescription [EnvironmentLink]
eEnvironmentLinks = lens _eEnvironmentLinks (\s a -> s {_eEnvironmentLinks = a}) . _Default . _Coerce

-- | Describes this environment.
eDescription :: Lens' EnvironmentDescription (Maybe Text)
eDescription = lens _eDescription (\s a -> s {_eDescription = a})

instance FromXML EnvironmentDescription where
  parseXML x =
    EnvironmentDescription'
      <$> (x .@? "Status")
      <*> (x .@? "CNAME")
      <*> (x .@? "TemplateName")
      <*> (x .@? "AbortableOperationInProgress")
      <*> (x .@? "EndpointURL")
      <*> (x .@? "Resources")
      <*> (x .@? "DateUpdated")
      <*> (x .@? "DateCreated")
      <*> (x .@? "Health")
      <*> (x .@? "VersionLabel")
      <*> (x .@? "OperationsRole")
      <*> (x .@? "PlatformArn")
      <*> (x .@? "Tier")
      <*> (x .@? "EnvironmentName")
      <*> (x .@? "ApplicationName")
      <*> (x .@? "EnvironmentArn")
      <*> (x .@? "SolutionStackName")
      <*> (x .@? "EnvironmentId")
      <*> (x .@? "HealthStatus")
      <*> ( x .@? "EnvironmentLinks" .!@ mempty
              >>= may (parseXMLList "member")
          )
      <*> (x .@? "Description")

instance Hashable EnvironmentDescription

instance NFData EnvironmentDescription
