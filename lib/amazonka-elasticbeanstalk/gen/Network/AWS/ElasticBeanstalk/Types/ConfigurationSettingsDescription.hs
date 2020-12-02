{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ConfigurationSettingsDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ConfigurationSettingsDescription where

import Network.AWS.ElasticBeanstalk.Types.ConfigurationDeploymentStatus
import Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionSetting
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the settings for a configuration set.
--
--
--
-- /See:/ 'configurationSettingsDescription' smart constructor.
data ConfigurationSettingsDescription = ConfigurationSettingsDescription'
  { _csdTemplateName ::
      !(Maybe Text),
    _csdOptionSettings ::
      !( Maybe
           [ConfigurationOptionSetting]
       ),
    _csdDateUpdated ::
      !(Maybe ISO8601),
    _csdDateCreated ::
      !(Maybe ISO8601),
    _csdPlatformARN ::
      !(Maybe Text),
    _csdEnvironmentName ::
      !(Maybe Text),
    _csdApplicationName ::
      !(Maybe Text),
    _csdDeploymentStatus ::
      !( Maybe
           ConfigurationDeploymentStatus
       ),
    _csdSolutionStackName ::
      !(Maybe Text),
    _csdDescription ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConfigurationSettingsDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csdTemplateName' - If not @null@ , the name of the configuration template for this configuration set.
--
-- * 'csdOptionSettings' - A list of the configuration options and their values in this configuration set.
--
-- * 'csdDateUpdated' - The date (in UTC time) when this configuration set was last modified.
--
-- * 'csdDateCreated' - The date (in UTC time) when this configuration set was created.
--
-- * 'csdPlatformARN' - The ARN of the platform version.
--
-- * 'csdEnvironmentName' - If not @null@ , the name of the environment for this configuration set.
--
-- * 'csdApplicationName' - The name of the application associated with this configuration set.
--
-- * 'csdDeploymentStatus' - If this configuration set is associated with an environment, the @DeploymentStatus@ parameter indicates the deployment status of this configuration set:      * @null@ : This configuration is not associated with a running environment.     * @pending@ : This is a draft configuration that is not deployed to the associated environment but is in the process of deploying.     * @deployed@ : This is the configuration that is currently deployed to the associated running environment.     * @failed@ : This is a draft configuration that failed to successfully deploy.
--
-- * 'csdSolutionStackName' - The name of the solution stack this configuration set uses.
--
-- * 'csdDescription' - Describes this configuration set.
configurationSettingsDescription ::
  ConfigurationSettingsDescription
configurationSettingsDescription =
  ConfigurationSettingsDescription'
    { _csdTemplateName = Nothing,
      _csdOptionSettings = Nothing,
      _csdDateUpdated = Nothing,
      _csdDateCreated = Nothing,
      _csdPlatformARN = Nothing,
      _csdEnvironmentName = Nothing,
      _csdApplicationName = Nothing,
      _csdDeploymentStatus = Nothing,
      _csdSolutionStackName = Nothing,
      _csdDescription = Nothing
    }

-- | If not @null@ , the name of the configuration template for this configuration set.
csdTemplateName :: Lens' ConfigurationSettingsDescription (Maybe Text)
csdTemplateName = lens _csdTemplateName (\s a -> s {_csdTemplateName = a})

-- | A list of the configuration options and their values in this configuration set.
csdOptionSettings :: Lens' ConfigurationSettingsDescription [ConfigurationOptionSetting]
csdOptionSettings = lens _csdOptionSettings (\s a -> s {_csdOptionSettings = a}) . _Default . _Coerce

-- | The date (in UTC time) when this configuration set was last modified.
csdDateUpdated :: Lens' ConfigurationSettingsDescription (Maybe UTCTime)
csdDateUpdated = lens _csdDateUpdated (\s a -> s {_csdDateUpdated = a}) . mapping _Time

-- | The date (in UTC time) when this configuration set was created.
csdDateCreated :: Lens' ConfigurationSettingsDescription (Maybe UTCTime)
csdDateCreated = lens _csdDateCreated (\s a -> s {_csdDateCreated = a}) . mapping _Time

-- | The ARN of the platform version.
csdPlatformARN :: Lens' ConfigurationSettingsDescription (Maybe Text)
csdPlatformARN = lens _csdPlatformARN (\s a -> s {_csdPlatformARN = a})

-- | If not @null@ , the name of the environment for this configuration set.
csdEnvironmentName :: Lens' ConfigurationSettingsDescription (Maybe Text)
csdEnvironmentName = lens _csdEnvironmentName (\s a -> s {_csdEnvironmentName = a})

-- | The name of the application associated with this configuration set.
csdApplicationName :: Lens' ConfigurationSettingsDescription (Maybe Text)
csdApplicationName = lens _csdApplicationName (\s a -> s {_csdApplicationName = a})

-- | If this configuration set is associated with an environment, the @DeploymentStatus@ parameter indicates the deployment status of this configuration set:      * @null@ : This configuration is not associated with a running environment.     * @pending@ : This is a draft configuration that is not deployed to the associated environment but is in the process of deploying.     * @deployed@ : This is the configuration that is currently deployed to the associated running environment.     * @failed@ : This is a draft configuration that failed to successfully deploy.
csdDeploymentStatus :: Lens' ConfigurationSettingsDescription (Maybe ConfigurationDeploymentStatus)
csdDeploymentStatus = lens _csdDeploymentStatus (\s a -> s {_csdDeploymentStatus = a})

-- | The name of the solution stack this configuration set uses.
csdSolutionStackName :: Lens' ConfigurationSettingsDescription (Maybe Text)
csdSolutionStackName = lens _csdSolutionStackName (\s a -> s {_csdSolutionStackName = a})

-- | Describes this configuration set.
csdDescription :: Lens' ConfigurationSettingsDescription (Maybe Text)
csdDescription = lens _csdDescription (\s a -> s {_csdDescription = a})

instance FromXML ConfigurationSettingsDescription where
  parseXML x =
    ConfigurationSettingsDescription'
      <$> (x .@? "TemplateName")
      <*> (x .@? "OptionSettings" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "DateUpdated")
      <*> (x .@? "DateCreated")
      <*> (x .@? "PlatformArn")
      <*> (x .@? "EnvironmentName")
      <*> (x .@? "ApplicationName")
      <*> (x .@? "DeploymentStatus")
      <*> (x .@? "SolutionStackName")
      <*> (x .@? "Description")

instance Hashable ConfigurationSettingsDescription

instance NFData ConfigurationSettingsDescription
