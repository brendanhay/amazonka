{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.UpdateEnvironment
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the environment description, deploys a new application version, updates the configuration settings to an entirely new configuration template, or updates select configuration option values in the running environment.
--
--
-- Attempting to update both the release and configuration is not allowed and AWS Elastic Beanstalk returns an @InvalidParameterCombination@ error.
--
-- When updating the configuration settings to a new template or individual settings, a draft configuration is created and 'DescribeConfigurationSettings' for this environment returns two setting descriptions with different @DeploymentStatus@ values.
--
module Network.AWS.ElasticBeanstalk.UpdateEnvironment
    (
    -- * Creating a Request
      updateEnvironment
    , UpdateEnvironment
    -- * Request Lenses
    , ueTemplateName
    , ueOptionsToRemove
    , ueOptionSettings
    , ueVersionLabel
    , uePlatformARN
    , ueTier
    , ueEnvironmentName
    , ueApplicationName
    , ueSolutionStackName
    , ueEnvironmentId
    , ueGroupName
    , ueDescription

    -- * Destructuring the Response
    , environmentDescription
    , EnvironmentDescription
    -- * Response Lenses
    , eStatus
    , eCNAME
    , eTemplateName
    , eAbortableOperationInProgress
    , eEndpointURL
    , eResources
    , eDateUpdated
    , eDateCreated
    , eHealth
    , eVersionLabel
    , ePlatformARN
    , eTier
    , eEnvironmentName
    , eApplicationName
    , eEnvironmentARN
    , eSolutionStackName
    , eEnvironmentId
    , eHealthStatus
    , eEnvironmentLinks
    , eDescription
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.ElasticBeanstalk.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to update an environment.
--
--
--
-- /See:/ 'updateEnvironment' smart constructor.
data UpdateEnvironment = UpdateEnvironment'
  { _ueTemplateName      :: !(Maybe Text)
  , _ueOptionsToRemove   :: !(Maybe [OptionSpecification])
  , _ueOptionSettings    :: !(Maybe [ConfigurationOptionSetting])
  , _ueVersionLabel      :: !(Maybe Text)
  , _uePlatformARN       :: !(Maybe Text)
  , _ueTier              :: !(Maybe EnvironmentTier)
  , _ueEnvironmentName   :: !(Maybe Text)
  , _ueApplicationName   :: !(Maybe Text)
  , _ueSolutionStackName :: !(Maybe Text)
  , _ueEnvironmentId     :: !(Maybe Text)
  , _ueGroupName         :: !(Maybe Text)
  , _ueDescription       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateEnvironment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ueTemplateName' - If this parameter is specified, AWS Elastic Beanstalk deploys this configuration template to the environment. If no such configuration template is found, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error.
--
-- * 'ueOptionsToRemove' - A list of custom user-defined configuration options to remove from the configuration set for this environment.
--
-- * 'ueOptionSettings' - If specified, AWS Elastic Beanstalk updates the configuration set associated with the running environment and sets the specified configuration options to the requested value.
--
-- * 'ueVersionLabel' - If this parameter is specified, AWS Elastic Beanstalk deploys the named application version to the environment. If no such application version is found, returns an @InvalidParameterValue@ error.
--
-- * 'uePlatformARN' - The ARN of the platform, if used.
--
-- * 'ueTier' - This specifies the tier to use to update the environment. Condition: At this time, if you change the tier version, name, or type, AWS Elastic Beanstalk returns @InvalidParameterValue@ error.
--
-- * 'ueEnvironmentName' - The name of the environment to update. If no environment with this name exists, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error.  Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
--
-- * 'ueApplicationName' - The name of the application with which the environment is associated.
--
-- * 'ueSolutionStackName' - This specifies the platform version that the environment will run after the environment is updated.
--
-- * 'ueEnvironmentId' - The ID of the environment to update. If no environment with this ID exists, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error. Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
--
-- * 'ueGroupName' - The name of the group to which the target environment belongs. Specify a group name only if the environment's name is specified in an environment manifest and not with the environment name or environment ID parameters. See <http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html Environment Manifest (env.yaml)> for details.
--
-- * 'ueDescription' - If this parameter is specified, AWS Elastic Beanstalk updates the description of this environment.
updateEnvironment
    :: UpdateEnvironment
updateEnvironment =
  UpdateEnvironment'
    { _ueTemplateName = Nothing
    , _ueOptionsToRemove = Nothing
    , _ueOptionSettings = Nothing
    , _ueVersionLabel = Nothing
    , _uePlatformARN = Nothing
    , _ueTier = Nothing
    , _ueEnvironmentName = Nothing
    , _ueApplicationName = Nothing
    , _ueSolutionStackName = Nothing
    , _ueEnvironmentId = Nothing
    , _ueGroupName = Nothing
    , _ueDescription = Nothing
    }


-- | If this parameter is specified, AWS Elastic Beanstalk deploys this configuration template to the environment. If no such configuration template is found, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error.
ueTemplateName :: Lens' UpdateEnvironment (Maybe Text)
ueTemplateName = lens _ueTemplateName (\ s a -> s{_ueTemplateName = a})

-- | A list of custom user-defined configuration options to remove from the configuration set for this environment.
ueOptionsToRemove :: Lens' UpdateEnvironment [OptionSpecification]
ueOptionsToRemove = lens _ueOptionsToRemove (\ s a -> s{_ueOptionsToRemove = a}) . _Default . _Coerce

-- | If specified, AWS Elastic Beanstalk updates the configuration set associated with the running environment and sets the specified configuration options to the requested value.
ueOptionSettings :: Lens' UpdateEnvironment [ConfigurationOptionSetting]
ueOptionSettings = lens _ueOptionSettings (\ s a -> s{_ueOptionSettings = a}) . _Default . _Coerce

-- | If this parameter is specified, AWS Elastic Beanstalk deploys the named application version to the environment. If no such application version is found, returns an @InvalidParameterValue@ error.
ueVersionLabel :: Lens' UpdateEnvironment (Maybe Text)
ueVersionLabel = lens _ueVersionLabel (\ s a -> s{_ueVersionLabel = a})

-- | The ARN of the platform, if used.
uePlatformARN :: Lens' UpdateEnvironment (Maybe Text)
uePlatformARN = lens _uePlatformARN (\ s a -> s{_uePlatformARN = a})

-- | This specifies the tier to use to update the environment. Condition: At this time, if you change the tier version, name, or type, AWS Elastic Beanstalk returns @InvalidParameterValue@ error.
ueTier :: Lens' UpdateEnvironment (Maybe EnvironmentTier)
ueTier = lens _ueTier (\ s a -> s{_ueTier = a})

-- | The name of the environment to update. If no environment with this name exists, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error.  Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
ueEnvironmentName :: Lens' UpdateEnvironment (Maybe Text)
ueEnvironmentName = lens _ueEnvironmentName (\ s a -> s{_ueEnvironmentName = a})

-- | The name of the application with which the environment is associated.
ueApplicationName :: Lens' UpdateEnvironment (Maybe Text)
ueApplicationName = lens _ueApplicationName (\ s a -> s{_ueApplicationName = a})

-- | This specifies the platform version that the environment will run after the environment is updated.
ueSolutionStackName :: Lens' UpdateEnvironment (Maybe Text)
ueSolutionStackName = lens _ueSolutionStackName (\ s a -> s{_ueSolutionStackName = a})

-- | The ID of the environment to update. If no environment with this ID exists, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error. Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
ueEnvironmentId :: Lens' UpdateEnvironment (Maybe Text)
ueEnvironmentId = lens _ueEnvironmentId (\ s a -> s{_ueEnvironmentId = a})

-- | The name of the group to which the target environment belongs. Specify a group name only if the environment's name is specified in an environment manifest and not with the environment name or environment ID parameters. See <http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html Environment Manifest (env.yaml)> for details.
ueGroupName :: Lens' UpdateEnvironment (Maybe Text)
ueGroupName = lens _ueGroupName (\ s a -> s{_ueGroupName = a})

-- | If this parameter is specified, AWS Elastic Beanstalk updates the description of this environment.
ueDescription :: Lens' UpdateEnvironment (Maybe Text)
ueDescription = lens _ueDescription (\ s a -> s{_ueDescription = a})

instance AWSRequest UpdateEnvironment where
        type Rs UpdateEnvironment = EnvironmentDescription
        request = postQuery elasticBeanstalk
        response
          = receiveXMLWrapper "UpdateEnvironmentResult"
              (\ s h x -> parseXML x)

instance Hashable UpdateEnvironment where

instance NFData UpdateEnvironment where

instance ToHeaders UpdateEnvironment where
        toHeaders = const mempty

instance ToPath UpdateEnvironment where
        toPath = const "/"

instance ToQuery UpdateEnvironment where
        toQuery UpdateEnvironment'{..}
          = mconcat
              ["Action" =: ("UpdateEnvironment" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "TemplateName" =: _ueTemplateName,
               "OptionsToRemove" =:
                 toQuery
                   (toQueryList "member" <$> _ueOptionsToRemove),
               "OptionSettings" =:
                 toQuery (toQueryList "member" <$> _ueOptionSettings),
               "VersionLabel" =: _ueVersionLabel,
               "PlatformArn" =: _uePlatformARN, "Tier" =: _ueTier,
               "EnvironmentName" =: _ueEnvironmentName,
               "ApplicationName" =: _ueApplicationName,
               "SolutionStackName" =: _ueSolutionStackName,
               "EnvironmentId" =: _ueEnvironmentId,
               "GroupName" =: _ueGroupName,
               "Description" =: _ueDescription]
