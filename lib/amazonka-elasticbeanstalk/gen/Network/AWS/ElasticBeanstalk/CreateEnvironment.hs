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
-- Module      : Network.AWS.ElasticBeanstalk.CreateEnvironment
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Launches an environment for the specified application using the specified configuration.
--
--
module Network.AWS.ElasticBeanstalk.CreateEnvironment
    (
    -- * Creating a Request
      createEnvironment
    , CreateEnvironment
    -- * Request Lenses
    , cCNAMEPrefix
    , cTemplateName
    , cOptionsToRemove
    , cOptionSettings
    , cVersionLabel
    , cPlatformARN
    , cTier
    , cEnvironmentName
    , cSolutionStackName
    , cGroupName
    , cDescription
    , cTags
    , cApplicationName

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

-- |
--
--
--
-- /See:/ 'createEnvironment' smart constructor.
data CreateEnvironment = CreateEnvironment'
  { _cCNAMEPrefix       :: !(Maybe Text)
  , _cTemplateName      :: !(Maybe Text)
  , _cOptionsToRemove   :: !(Maybe [OptionSpecification])
  , _cOptionSettings    :: !(Maybe [ConfigurationOptionSetting])
  , _cVersionLabel      :: !(Maybe Text)
  , _cPlatformARN       :: !(Maybe Text)
  , _cTier              :: !(Maybe EnvironmentTier)
  , _cEnvironmentName   :: !(Maybe Text)
  , _cSolutionStackName :: !(Maybe Text)
  , _cGroupName         :: !(Maybe Text)
  , _cDescription       :: !(Maybe Text)
  , _cTags              :: !(Maybe [Tag])
  , _cApplicationName   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateEnvironment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCNAMEPrefix' - If specified, the environment attempts to use this value as the prefix for the CNAME. If not specified, the CNAME is generated automatically by appending a random alphanumeric string to the environment name.
--
-- * 'cTemplateName' - The name of the configuration template to use in deployment. If no configuration template is found with this name, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error.
--
-- * 'cOptionsToRemove' - A list of custom user-defined configuration options to remove from the configuration set for this new environment.
--
-- * 'cOptionSettings' - If specified, AWS Elastic Beanstalk sets the specified configuration options to the requested value in the configuration set for the new environment. These override the values obtained from the solution stack or the configuration template.
--
-- * 'cVersionLabel' - The name of the application version to deploy. If the specified application has no associated application versions, AWS Elastic Beanstalk @UpdateEnvironment@ returns an @InvalidParameterValue@ error.  Default: If not specified, AWS Elastic Beanstalk attempts to launch the sample application in the container.
--
-- * 'cPlatformARN' - The ARN of the platform.
--
-- * 'cTier' - This specifies the tier to use for creating this environment.
--
-- * 'cEnvironmentName' - A unique name for the deployment environment. Used in the application URL. Constraint: Must be from 4 to 40 characters in length. The name can contain only letters, numbers, and hyphens. It cannot start or end with a hyphen. This name must be unique within a region in your account. If the specified name already exists in the region, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error.  Default: If the CNAME parameter is not specified, the environment name becomes part of the CNAME, and therefore part of the visible URL for your application.
--
-- * 'cSolutionStackName' - This is an alternative to specifying a template name. If specified, AWS Elastic Beanstalk sets the configuration values to the default values associated with the specified solution stack.
--
-- * 'cGroupName' - The name of the group to which the target environment belongs. Specify a group name only if the environment's name is specified in an environment manifest and not with the environment name parameter. See <http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html Environment Manifest (env.yaml)> for details.
--
-- * 'cDescription' - Describes this environment.
--
-- * 'cTags' - This specifies the tags applied to resources in the environment.
--
-- * 'cApplicationName' - The name of the application that contains the version to be deployed. If no application is found with this name, @CreateEnvironment@ returns an @InvalidParameterValue@ error.
createEnvironment
    :: Text -- ^ 'cApplicationName'
    -> CreateEnvironment
createEnvironment pApplicationName_ =
  CreateEnvironment'
    { _cCNAMEPrefix = Nothing
    , _cTemplateName = Nothing
    , _cOptionsToRemove = Nothing
    , _cOptionSettings = Nothing
    , _cVersionLabel = Nothing
    , _cPlatformARN = Nothing
    , _cTier = Nothing
    , _cEnvironmentName = Nothing
    , _cSolutionStackName = Nothing
    , _cGroupName = Nothing
    , _cDescription = Nothing
    , _cTags = Nothing
    , _cApplicationName = pApplicationName_
    }


-- | If specified, the environment attempts to use this value as the prefix for the CNAME. If not specified, the CNAME is generated automatically by appending a random alphanumeric string to the environment name.
cCNAMEPrefix :: Lens' CreateEnvironment (Maybe Text)
cCNAMEPrefix = lens _cCNAMEPrefix (\ s a -> s{_cCNAMEPrefix = a})

-- | The name of the configuration template to use in deployment. If no configuration template is found with this name, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error.
cTemplateName :: Lens' CreateEnvironment (Maybe Text)
cTemplateName = lens _cTemplateName (\ s a -> s{_cTemplateName = a})

-- | A list of custom user-defined configuration options to remove from the configuration set for this new environment.
cOptionsToRemove :: Lens' CreateEnvironment [OptionSpecification]
cOptionsToRemove = lens _cOptionsToRemove (\ s a -> s{_cOptionsToRemove = a}) . _Default . _Coerce

-- | If specified, AWS Elastic Beanstalk sets the specified configuration options to the requested value in the configuration set for the new environment. These override the values obtained from the solution stack or the configuration template.
cOptionSettings :: Lens' CreateEnvironment [ConfigurationOptionSetting]
cOptionSettings = lens _cOptionSettings (\ s a -> s{_cOptionSettings = a}) . _Default . _Coerce

-- | The name of the application version to deploy. If the specified application has no associated application versions, AWS Elastic Beanstalk @UpdateEnvironment@ returns an @InvalidParameterValue@ error.  Default: If not specified, AWS Elastic Beanstalk attempts to launch the sample application in the container.
cVersionLabel :: Lens' CreateEnvironment (Maybe Text)
cVersionLabel = lens _cVersionLabel (\ s a -> s{_cVersionLabel = a})

-- | The ARN of the platform.
cPlatformARN :: Lens' CreateEnvironment (Maybe Text)
cPlatformARN = lens _cPlatformARN (\ s a -> s{_cPlatformARN = a})

-- | This specifies the tier to use for creating this environment.
cTier :: Lens' CreateEnvironment (Maybe EnvironmentTier)
cTier = lens _cTier (\ s a -> s{_cTier = a})

-- | A unique name for the deployment environment. Used in the application URL. Constraint: Must be from 4 to 40 characters in length. The name can contain only letters, numbers, and hyphens. It cannot start or end with a hyphen. This name must be unique within a region in your account. If the specified name already exists in the region, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error.  Default: If the CNAME parameter is not specified, the environment name becomes part of the CNAME, and therefore part of the visible URL for your application.
cEnvironmentName :: Lens' CreateEnvironment (Maybe Text)
cEnvironmentName = lens _cEnvironmentName (\ s a -> s{_cEnvironmentName = a})

-- | This is an alternative to specifying a template name. If specified, AWS Elastic Beanstalk sets the configuration values to the default values associated with the specified solution stack.
cSolutionStackName :: Lens' CreateEnvironment (Maybe Text)
cSolutionStackName = lens _cSolutionStackName (\ s a -> s{_cSolutionStackName = a})

-- | The name of the group to which the target environment belongs. Specify a group name only if the environment's name is specified in an environment manifest and not with the environment name parameter. See <http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html Environment Manifest (env.yaml)> for details.
cGroupName :: Lens' CreateEnvironment (Maybe Text)
cGroupName = lens _cGroupName (\ s a -> s{_cGroupName = a})

-- | Describes this environment.
cDescription :: Lens' CreateEnvironment (Maybe Text)
cDescription = lens _cDescription (\ s a -> s{_cDescription = a})

-- | This specifies the tags applied to resources in the environment.
cTags :: Lens' CreateEnvironment [Tag]
cTags = lens _cTags (\ s a -> s{_cTags = a}) . _Default . _Coerce

-- | The name of the application that contains the version to be deployed. If no application is found with this name, @CreateEnvironment@ returns an @InvalidParameterValue@ error.
cApplicationName :: Lens' CreateEnvironment Text
cApplicationName = lens _cApplicationName (\ s a -> s{_cApplicationName = a})

instance AWSRequest CreateEnvironment where
        type Rs CreateEnvironment = EnvironmentDescription
        request = postQuery elasticBeanstalk
        response
          = receiveXMLWrapper "CreateEnvironmentResult"
              (\ s h x -> parseXML x)

instance Hashable CreateEnvironment where

instance NFData CreateEnvironment where

instance ToHeaders CreateEnvironment where
        toHeaders = const mempty

instance ToPath CreateEnvironment where
        toPath = const "/"

instance ToQuery CreateEnvironment where
        toQuery CreateEnvironment'{..}
          = mconcat
              ["Action" =: ("CreateEnvironment" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "CNAMEPrefix" =: _cCNAMEPrefix,
               "TemplateName" =: _cTemplateName,
               "OptionsToRemove" =:
                 toQuery (toQueryList "member" <$> _cOptionsToRemove),
               "OptionSettings" =:
                 toQuery (toQueryList "member" <$> _cOptionSettings),
               "VersionLabel" =: _cVersionLabel,
               "PlatformArn" =: _cPlatformARN, "Tier" =: _cTier,
               "EnvironmentName" =: _cEnvironmentName,
               "SolutionStackName" =: _cSolutionStackName,
               "GroupName" =: _cGroupName,
               "Description" =: _cDescription,
               "Tags" =: toQuery (toQueryList "member" <$> _cTags),
               "ApplicationName" =: _cApplicationName]
