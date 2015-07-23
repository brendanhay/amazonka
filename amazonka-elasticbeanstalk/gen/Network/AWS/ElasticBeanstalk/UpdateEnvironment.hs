{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.UpdateEnvironment
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Updates the environment description, deploys a new application version,
-- updates the configuration settings to an entirely new configuration
-- template, or updates select configuration option values in the running
-- environment.
--
-- Attempting to update both the release and configuration is not allowed
-- and AWS Elastic Beanstalk returns an @InvalidParameterCombination@
-- error.
--
-- When updating the configuration settings to a new template or individual
-- settings, a draft configuration is created and
-- DescribeConfigurationSettings for this environment returns two setting
-- descriptions with different @DeploymentStatus@ values.
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_UpdateEnvironment.html>
module Network.AWS.ElasticBeanstalk.UpdateEnvironment
    (
    -- * Request
      UpdateEnvironment
    -- ** Request constructor
    , updateEnvironment
    -- ** Request lenses
    , uerqTemplateName
    , uerqOptionsToRemove
    , uerqOptionSettings
    , uerqVersionLabel
    , uerqTier
    , uerqEnvironmentName
    , uerqEnvironmentId
    , uerqSolutionStackName
    , uerqDescription

    -- * Response
    , EnvironmentDescription
    -- ** Response constructor
    , environmentDescription
    -- ** Response lenses
    , eCNAME
    , eStatus
    , eTemplateName
    , eAbortableOperationInProgress
    , eEndpointURL
    , eDateUpdated
    , eResources
    , eHealth
    , eVersionLabel
    , eDateCreated
    , eTier
    , eEnvironmentName
    , eApplicationName
    , eEnvironmentId
    , eSolutionStackName
    , eDescription
    ) where

import           Network.AWS.ElasticBeanstalk.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | This documentation target is not reported in the API reference.
--
-- /See:/ 'updateEnvironment' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uerqTemplateName'
--
-- * 'uerqOptionsToRemove'
--
-- * 'uerqOptionSettings'
--
-- * 'uerqVersionLabel'
--
-- * 'uerqTier'
--
-- * 'uerqEnvironmentName'
--
-- * 'uerqEnvironmentId'
--
-- * 'uerqSolutionStackName'
--
-- * 'uerqDescription'
data UpdateEnvironment = UpdateEnvironment'
    { _uerqTemplateName      :: !(Maybe Text)
    , _uerqOptionsToRemove   :: !(Maybe [OptionSpecification])
    , _uerqOptionSettings    :: !(Maybe [ConfigurationOptionSetting])
    , _uerqVersionLabel      :: !(Maybe Text)
    , _uerqTier              :: !(Maybe EnvironmentTier)
    , _uerqEnvironmentName   :: !(Maybe Text)
    , _uerqEnvironmentId     :: !(Maybe Text)
    , _uerqSolutionStackName :: !(Maybe Text)
    , _uerqDescription       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateEnvironment' smart constructor.
updateEnvironment :: UpdateEnvironment
updateEnvironment =
    UpdateEnvironment'
    { _uerqTemplateName = Nothing
    , _uerqOptionsToRemove = Nothing
    , _uerqOptionSettings = Nothing
    , _uerqVersionLabel = Nothing
    , _uerqTier = Nothing
    , _uerqEnvironmentName = Nothing
    , _uerqEnvironmentId = Nothing
    , _uerqSolutionStackName = Nothing
    , _uerqDescription = Nothing
    }

-- | If this parameter is specified, AWS Elastic Beanstalk deploys this
-- configuration template to the environment. If no such configuration
-- template is found, AWS Elastic Beanstalk returns an
-- @InvalidParameterValue@ error.
uerqTemplateName :: Lens' UpdateEnvironment (Maybe Text)
uerqTemplateName = lens _uerqTemplateName (\ s a -> s{_uerqTemplateName = a});

-- | A list of custom user-defined configuration options to remove from the
-- configuration set for this environment.
uerqOptionsToRemove :: Lens' UpdateEnvironment [OptionSpecification]
uerqOptionsToRemove = lens _uerqOptionsToRemove (\ s a -> s{_uerqOptionsToRemove = a}) . _Default;

-- | If specified, AWS Elastic Beanstalk updates the configuration set
-- associated with the running environment and sets the specified
-- configuration options to the requested value.
uerqOptionSettings :: Lens' UpdateEnvironment [ConfigurationOptionSetting]
uerqOptionSettings = lens _uerqOptionSettings (\ s a -> s{_uerqOptionSettings = a}) . _Default;

-- | If this parameter is specified, AWS Elastic Beanstalk deploys the named
-- application version to the environment. If no such application version
-- is found, returns an @InvalidParameterValue@ error.
uerqVersionLabel :: Lens' UpdateEnvironment (Maybe Text)
uerqVersionLabel = lens _uerqVersionLabel (\ s a -> s{_uerqVersionLabel = a});

-- | This specifies the tier to use to update the environment.
--
-- Condition: At this time, if you change the tier version, name, or type,
-- AWS Elastic Beanstalk returns @InvalidParameterValue@ error.
uerqTier :: Lens' UpdateEnvironment (Maybe EnvironmentTier)
uerqTier = lens _uerqTier (\ s a -> s{_uerqTier = a});

-- | The name of the environment to update. If no environment with this name
-- exists, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
uerqEnvironmentName :: Lens' UpdateEnvironment (Maybe Text)
uerqEnvironmentName = lens _uerqEnvironmentName (\ s a -> s{_uerqEnvironmentName = a});

-- | The ID of the environment to update.
--
-- If no environment with this ID exists, AWS Elastic Beanstalk returns an
-- @InvalidParameterValue@ error.
--
-- Condition: You must specify either this or an EnvironmentName, or both.
-- If you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
uerqEnvironmentId :: Lens' UpdateEnvironment (Maybe Text)
uerqEnvironmentId = lens _uerqEnvironmentId (\ s a -> s{_uerqEnvironmentId = a});

-- | This specifies the platform version that the environment will run after
-- the environment is updated.
uerqSolutionStackName :: Lens' UpdateEnvironment (Maybe Text)
uerqSolutionStackName = lens _uerqSolutionStackName (\ s a -> s{_uerqSolutionStackName = a});

-- | If this parameter is specified, AWS Elastic Beanstalk updates the
-- description of this environment.
uerqDescription :: Lens' UpdateEnvironment (Maybe Text)
uerqDescription = lens _uerqDescription (\ s a -> s{_uerqDescription = a});

instance AWSRequest UpdateEnvironment where
        type Sv UpdateEnvironment = ElasticBeanstalk
        type Rs UpdateEnvironment = EnvironmentDescription
        request = post
        response
          = receiveXMLWrapper "UpdateEnvironmentResult"
              (\ s h x -> parseXML x)

instance ToHeaders UpdateEnvironment where
        toHeaders = const mempty

instance ToPath UpdateEnvironment where
        toPath = const "/"

instance ToQuery UpdateEnvironment where
        toQuery UpdateEnvironment'{..}
          = mconcat
              ["Action" =: ("UpdateEnvironment" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "TemplateName" =: _uerqTemplateName,
               "OptionsToRemove" =:
                 toQuery
                   (toQueryList "member" <$> _uerqOptionsToRemove),
               "OptionSettings" =:
                 toQuery
                   (toQueryList "member" <$> _uerqOptionSettings),
               "VersionLabel" =: _uerqVersionLabel,
               "Tier" =: _uerqTier,
               "EnvironmentName" =: _uerqEnvironmentName,
               "EnvironmentId" =: _uerqEnvironmentId,
               "SolutionStackName" =: _uerqSolutionStackName,
               "Description" =: _uerqDescription]
