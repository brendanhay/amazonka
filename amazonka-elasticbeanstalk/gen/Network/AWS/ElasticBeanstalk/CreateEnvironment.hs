{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.ElasticBeanstalk.CreateEnvironment
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Launches an environment for the specified application using the specified
-- configuration.
module Network.AWS.ElasticBeanstalk.CreateEnvironment
    (
    -- * Request
      CreateEnvironmentMessage
    -- ** Request constructor
    , createEnvironmentMessage
    -- ** Request lenses
    , cemApplicationName
    , cemCNAMEPrefix
    , cemDescription
    , cemEnvironmentName
    , cemOptionSettings
    , cemOptionsToRemove
    , cemSolutionStackName
    , cemTags
    , cemTemplateName
    , cemTier
    , cemVersionLabel

    -- * Response
    , EnvironmentDescription
    -- ** Response constructor
    , environmentDescription
    -- ** Response lenses
    , ed1ApplicationName
    , ed1CNAME
    , ed1DateCreated
    , ed1DateUpdated
    , ed1Description
    , ed1EndpointURL
    , ed1EnvironmentId
    , ed1EnvironmentName
    , ed1Health
    , ed1Resources
    , ed1SolutionStackName
    , ed1Status
    , ed1TemplateName
    , ed1Tier
    , ed1VersionLabel
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types

data CreateEnvironmentMessage = CreateEnvironmentMessage
    { _cemApplicationName   :: Text
    , _cemCNAMEPrefix       :: Maybe Text
    , _cemDescription       :: Maybe Text
    , _cemEnvironmentName   :: Text
    , _cemOptionSettings    :: [ConfigurationOptionSetting]
    , _cemOptionsToRemove   :: [OptionSpecification]
    , _cemSolutionStackName :: Maybe Text
    , _cemTags              :: [Tag]
    , _cemTemplateName      :: Maybe Text
    , _cemTier              :: Maybe EnvironmentTier
    , _cemVersionLabel      :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'CreateEnvironmentMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cemApplicationName' @::@ 'Text'
--
-- * 'cemCNAMEPrefix' @::@ 'Maybe' 'Text'
--
-- * 'cemDescription' @::@ 'Maybe' 'Text'
--
-- * 'cemEnvironmentName' @::@ 'Text'
--
-- * 'cemOptionSettings' @::@ ['ConfigurationOptionSetting']
--
-- * 'cemOptionsToRemove' @::@ ['OptionSpecification']
--
-- * 'cemSolutionStackName' @::@ 'Maybe' 'Text'
--
-- * 'cemTags' @::@ ['Tag']
--
-- * 'cemTemplateName' @::@ 'Maybe' 'Text'
--
-- * 'cemTier' @::@ 'Maybe' 'EnvironmentTier'
--
-- * 'cemVersionLabel' @::@ 'Maybe' 'Text'
--
createEnvironmentMessage :: Text -- ^ 'cemApplicationName'
                         -> Text -- ^ 'cemEnvironmentName'
                         -> CreateEnvironmentMessage
createEnvironmentMessage p1 p2 = CreateEnvironmentMessage
    { _cemApplicationName   = p1
    , _cemEnvironmentName   = p2
    , _cemDescription       = Nothing
    , _cemCNAMEPrefix       = Nothing
    , _cemTier              = Nothing
    , _cemTags              = mempty
    , _cemVersionLabel      = Nothing
    , _cemTemplateName      = Nothing
    , _cemSolutionStackName = Nothing
    , _cemOptionSettings    = mempty
    , _cemOptionsToRemove   = mempty
    }

-- | The name of the application that contains the version to be deployed. If
-- no application is found with this name, CreateEnvironment returns an
-- InvalidParameterValue error.
cemApplicationName :: Lens' CreateEnvironmentMessage Text
cemApplicationName =
    lens _cemApplicationName (\s a -> s { _cemApplicationName = a })

-- | If specified, the environment attempts to use this value as the prefix
-- for the CNAME. If not specified, the CNAME is generated automatically by
-- appending a random alphanumeric string to the environment name.
cemCNAMEPrefix :: Lens' CreateEnvironmentMessage (Maybe Text)
cemCNAMEPrefix = lens _cemCNAMEPrefix (\s a -> s { _cemCNAMEPrefix = a })

-- | Describes this environment.
cemDescription :: Lens' CreateEnvironmentMessage (Maybe Text)
cemDescription = lens _cemDescription (\s a -> s { _cemDescription = a })

-- | A unique name for the deployment environment. Used in the application
-- URL. Constraint: Must be from 4 to 23 characters in length. The name can
-- contain only letters, numbers, and hyphens. It cannot start or end with a
-- hyphen. This name must be unique in your account. If the specified name
-- already exists, AWS Elastic Beanstalk returns an InvalidParameterValue
-- error. Default: If the CNAME parameter is not specified, the environment
-- name becomes part of the CNAME, and therefore part of the visible URL for
-- your application.
cemEnvironmentName :: Lens' CreateEnvironmentMessage Text
cemEnvironmentName =
    lens _cemEnvironmentName (\s a -> s { _cemEnvironmentName = a })

-- | If specified, AWS Elastic Beanstalk sets the specified configuration
-- options to the requested value in the configuration set for the new
-- environment. These override the values obtained from the solution stack
-- or the configuration template.
cemOptionSettings :: Lens' CreateEnvironmentMessage [ConfigurationOptionSetting]
cemOptionSettings =
    lens _cemOptionSettings (\s a -> s { _cemOptionSettings = a })

-- | A list of custom user-defined configuration options to remove from the
-- configuration set for this new environment.
cemOptionsToRemove :: Lens' CreateEnvironmentMessage [OptionSpecification]
cemOptionsToRemove =
    lens _cemOptionsToRemove (\s a -> s { _cemOptionsToRemove = a })

-- | This is an alternative to specifying a configuration name. If specified,
-- AWS Elastic Beanstalk sets the configuration values to the default values
-- associated with the specified solution stack. Condition: You must specify
-- either this or a TemplateName, but not both. If you specify both, AWS
-- Elastic Beanstalk returns an InvalidParameterCombination error. If you do
-- not specify either, AWS Elastic Beanstalk returns a
-- MissingRequiredParameter error.
cemSolutionStackName :: Lens' CreateEnvironmentMessage (Maybe Text)
cemSolutionStackName =
    lens _cemSolutionStackName (\s a -> s { _cemSolutionStackName = a })

-- | This specifies the tags applied to resources in the environment.
cemTags :: Lens' CreateEnvironmentMessage [Tag]
cemTags = lens _cemTags (\s a -> s { _cemTags = a })

-- | The name of the configuration template to use in deployment. If no
-- configuration template is found with this name, AWS Elastic Beanstalk
-- returns an InvalidParameterValue error. Condition: You must specify
-- either this parameter or a SolutionStackName, but not both. If you
-- specify both, AWS Elastic Beanstalk returns an
-- InvalidParameterCombination error. If you do not specify either, AWS
-- Elastic Beanstalk returns a MissingRequiredParameter error.
cemTemplateName :: Lens' CreateEnvironmentMessage (Maybe Text)
cemTemplateName = lens _cemTemplateName (\s a -> s { _cemTemplateName = a })

-- | This specifies the tier to use for creating this environment.
cemTier :: Lens' CreateEnvironmentMessage (Maybe EnvironmentTier)
cemTier = lens _cemTier (\s a -> s { _cemTier = a })

-- | The name of the application version to deploy. If the specified
-- application has no associated application versions, AWS Elastic Beanstalk
-- UpdateEnvironment returns an InvalidParameterValue error. Default: If not
-- specified, AWS Elastic Beanstalk attempts to launch the sample
-- application in the container.
cemVersionLabel :: Lens' CreateEnvironmentMessage (Maybe Text)
cemVersionLabel = lens _cemVersionLabel (\s a -> s { _cemVersionLabel = a })

instance ToQuery CreateEnvironmentMessage

instance ToPath CreateEnvironmentMessage where
    toPath = const "/"

instance AWSRequest CreateEnvironmentMessage where
    type Sv CreateEnvironmentMessage = ElasticBeanstalk
    type Rs CreateEnvironmentMessage = EnvironmentDescription

    request  = post "CreateEnvironment"
    response = xmlResponse $ const decodeCursor
