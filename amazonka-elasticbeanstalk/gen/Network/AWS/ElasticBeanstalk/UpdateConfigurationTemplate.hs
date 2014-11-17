{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.UpdateConfigurationTemplate
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates the specified configuration template to have the specified
-- properties or configuration option values. Related Topics
-- DescribeConfigurationOptions.
module Network.AWS.ElasticBeanstalk.UpdateConfigurationTemplate
    (
    -- * Request
      UpdateConfigurationTemplate
    -- ** Request constructor
    , updateConfigurationTemplate
    -- ** Request lenses
    , uctApplicationName
    , uctDescription
    , uctOptionSettings
    , uctOptionsToRemove
    , uctTemplateName

    -- * Response
    , UpdateConfigurationTemplateResponse
    -- ** Response constructor
    , updateConfigurationTemplateResponse
    -- ** Response lenses
    , uctrApplicationName
    , uctrDateCreated
    , uctrDateUpdated
    , uctrDeploymentStatus
    , uctrDescription
    , uctrEnvironmentName
    , uctrOptionSettings
    , uctrSolutionStackName
    , uctrTemplateName
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import qualified GHC.Exts

data UpdateConfigurationTemplate = UpdateConfigurationTemplate
    { _uctApplicationName :: Text
    , _uctDescription     :: Maybe Text
    , _uctOptionSettings  :: [ConfigurationOptionSetting]
    , _uctOptionsToRemove :: [OptionSpecification]
    , _uctTemplateName    :: Text
    } deriving (Eq, Show, Generic)

-- | 'UpdateConfigurationTemplate' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uctApplicationName' @::@ 'Text'
--
-- * 'uctDescription' @::@ 'Maybe' 'Text'
--
-- * 'uctOptionSettings' @::@ ['ConfigurationOptionSetting']
--
-- * 'uctOptionsToRemove' @::@ ['OptionSpecification']
--
-- * 'uctTemplateName' @::@ 'Text'
--
updateConfigurationTemplate :: Text -- ^ 'uctApplicationName'
                            -> Text -- ^ 'uctTemplateName'
                            -> UpdateConfigurationTemplate
updateConfigurationTemplate p1 p2 = UpdateConfigurationTemplate
    { _uctApplicationName = p1
    , _uctTemplateName    = p2
    , _uctDescription     = Nothing
    , _uctOptionSettings  = mempty
    , _uctOptionsToRemove = mempty
    }

-- | The name of the application associated with the configuration template to
-- update. If no application is found with this name,
-- UpdateConfigurationTemplate returns an InvalidParameterValue error.
uctApplicationName :: Lens' UpdateConfigurationTemplate Text
uctApplicationName =
    lens _uctApplicationName (\s a -> s { _uctApplicationName = a })

-- | A new description for the configuration.
uctDescription :: Lens' UpdateConfigurationTemplate (Maybe Text)
uctDescription = lens _uctDescription (\s a -> s { _uctDescription = a })

-- | A list of configuration option settings to update with the new specified
-- option value.
uctOptionSettings :: Lens' UpdateConfigurationTemplate [ConfigurationOptionSetting]
uctOptionSettings =
    lens _uctOptionSettings (\s a -> s { _uctOptionSettings = a })

-- | A list of configuration options to remove from the configuration set.
-- Constraint: You can remove only UserDefined configuration options.
uctOptionsToRemove :: Lens' UpdateConfigurationTemplate [OptionSpecification]
uctOptionsToRemove =
    lens _uctOptionsToRemove (\s a -> s { _uctOptionsToRemove = a })

-- | The name of the configuration template to update. If no configuration
-- template is found with this name, UpdateConfigurationTemplate returns an
-- InvalidParameterValue error.
uctTemplateName :: Lens' UpdateConfigurationTemplate Text
uctTemplateName = lens _uctTemplateName (\s a -> s { _uctTemplateName = a })

data UpdateConfigurationTemplateResponse = UpdateConfigurationTemplateResponse
    { _uctrApplicationName   :: Maybe Text
    , _uctrDateCreated       :: Maybe RFC822
    , _uctrDateUpdated       :: Maybe RFC822
    , _uctrDeploymentStatus  :: Maybe Text
    , _uctrDescription       :: Maybe Text
    , _uctrEnvironmentName   :: Maybe Text
    , _uctrOptionSettings    :: [ConfigurationOptionSetting]
    , _uctrSolutionStackName :: Maybe Text
    , _uctrTemplateName      :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'UpdateConfigurationTemplateResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uctrApplicationName' @::@ 'Maybe' 'Text'
--
-- * 'uctrDateCreated' @::@ 'Maybe' 'UTCTime'
--
-- * 'uctrDateUpdated' @::@ 'Maybe' 'UTCTime'
--
-- * 'uctrDeploymentStatus' @::@ 'Maybe' 'Text'
--
-- * 'uctrDescription' @::@ 'Maybe' 'Text'
--
-- * 'uctrEnvironmentName' @::@ 'Maybe' 'Text'
--
-- * 'uctrOptionSettings' @::@ ['ConfigurationOptionSetting']
--
-- * 'uctrSolutionStackName' @::@ 'Maybe' 'Text'
--
-- * 'uctrTemplateName' @::@ 'Maybe' 'Text'
--
updateConfigurationTemplateResponse :: UpdateConfigurationTemplateResponse
updateConfigurationTemplateResponse = UpdateConfigurationTemplateResponse
    { _uctrSolutionStackName = Nothing
    , _uctrApplicationName   = Nothing
    , _uctrTemplateName      = Nothing
    , _uctrDescription       = Nothing
    , _uctrEnvironmentName   = Nothing
    , _uctrDeploymentStatus  = Nothing
    , _uctrDateCreated       = Nothing
    , _uctrDateUpdated       = Nothing
    , _uctrOptionSettings    = mempty
    }

-- | The name of the application associated with this configuration set.
uctrApplicationName :: Lens' UpdateConfigurationTemplateResponse (Maybe Text)
uctrApplicationName =
    lens _uctrApplicationName (\s a -> s { _uctrApplicationName = a })

-- | The date (in UTC time) when this configuration set was created.
uctrDateCreated :: Lens' UpdateConfigurationTemplateResponse (Maybe UTCTime)
uctrDateCreated = lens _uctrDateCreated (\s a -> s { _uctrDateCreated = a })
    . mapping _Time

-- | The date (in UTC time) when this configuration set was last modified.
uctrDateUpdated :: Lens' UpdateConfigurationTemplateResponse (Maybe UTCTime)
uctrDateUpdated = lens _uctrDateUpdated (\s a -> s { _uctrDateUpdated = a })
    . mapping _Time

-- | If this configuration set is associated with an environment, the
-- DeploymentStatus parameter indicates the deployment status of this
-- configuration set: null: This configuration is not associated with a
-- running environment. pending: This is a draft configuration that is not
-- deployed to the associated environment but is in the process of
-- deploying. deployed: This is the configuration that is currently deployed
-- to the associated running environment. failed: This is a draft
-- configuration, that failed to successfully deploy. null: This
-- configuration is not associated with a running environment. pending: This
-- is a draft configuration that is not deployed to the associated
-- environment but is in the process of deploying. deployed: This is the
-- configuration that is currently deployed to the associated running
-- environment. failed: This is a draft configuration that failed to
-- successfully deploy.
uctrDeploymentStatus :: Lens' UpdateConfigurationTemplateResponse (Maybe Text)
uctrDeploymentStatus =
    lens _uctrDeploymentStatus (\s a -> s { _uctrDeploymentStatus = a })

-- | Describes this configuration set.
uctrDescription :: Lens' UpdateConfigurationTemplateResponse (Maybe Text)
uctrDescription = lens _uctrDescription (\s a -> s { _uctrDescription = a })

-- | If not null, the name of the environment for this configuration set.
uctrEnvironmentName :: Lens' UpdateConfigurationTemplateResponse (Maybe Text)
uctrEnvironmentName =
    lens _uctrEnvironmentName (\s a -> s { _uctrEnvironmentName = a })

-- | A list of the configuration options and their values in this
-- configuration set.
uctrOptionSettings :: Lens' UpdateConfigurationTemplateResponse [ConfigurationOptionSetting]
uctrOptionSettings =
    lens _uctrOptionSettings (\s a -> s { _uctrOptionSettings = a })

-- | The name of the solution stack this configuration set uses.
uctrSolutionStackName :: Lens' UpdateConfigurationTemplateResponse (Maybe Text)
uctrSolutionStackName =
    lens _uctrSolutionStackName (\s a -> s { _uctrSolutionStackName = a })

-- | If not null, the name of the configuration template for this
-- configuration set.
uctrTemplateName :: Lens' UpdateConfigurationTemplateResponse (Maybe Text)
uctrTemplateName = lens _uctrTemplateName (\s a -> s { _uctrTemplateName = a })

instance AWSRequest UpdateConfigurationTemplate where
    type Sv UpdateConfigurationTemplate = ElasticBeanstalk
    type Rs UpdateConfigurationTemplate = UpdateConfigurationTemplateResponse

    request  = post "UpdateConfigurationTemplate"
    response = xmlResponse

instance FromXML UpdateConfigurationTemplateResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "UpdateConfigurationTemplateResponse"

instance ToPath UpdateConfigurationTemplate where
    toPath = const "/"

instance ToHeaders UpdateConfigurationTemplate

instance ToQuery UpdateConfigurationTemplate
