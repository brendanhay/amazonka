{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CodeDeploy.CreateDeployment
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deploys an application revision to the specified deployment group.
module Network.AWS.CodeDeploy.CreateDeployment
    (
    -- * Request
      CreateDeployment
    -- ** Request constructor
    , createDeployment
    -- ** Request lenses
    , cdApplicationName
    , cdDeploymentConfigName
    , cdDeploymentGroupName
    , cdDescription
    , cdIgnoreApplicationStopFailures
    , cdRevision

    -- * Response
    , CreateDeploymentResponse
    -- ** Response constructor
    , createDeploymentResponse
    -- ** Response lenses
    , cdrDeploymentId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CodeDeploy.Types
import qualified GHC.Exts

data CreateDeployment = CreateDeployment
    { _cdApplicationName               :: Text
    , _cdDeploymentConfigName          :: Maybe Text
    , _cdDeploymentGroupName           :: Maybe Text
    , _cdDescription                   :: Maybe Text
    , _cdIgnoreApplicationStopFailures :: Maybe Bool
    , _cdRevision                      :: Maybe RevisionLocation
    } deriving (Eq, Show, Generic)

-- | 'CreateDeployment' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdApplicationName' @::@ 'Text'
--
-- * 'cdDeploymentConfigName' @::@ 'Maybe' 'Text'
--
-- * 'cdDeploymentGroupName' @::@ 'Maybe' 'Text'
--
-- * 'cdDescription' @::@ 'Maybe' 'Text'
--
-- * 'cdIgnoreApplicationStopFailures' @::@ 'Maybe' 'Bool'
--
-- * 'cdRevision' @::@ 'Maybe' 'RevisionLocation'
--
createDeployment :: Text -- ^ 'cdApplicationName'
                 -> CreateDeployment
createDeployment p1 = CreateDeployment
    { _cdApplicationName               = p1
    , _cdDeploymentGroupName           = Nothing
    , _cdRevision                      = Nothing
    , _cdDeploymentConfigName          = Nothing
    , _cdDescription                   = Nothing
    , _cdIgnoreApplicationStopFailures = Nothing
    }

-- | The name of an existing AWS CodeDeploy application within the AWS user
-- account.
cdApplicationName :: Lens' CreateDeployment Text
cdApplicationName =
    lens _cdApplicationName (\s a -> s { _cdApplicationName = a })

-- | The name of an existing deployment configuration within the AWS user
-- account. If not specified, the value configured in the deployment group
-- will be used as the default. If the deployment group does not have a
-- deployment configuration associated with it, then
-- CodeDeployDefault.OneAtATime will be used by default.
cdDeploymentConfigName :: Lens' CreateDeployment (Maybe Text)
cdDeploymentConfigName =
    lens _cdDeploymentConfigName (\s a -> s { _cdDeploymentConfigName = a })

-- | The deployment group's name.
cdDeploymentGroupName :: Lens' CreateDeployment (Maybe Text)
cdDeploymentGroupName =
    lens _cdDeploymentGroupName (\s a -> s { _cdDeploymentGroupName = a })

-- | A comment about the deployment.
cdDescription :: Lens' CreateDeployment (Maybe Text)
cdDescription = lens _cdDescription (\s a -> s { _cdDescription = a })

-- | If set to true, then if the deployment causes the ApplicationStop
-- deployment lifecycle event to fail to a specific instance, the deployment
-- will not be considered to have failed to that instance at that point and
-- will continue on to the BeforeInstall deployment lifecycle event. If set
-- to false or not specified, then if the deployment causes the
-- ApplicationStop deployment lifecycle event to fail to a specific
-- instance, the deployment will stop to that instance, and the deployment
-- to that instance will be considered to have failed.
cdIgnoreApplicationStopFailures :: Lens' CreateDeployment (Maybe Bool)
cdIgnoreApplicationStopFailures =
    lens _cdIgnoreApplicationStopFailures
        (\s a -> s { _cdIgnoreApplicationStopFailures = a })

-- | The type of revision to deploy, along with information about the
-- revision's location.
cdRevision :: Lens' CreateDeployment (Maybe RevisionLocation)
cdRevision = lens _cdRevision (\s a -> s { _cdRevision = a })

newtype CreateDeploymentResponse = CreateDeploymentResponse
    { _cdrDeploymentId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'CreateDeploymentResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdrDeploymentId' @::@ 'Maybe' 'Text'
--
createDeploymentResponse :: CreateDeploymentResponse
createDeploymentResponse = CreateDeploymentResponse
    { _cdrDeploymentId = Nothing
    }

-- | A unique deployment ID.
cdrDeploymentId :: Lens' CreateDeploymentResponse (Maybe Text)
cdrDeploymentId = lens _cdrDeploymentId (\s a -> s { _cdrDeploymentId = a })

instance AWSRequest CreateDeployment where
    type Sv CreateDeployment = CodeDeploy
    type Rs CreateDeployment = CreateDeploymentResponse

    request  = post
    response = jsonResponse

instance FromJSON CreateDeploymentResponse where
    parseJSON = genericParseJSON jsonOptions

instance ToPath CreateDeployment where
    toPath = const "/"

instance ToHeaders CreateDeployment

instance ToQuery CreateDeployment where
    toQuery = const mempty

instance ToJSON CreateDeployment where
    toJSON = genericToJSON jsonOptions
