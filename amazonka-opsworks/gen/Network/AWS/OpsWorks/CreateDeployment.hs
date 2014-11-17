{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.CreateDeployment
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deploys a stack or app. App deployment generates a deploy event, which runs
-- the associated recipes and passes them a JSON stack configuration object
-- that includes information about the app. Stack deployment runs the deploy
-- recipes but does not raise an event. For more information, see Deploying
-- Apps and Run Stack Commands. Required Permissions: To use this action, an
-- IAM user must have a Deploy or Manage permissions level for the stack, or
-- an attached policy that explicitly grants permissions. For more information
-- on user permissions, see Managing User Permissions.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_CreateDeployment.html>
module Network.AWS.OpsWorks.CreateDeployment
    (
    -- * Request
      CreateDeployment
    -- ** Request constructor
    , createDeployment
    -- ** Request lenses
    , cdAppId
    , cdCommand
    , cdComment
    , cdCustomJson
    , cdInstanceIds
    , cdStackId

    -- * Response
    , CreateDeploymentResponse
    -- ** Response constructor
    , createDeploymentResponse
    -- ** Response lenses
    , cdrDeploymentId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

data CreateDeployment = CreateDeployment
    { _cdAppId       :: Maybe Text
    , _cdCommand     :: DeploymentCommand
    , _cdComment     :: Maybe Text
    , _cdCustomJson  :: Maybe Text
    , _cdInstanceIds :: [Text]
    , _cdStackId     :: Text
    } deriving (Eq, Show, Generic)

-- | 'CreateDeployment' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdAppId' @::@ 'Maybe' 'Text'
--
-- * 'cdCommand' @::@ 'DeploymentCommand'
--
-- * 'cdComment' @::@ 'Maybe' 'Text'
--
-- * 'cdCustomJson' @::@ 'Maybe' 'Text'
--
-- * 'cdInstanceIds' @::@ ['Text']
--
-- * 'cdStackId' @::@ 'Text'
--
createDeployment :: Text -- ^ 'cdStackId'
                 -> DeploymentCommand -- ^ 'cdCommand'
                 -> CreateDeployment
createDeployment p1 p2 = CreateDeployment
    { _cdStackId     = p1
    , _cdCommand     = p2
    , _cdAppId       = Nothing
    , _cdInstanceIds = mempty
    , _cdComment     = Nothing
    , _cdCustomJson  = Nothing
    }

-- | The app ID. This parameter is required for app deployments, but not for
-- other deployment commands.
cdAppId :: Lens' CreateDeployment (Maybe Text)
cdAppId = lens _cdAppId (\s a -> s { _cdAppId = a })

-- | A DeploymentCommand object that specifies the deployment command and any
-- associated arguments.
cdCommand :: Lens' CreateDeployment DeploymentCommand
cdCommand = lens _cdCommand (\s a -> s { _cdCommand = a })

-- | A user-defined comment.
cdComment :: Lens' CreateDeployment (Maybe Text)
cdComment = lens _cdComment (\s a -> s { _cdComment = a })

-- | A string that contains user-defined, custom JSON. It is used to override
-- the corresponding default stack configuration JSON values. The string
-- should be in the following format and must escape characters such as
-- '"'.: "{\"key1\": \"value1\", \"key2\": \"value2\",...}" For more
-- information on custom JSON, see Use Custom JSON to Modify the Stack
-- Configuration JSON.
cdCustomJson :: Lens' CreateDeployment (Maybe Text)
cdCustomJson = lens _cdCustomJson (\s a -> s { _cdCustomJson = a })

-- | The instance IDs for the deployment targets.
cdInstanceIds :: Lens' CreateDeployment [Text]
cdInstanceIds = lens _cdInstanceIds (\s a -> s { _cdInstanceIds = a })

-- | The stack ID.
cdStackId :: Lens' CreateDeployment Text
cdStackId = lens _cdStackId (\s a -> s { _cdStackId = a })

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

-- | The deployment ID, which can be used with other requests to identify the
-- deployment.
cdrDeploymentId :: Lens' CreateDeploymentResponse (Maybe Text)
cdrDeploymentId = lens _cdrDeploymentId (\s a -> s { _cdrDeploymentId = a })

instance ToPath CreateDeployment where
    toPath = const "/"

instance ToQuery CreateDeployment where
    toQuery = const mempty

instance ToHeaders CreateDeployment
instance ToJSON CreateDeployment where
    toJSON = genericToJSON jsonOptions

instance AWSRequest CreateDeployment where
    type Sv CreateDeployment = OpsWorks
    type Rs CreateDeployment = CreateDeploymentResponse

    request  = post
    response = jsonResponse

instance FromJSON CreateDeploymentResponse where
    parseJSON = genericParseJSON jsonOptions
