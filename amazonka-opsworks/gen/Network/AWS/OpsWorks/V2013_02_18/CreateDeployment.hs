{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.CreateDeployment
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
module Network.AWS.OpsWorks.V2013_02_18.CreateDeployment
    (
    -- * Request
      CreateDeployment
    -- ** Request constructor
    , mkCreateDeployment
    -- ** Request lenses
    , cdStackId
    , cdAppId
    , cdInstanceIds
    , cdCommand
    , cdComment
    , cdCustomJson

    -- * Response
    , CreateDeploymentResponse
    -- ** Response lenses
    , cdrDeploymentId
    ) where

import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data CreateDeployment = CreateDeployment
    { _cdStackId :: Text
    , _cdAppId :: Maybe Text
    , _cdInstanceIds :: [Text]
    , _cdCommand :: DeploymentCommand
    , _cdComment :: Maybe Text
    , _cdCustomJson :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateDeployment' request.
mkCreateDeployment :: Text -- ^ 'cdStackId'
                   -> DeploymentCommand -- ^ 'cdCommand'
                   -> CreateDeployment
mkCreateDeployment p1 p4 = CreateDeployment
    { _cdStackId = p1
    , _cdAppId = Nothing
    , _cdInstanceIds = mempty
    , _cdCommand = p4
    , _cdComment = Nothing
    , _cdCustomJson = Nothing
    }

-- | The stack ID.
cdStackId :: Lens' CreateDeployment Text
cdStackId = lens _cdStackId (\s a -> s { _cdStackId = a })

-- | The app ID. This parameter is required for app deployments, but not for
-- other deployment commands.
cdAppId :: Lens' CreateDeployment (Maybe Text)
cdAppId = lens _cdAppId (\s a -> s { _cdAppId = a })

-- | The instance IDs for the deployment targets.
cdInstanceIds :: Lens' CreateDeployment [Text]
cdInstanceIds = lens _cdInstanceIds (\s a -> s { _cdInstanceIds = a })

-- | A DeploymentCommand object that specifies the deployment command and any
-- associated arguments.
cdCommand :: Lens' CreateDeployment DeploymentCommand
cdCommand = lens _cdCommand (\s a -> s { _cdCommand = a })

-- | A user-defined comment.
cdComment :: Lens' CreateDeployment (Maybe Text)
cdComment = lens _cdComment (\s a -> s { _cdComment = a })

-- | A string that contains user-defined, custom JSON. It is used to override
-- the corresponding default stack configuration JSON values. The string
-- should be in the following format and must escape characters such as '"'.:
-- "{\"key1\": \"value1\", \"key2\": \"value2\",...}" For more information on
-- custom JSON, see Use Custom JSON to Modify the Stack Configuration JSON.
cdCustomJson :: Lens' CreateDeployment (Maybe Text)
cdCustomJson = lens _cdCustomJson (\s a -> s { _cdCustomJson = a })

instance ToPath CreateDeployment

instance ToQuery CreateDeployment

instance ToHeaders CreateDeployment

instance ToJSON CreateDeployment

-- | Contains the response to a CreateDeployment request.
newtype CreateDeploymentResponse = CreateDeploymentResponse
    { _cdrDeploymentId :: Maybe Text
    } deriving (Show, Generic)

-- | The deployment ID, which can be used with other requests to identify the
-- deployment.
cdrDeploymentId :: Lens' CreateDeploymentResponse (Maybe Text)
cdrDeploymentId = lens _cdrDeploymentId (\s a -> s { _cdrDeploymentId = a })

instance FromJSON CreateDeploymentResponse

instance AWSRequest CreateDeployment where
    type Sv CreateDeployment = OpsWorks
    type Rs CreateDeployment = CreateDeploymentResponse

    request = get
    response _ = jsonResponse
