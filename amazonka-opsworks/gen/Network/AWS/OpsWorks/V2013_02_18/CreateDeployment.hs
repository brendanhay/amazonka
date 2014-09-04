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
    , createDeployment
    -- ** Request lenses
    , cdrCommand
    , cdrStackId
    , cdrAppId
    , cdrComment
    , cdrCustomJson
    , cdrInstanceIds

    -- * Response
    , CreateDeploymentResponse
    -- ** Response lenses
    , cdsDeploymentId
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'CreateDeployment' request.
createDeployment :: DeploymentCommand -- ^ 'cdrCommand'
                 -> Text -- ^ 'cdrStackId'
                 -> CreateDeployment
createDeployment p1 p2 = CreateDeployment
    { _cdrCommand = p1
    , _cdrStackId = p2
    , _cdrAppId = Nothing
    , _cdrComment = Nothing
    , _cdrCustomJson = Nothing
    , _cdrInstanceIds = mempty
    }
{-# INLINE createDeployment #-}

data CreateDeployment = CreateDeployment
    { _cdrCommand :: DeploymentCommand
      -- ^ A DeploymentCommand object that specifies the deployment command
      -- and any associated arguments.
    , _cdrStackId :: Text
      -- ^ The stack ID.
    , _cdrAppId :: Maybe Text
      -- ^ The app ID. This parameter is required for app deployments, but
      -- not for other deployment commands.
    , _cdrComment :: Maybe Text
      -- ^ A user-defined comment.
    , _cdrCustomJson :: Maybe Text
      -- ^ A string that contains user-defined, custom JSON. It is used to
      -- override the corresponding default stack configuration JSON
      -- values. The string should be in the following format and must
      -- escape characters such as '"'.: "{\"key1\": \"value1\", \"key2\":
      -- \"value2\",...}" For more information on custom JSON, see Use
      -- Custom JSON to Modify the Stack Configuration JSON.
    , _cdrInstanceIds :: [Text]
      -- ^ The instance IDs for the deployment targets.
    } deriving (Show, Generic)

-- | A DeploymentCommand object that specifies the deployment command and any
-- associated arguments.
cdrCommand :: Lens' CreateDeployment (DeploymentCommand)
cdrCommand f x =
    f (_cdrCommand x)
        <&> \y -> x { _cdrCommand = y }
{-# INLINE cdrCommand #-}

-- | The stack ID.
cdrStackId :: Lens' CreateDeployment (Text)
cdrStackId f x =
    f (_cdrStackId x)
        <&> \y -> x { _cdrStackId = y }
{-# INLINE cdrStackId #-}

-- | The app ID. This parameter is required for app deployments, but not for
-- other deployment commands.
cdrAppId :: Lens' CreateDeployment (Maybe Text)
cdrAppId f x =
    f (_cdrAppId x)
        <&> \y -> x { _cdrAppId = y }
{-# INLINE cdrAppId #-}

-- | A user-defined comment.
cdrComment :: Lens' CreateDeployment (Maybe Text)
cdrComment f x =
    f (_cdrComment x)
        <&> \y -> x { _cdrComment = y }
{-# INLINE cdrComment #-}

-- | A string that contains user-defined, custom JSON. It is used to override
-- the corresponding default stack configuration JSON values. The string
-- should be in the following format and must escape characters such as '"'.:
-- "{\"key1\": \"value1\", \"key2\": \"value2\",...}" For more information on
-- custom JSON, see Use Custom JSON to Modify the Stack Configuration JSON.
cdrCustomJson :: Lens' CreateDeployment (Maybe Text)
cdrCustomJson f x =
    f (_cdrCustomJson x)
        <&> \y -> x { _cdrCustomJson = y }
{-# INLINE cdrCustomJson #-}

-- | The instance IDs for the deployment targets.
cdrInstanceIds :: Lens' CreateDeployment ([Text])
cdrInstanceIds f x =
    f (_cdrInstanceIds x)
        <&> \y -> x { _cdrInstanceIds = y }
{-# INLINE cdrInstanceIds #-}

instance ToPath CreateDeployment

instance ToQuery CreateDeployment

instance ToHeaders CreateDeployment

instance ToJSON CreateDeployment

data CreateDeploymentResponse = CreateDeploymentResponse
    { _cdsDeploymentId :: Maybe Text
      -- ^ The deployment ID, which can be used with other requests to
      -- identify the deployment.
    } deriving (Show, Generic)

-- | The deployment ID, which can be used with other requests to identify the
-- deployment.
cdsDeploymentId :: Lens' CreateDeploymentResponse (Maybe Text)
cdsDeploymentId f x =
    f (_cdsDeploymentId x)
        <&> \y -> x { _cdsDeploymentId = y }
{-# INLINE cdsDeploymentId #-}

instance FromJSON CreateDeploymentResponse

instance AWSRequest CreateDeployment where
    type Sv CreateDeployment = OpsWorks
    type Rs CreateDeployment = CreateDeploymentResponse

    request = get
    response _ = jsonResponse
