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
    , mkCreateDeploymentRequest
    -- ** Request lenses
    , cdrStackId
    , cdrAppId
    , cdrInstanceIds
    , cdrCommand
    , cdrComment
    , cdrCustomJson

    -- * Response
    , CreateDeploymentResponse
    -- ** Response lenses
    , cdsDeploymentId
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateDeployment' request.
mkCreateDeploymentRequest :: Text -- ^ 'cdrStackId'
                          -> DeploymentCommand -- ^ 'cdrCommand'
                          -> CreateDeployment
mkCreateDeploymentRequest p1 p2 = CreateDeployment
    { _cdrStackId = p1
    , _cdrAppId = Nothing
    , _cdrInstanceIds = mempty
    , _cdrCommand = p4
    , _cdrComment = Nothing
    , _cdrCustomJson = Nothing
    }
{-# INLINE mkCreateDeploymentRequest #-}

data CreateDeployment = CreateDeployment
    { _cdrStackId :: Text
      -- ^ The stack ID.
    , _cdrAppId :: Maybe Text
      -- ^ The app ID. This parameter is required for app deployments, but
      -- not for other deployment commands.
    , _cdrInstanceIds :: [Text]
      -- ^ The instance IDs for the deployment targets.
    , _cdrCommand :: DeploymentCommand
      -- ^ A DeploymentCommand object that specifies the deployment command
      -- and any associated arguments.
    , _cdrComment :: Maybe Text
      -- ^ A user-defined comment.
    , _cdrCustomJson :: Maybe Text
      -- ^ A string that contains user-defined, custom JSON. It is used to
      -- override the corresponding default stack configuration JSON
      -- values. The string should be in the following format and must
      -- escape characters such as '"'.: "{\"key1\": \"value1\", \"key2\":
      -- \"value2\",...}" For more information on custom JSON, see Use
      -- Custom JSON to Modify the Stack Configuration JSON.
    } deriving (Show, Generic)

-- | The stack ID.
cdrStackId :: Lens' CreateDeployment (Text)
cdrStackId = lens _cdrStackId (\s a -> s { _cdrStackId = a })
{-# INLINE cdrStackId #-}

-- | The app ID. This parameter is required for app deployments, but not for
-- other deployment commands.
cdrAppId :: Lens' CreateDeployment (Maybe Text)
cdrAppId = lens _cdrAppId (\s a -> s { _cdrAppId = a })
{-# INLINE cdrAppId #-}

-- | The instance IDs for the deployment targets.
cdrInstanceIds :: Lens' CreateDeployment ([Text])
cdrInstanceIds = lens _cdrInstanceIds (\s a -> s { _cdrInstanceIds = a })
{-# INLINE cdrInstanceIds #-}

-- | A DeploymentCommand object that specifies the deployment command and any
-- associated arguments.
cdrCommand :: Lens' CreateDeployment (DeploymentCommand)
cdrCommand = lens _cdrCommand (\s a -> s { _cdrCommand = a })
{-# INLINE cdrCommand #-}

-- | A user-defined comment.
cdrComment :: Lens' CreateDeployment (Maybe Text)
cdrComment = lens _cdrComment (\s a -> s { _cdrComment = a })
{-# INLINE cdrComment #-}

-- | A string that contains user-defined, custom JSON. It is used to override
-- the corresponding default stack configuration JSON values. The string
-- should be in the following format and must escape characters such as '"'.:
-- "{\"key1\": \"value1\", \"key2\": \"value2\",...}" For more information on
-- custom JSON, see Use Custom JSON to Modify the Stack Configuration JSON.
cdrCustomJson :: Lens' CreateDeployment (Maybe Text)
cdrCustomJson = lens _cdrCustomJson (\s a -> s { _cdrCustomJson = a })
{-# INLINE cdrCustomJson #-}

instance ToPath CreateDeployment

instance ToQuery CreateDeployment

instance ToHeaders CreateDeployment

instance ToJSON CreateDeployment

newtype CreateDeploymentResponse = CreateDeploymentResponse
    { _cdsDeploymentId :: Maybe Text
      -- ^ The deployment ID, which can be used with other requests to
      -- identify the deployment.
    } deriving (Show, Generic)

-- | The deployment ID, which can be used with other requests to identify the
-- deployment.
cdsDeploymentId :: Lens' CreateDeploymentResponse (Maybe Text)
cdsDeploymentId = lens _cdsDeploymentId (\s a -> s { _cdsDeploymentId = a })
{-# INLINE cdsDeploymentId #-}

instance FromJSON CreateDeploymentResponse

instance AWSRequest CreateDeployment where
    type Sv CreateDeployment = OpsWorks
    type Rs CreateDeployment = CreateDeploymentResponse

    request = get
    response _ = jsonResponse
