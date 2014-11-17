{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CodeDeploy.GetDeploymentInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets information about an Amazon EC2 instance as part of a deployment.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_GetDeploymentInstance.html>
module Network.AWS.CodeDeploy.GetDeploymentInstance
    (
    -- * Request
      GetDeploymentInstance
    -- ** Request constructor
    , getDeploymentInstance
    -- ** Request lenses
    , gdiDeploymentId
    , gdiInstanceId

    -- * Response
    , GetDeploymentInstanceResponse
    -- ** Response constructor
    , getDeploymentInstanceResponse
    -- ** Response lenses
    , gdirInstanceSummary
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CodeDeploy.Types
import qualified GHC.Exts

data GetDeploymentInstance = GetDeploymentInstance
    { _gdiDeploymentId :: Text
    , _gdiInstanceId   :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'GetDeploymentInstance' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdiDeploymentId' @::@ 'Text'
--
-- * 'gdiInstanceId' @::@ 'Text'
--
getDeploymentInstance :: Text -- ^ 'gdiDeploymentId'
                      -> Text -- ^ 'gdiInstanceId'
                      -> GetDeploymentInstance
getDeploymentInstance p1 p2 = GetDeploymentInstance
    { _gdiDeploymentId = p1
    , _gdiInstanceId   = p2
    }

-- | The unique ID of a deployment.
gdiDeploymentId :: Lens' GetDeploymentInstance Text
gdiDeploymentId = lens _gdiDeploymentId (\s a -> s { _gdiDeploymentId = a })

-- | The unique ID of an Amazon EC2 instance in the deployment's deployment
-- group.
gdiInstanceId :: Lens' GetDeploymentInstance Text
gdiInstanceId = lens _gdiInstanceId (\s a -> s { _gdiInstanceId = a })

newtype GetDeploymentInstanceResponse = GetDeploymentInstanceResponse
    { _gdirInstanceSummary :: Maybe InstanceSummary
    } deriving (Eq, Show, Generic)

-- | 'GetDeploymentInstanceResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdirInstanceSummary' @::@ 'Maybe' 'InstanceSummary'
--
getDeploymentInstanceResponse :: GetDeploymentInstanceResponse
getDeploymentInstanceResponse = GetDeploymentInstanceResponse
    { _gdirInstanceSummary = Nothing
    }

-- | Information about the instance.
gdirInstanceSummary :: Lens' GetDeploymentInstanceResponse (Maybe InstanceSummary)
gdirInstanceSummary =
    lens _gdirInstanceSummary (\s a -> s { _gdirInstanceSummary = a })

instance AWSRequest GetDeploymentInstance where
    type Sv GetDeploymentInstance = CodeDeploy
    type Rs GetDeploymentInstance = GetDeploymentInstanceResponse

    request  = post
    response = jsonResponse

instance FromJSON GetDeploymentInstanceResponse where
    parseJSON = genericParseJSON jsonOptions

instance ToPath GetDeploymentInstance where
    toPath = const "/"

instance ToHeaders GetDeploymentInstance

instance ToQuery GetDeploymentInstance where
    toQuery = const mempty

instance ToJSON GetDeploymentInstance where
    toJSON = genericToJSON jsonOptions
