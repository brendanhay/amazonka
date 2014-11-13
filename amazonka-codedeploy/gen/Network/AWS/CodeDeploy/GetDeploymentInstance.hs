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
import Network.AWS.Request
import Network.AWS.CodeDeploy.Types

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

instance ToPath GetDeploymentInstance where
    toPath = const "/"

instance ToQuery GetDeploymentInstance where
    toQuery = const mempty

instance ToHeaders GetDeploymentInstance

instance ToBody GetDeploymentInstance where
    toBody = toBody . encode . _gdiDeploymentId

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

-- FromJSON

instance AWSRequest GetDeploymentInstance where
    type Sv GetDeploymentInstance = CodeDeploy
    type Rs GetDeploymentInstance = GetDeploymentInstanceResponse

    request  = post'
    response = jsonResponse $ \h o -> GetDeploymentInstanceResponse
        <$> o .: "instanceSummary"
