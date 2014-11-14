{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.CodeDeploy.GetDeployment
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets information about a deployment.
module Network.AWS.CodeDeploy.GetDeployment
    (
    -- * Request
      GetDeployment
    -- ** Request constructor
    , getDeployment
    -- ** Request lenses
    , gdDeploymentId

    -- * Response
    , GetDeploymentResponse
    -- ** Response constructor
    , getDeploymentResponse
    -- ** Response lenses
    , gdrDeploymentInfo
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CodeDeploy.Types
import qualified GHC.Exts

newtype GetDeployment = GetDeployment
    { _gdDeploymentId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'GetDeployment' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdDeploymentId' @::@ 'Text'
--
getDeployment :: Text -- ^ 'gdDeploymentId'
              -> GetDeployment
getDeployment p1 = GetDeployment
    { _gdDeploymentId = p1
    }

-- | An existing deployment ID within the AWS user account.
gdDeploymentId :: Lens' GetDeployment Text
gdDeploymentId = lens _gdDeploymentId (\s a -> s { _gdDeploymentId = a })

instance ToPath GetDeployment where
    toPath = const "/"

instance ToQuery GetDeployment where
    toQuery = const mempty

instance ToHeaders GetDeployment

instance ToBody GetDeployment where
    toBody = toBody . encode . _gdDeploymentId

newtype GetDeploymentResponse = GetDeploymentResponse
    { _gdrDeploymentInfo :: Maybe DeploymentInfo
    } deriving (Eq, Show, Generic)

-- | 'GetDeploymentResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdrDeploymentInfo' @::@ 'Maybe' 'DeploymentInfo'
--
getDeploymentResponse :: GetDeploymentResponse
getDeploymentResponse = GetDeploymentResponse
    { _gdrDeploymentInfo = Nothing
    }

-- | Information about the deployment.
gdrDeploymentInfo :: Lens' GetDeploymentResponse (Maybe DeploymentInfo)
gdrDeploymentInfo =
    lens _gdrDeploymentInfo (\s a -> s { _gdrDeploymentInfo = a })

instance AWSRequest GetDeployment where
    type Sv GetDeployment = CodeDeploy
    type Rs GetDeployment = GetDeploymentResponse

    request  = post
    response = jsonResponse $ \h o -> GetDeploymentResponse
        <$> o .: "deploymentInfo"
