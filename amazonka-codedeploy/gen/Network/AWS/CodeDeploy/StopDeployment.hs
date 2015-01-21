{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CodeDeploy.StopDeployment
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Attempts to stop an ongoing deployment.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_StopDeployment.html>
module Network.AWS.CodeDeploy.StopDeployment
    (
    -- * Request
      StopDeployment
    -- ** Request constructor
    , stopDeployment
    -- ** Request lenses
    , sdDeploymentId

    -- * Response
    , StopDeploymentResponse
    -- ** Response constructor
    , stopDeploymentResponse
    -- ** Response lenses
    , sdrStatus
    , sdrStatusMessage
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CodeDeploy.Types
import qualified GHC.Exts

newtype StopDeployment = StopDeployment
    { _sdDeploymentId :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'StopDeployment' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sdDeploymentId' @::@ 'Text'
--
stopDeployment :: Text -- ^ 'sdDeploymentId'
               -> StopDeployment
stopDeployment p1 = StopDeployment
    { _sdDeploymentId = p1
    }

-- | The unique ID of a deployment.
sdDeploymentId :: Lens' StopDeployment Text
sdDeploymentId = lens _sdDeploymentId (\s a -> s { _sdDeploymentId = a })

data StopDeploymentResponse = StopDeploymentResponse
    { _sdrStatus        :: Maybe StopStatus
    , _sdrStatusMessage :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'StopDeploymentResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sdrStatus' @::@ 'Maybe' 'StopStatus'
--
-- * 'sdrStatusMessage' @::@ 'Maybe' 'Text'
--
stopDeploymentResponse :: StopDeploymentResponse
stopDeploymentResponse = StopDeploymentResponse
    { _sdrStatus        = Nothing
    , _sdrStatusMessage = Nothing
    }

-- | The status of the stop deployment operation:
--
-- Pending: The stop operation is pending. Succeeded: The stop operation
-- succeeded.
sdrStatus :: Lens' StopDeploymentResponse (Maybe StopStatus)
sdrStatus = lens _sdrStatus (\s a -> s { _sdrStatus = a })

-- | An accompanying status message.
sdrStatusMessage :: Lens' StopDeploymentResponse (Maybe Text)
sdrStatusMessage = lens _sdrStatusMessage (\s a -> s { _sdrStatusMessage = a })

instance ToPath StopDeployment where
    toPath = const "/"

instance ToQuery StopDeployment where
    toQuery = const mempty

instance ToHeaders StopDeployment

instance ToJSON StopDeployment where
    toJSON StopDeployment{..} = object
        [ "deploymentId" .= _sdDeploymentId
        ]

instance AWSRequest StopDeployment where
    type Sv StopDeployment = CodeDeploy
    type Rs StopDeployment = StopDeploymentResponse

    request  = post "StopDeployment"
    response = jsonResponse

instance FromJSON StopDeploymentResponse where
    parseJSON = withObject "StopDeploymentResponse" $ \o -> StopDeploymentResponse
        <$> o .:? "status"
        <*> o .:? "statusMessage"
