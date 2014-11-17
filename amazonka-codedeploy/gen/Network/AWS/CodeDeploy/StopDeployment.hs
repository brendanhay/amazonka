{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
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

-- | Attempts to stop an ongoing deployment.
--
-- <StopDeployment.html>
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
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

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
    { _sdrStatus        :: Maybe Text
    , _sdrStatusMessage :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'StopDeploymentResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sdrStatus' @::@ 'Maybe' 'Text'
--
-- * 'sdrStatusMessage' @::@ 'Maybe' 'Text'
--
stopDeploymentResponse :: StopDeploymentResponse
stopDeploymentResponse = StopDeploymentResponse
    { _sdrStatus        = Nothing
    , _sdrStatusMessage = Nothing
    }

-- | The status of the stop deployment operation: Pending: The stop operation
-- is pending. Succeeded: The stop operation succeeded.
sdrStatus :: Lens' StopDeploymentResponse (Maybe Text)
sdrStatus = lens _sdrStatus (\s a -> s { _sdrStatus = a })

-- | An accompanying status message.
sdrStatusMessage :: Lens' StopDeploymentResponse (Maybe Text)
sdrStatusMessage = lens _sdrStatusMessage (\s a -> s { _sdrStatusMessage = a })

instance AWSRequest StopDeployment where
    type Sv StopDeployment = CodeDeploy
    type Rs StopDeployment = StopDeploymentResponse

    request  = post
    response = jsonResponse

instance FromJSON StopDeploymentResponse where
    parseJSON = genericParseJSON jsonOptions

instance ToPath StopDeployment where
    toPath = const "/"

instance ToHeaders StopDeployment

instance ToQuery StopDeployment where
    toQuery = const mempty

instance ToJSON StopDeployment where
    toJSON = genericToJSON jsonOptions
