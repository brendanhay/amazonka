{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CodeDeploy.StopDeployment
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
    , sdrStatusMessage
    , sdrStatus
    ) where

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a stop deployment operation.
--
-- /See:/ 'stopDeployment' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sdDeploymentId'
newtype StopDeployment = StopDeployment'
    { _sdDeploymentId :: Text
    } deriving (Eq,Read,Show)

-- | 'StopDeployment' smart constructor.
stopDeployment :: Text -> StopDeployment
stopDeployment pDeploymentId =
    StopDeployment'
    { _sdDeploymentId = pDeploymentId
    }

-- | The unique ID of a deployment.
sdDeploymentId :: Lens' StopDeployment Text
sdDeploymentId = lens _sdDeploymentId (\ s a -> s{_sdDeploymentId = a});

instance AWSRequest StopDeployment where
        type Sv StopDeployment = CodeDeploy
        type Rs StopDeployment = StopDeploymentResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 StopDeploymentResponse' <$>
                   (x .?> "statusMessage") <*> (pure s))

instance ToHeaders StopDeployment where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.StopDeployment" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopDeployment where
        toJSON StopDeployment'{..}
          = object ["deploymentId" .= _sdDeploymentId]

instance ToPath StopDeployment where
        toPath = const "/"

instance ToQuery StopDeployment where
        toQuery = const mempty

-- | Represents the output of a stop deployment operation.
--
-- /See:/ 'stopDeploymentResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sdrStatusMessage'
--
-- * 'sdrStatus'
data StopDeploymentResponse = StopDeploymentResponse'
    { _sdrStatusMessage :: !(Maybe Text)
    , _sdrStatus        :: !Status
    } deriving (Eq,Show)

-- | 'StopDeploymentResponse' smart constructor.
stopDeploymentResponse :: Status -> StopDeploymentResponse
stopDeploymentResponse pStatus =
    StopDeploymentResponse'
    { _sdrStatusMessage = Nothing
    , _sdrStatus = pStatus
    }

-- | An accompanying status message.
sdrStatusMessage :: Lens' StopDeploymentResponse (Maybe Text)
sdrStatusMessage = lens _sdrStatusMessage (\ s a -> s{_sdrStatusMessage = a});

-- | FIXME: Undocumented member.
sdrStatus :: Lens' StopDeploymentResponse Status
sdrStatus = lens _sdrStatus (\ s a -> s{_sdrStatus = a});
