{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.StopDeployment
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Attempts to stop an ongoing deployment.
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
    , sdrsStatusMessage
    , sdrsStatus
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StopDeployment' smart constructor.
stopDeployment :: Text -> StopDeployment
stopDeployment pDeploymentId_ =
    StopDeployment'
    { _sdDeploymentId = pDeploymentId_
    }

-- | The unique ID of a deployment.
sdDeploymentId :: Lens' StopDeployment Text
sdDeploymentId = lens _sdDeploymentId (\ s a -> s{_sdDeploymentId = a});

instance AWSRequest StopDeployment where
        type Sv StopDeployment = CodeDeploy
        type Rs StopDeployment = StopDeploymentResponse
        request = postJSON "StopDeployment"
        response
          = receiveJSON
              (\ s h x ->
                 StopDeploymentResponse' <$>
                   (x .?> "statusMessage") <*> (pure (fromEnum s)))

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
-- * 'sdrsStatusMessage'
--
-- * 'sdrsStatus'
data StopDeploymentResponse = StopDeploymentResponse'
    { _sdrsStatusMessage :: !(Maybe Text)
    , _sdrsStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StopDeploymentResponse' smart constructor.
stopDeploymentResponse :: Int -> StopDeploymentResponse
stopDeploymentResponse pStatus_ =
    StopDeploymentResponse'
    { _sdrsStatusMessage = Nothing
    , _sdrsStatus = pStatus_
    }

-- | An accompanying status message.
sdrsStatusMessage :: Lens' StopDeploymentResponse (Maybe Text)
sdrsStatusMessage = lens _sdrsStatusMessage (\ s a -> s{_sdrsStatusMessage = a});

-- | FIXME: Undocumented member.
sdrsStatus :: Lens' StopDeploymentResponse Int
sdrsStatus = lens _sdrsStatus (\ s a -> s{_sdrsStatus = a});
