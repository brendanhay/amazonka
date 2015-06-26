{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CodeDeploy.GetDeployment
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

-- | Gets information about a deployment.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_GetDeployment.html>
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
    , gdrStatusCode
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a get deployment operation.
--
-- /See:/ 'getDeployment' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdDeploymentId'
newtype GetDeployment = GetDeployment'{_gdDeploymentId :: Text} deriving (Eq, Read, Show)

-- | 'GetDeployment' smart constructor.
getDeployment :: Text -> GetDeployment
getDeployment pDeploymentId = GetDeployment'{_gdDeploymentId = pDeploymentId};

-- | An existing deployment ID associated with the applicable IAM user or AWS
-- account.
gdDeploymentId :: Lens' GetDeployment Text
gdDeploymentId = lens _gdDeploymentId (\ s a -> s{_gdDeploymentId = a});

instance AWSRequest GetDeployment where
        type Sv GetDeployment = CodeDeploy
        type Rs GetDeployment = GetDeploymentResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetDeploymentResponse' <$>
                   (x .?> "deploymentInfo") <*> (pure (fromEnum s)))

instance ToHeaders GetDeployment where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.GetDeployment" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDeployment where
        toJSON GetDeployment'{..}
          = object ["deploymentId" .= _gdDeploymentId]

instance ToPath GetDeployment where
        toPath = const "/"

instance ToQuery GetDeployment where
        toQuery = const mempty

-- | Represents the output of a get deployment operation.
--
-- /See:/ 'getDeploymentResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdrDeploymentInfo'
--
-- * 'gdrStatusCode'
data GetDeploymentResponse = GetDeploymentResponse'{_gdrDeploymentInfo :: Maybe DeploymentInfo, _gdrStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'GetDeploymentResponse' smart constructor.
getDeploymentResponse :: Int -> GetDeploymentResponse
getDeploymentResponse pStatusCode = GetDeploymentResponse'{_gdrDeploymentInfo = Nothing, _gdrStatusCode = pStatusCode};

-- | Information about the deployment.
gdrDeploymentInfo :: Lens' GetDeploymentResponse (Maybe DeploymentInfo)
gdrDeploymentInfo = lens _gdrDeploymentInfo (\ s a -> s{_gdrDeploymentInfo = a});

-- | FIXME: Undocumented member.
gdrStatusCode :: Lens' GetDeploymentResponse Int
gdrStatusCode = lens _gdrStatusCode (\ s a -> s{_gdrStatusCode = a});
