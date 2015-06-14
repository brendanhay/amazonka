{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CodeDeploy.BatchGetDeployments
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

-- | Gets information about one or more deployments.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_BatchGetDeployments.html>
module Network.AWS.CodeDeploy.BatchGetDeployments
    (
    -- * Request
      BatchGetDeployments
    -- ** Request constructor
    , batchGetDeployments
    -- ** Request lenses
    , bgdDeploymentIds

    -- * Response
    , BatchGetDeploymentsResponse
    -- ** Response constructor
    , batchGetDeploymentsResponse
    -- ** Response lenses
    , bgdrDeploymentsInfo
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CodeDeploy.Types

-- | /See:/ 'batchGetDeployments' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bgdDeploymentIds'
newtype BatchGetDeployments = BatchGetDeployments'{_bgdDeploymentIds :: [Text]} deriving (Eq, Read, Show)

-- | 'BatchGetDeployments' smart constructor.
batchGetDeployments :: BatchGetDeployments
batchGetDeployments = BatchGetDeployments'{_bgdDeploymentIds = mempty};

-- | A list of deployment IDs, with multiple deployment IDs separated by
-- spaces.
bgdDeploymentIds :: Lens' BatchGetDeployments [Text]
bgdDeploymentIds = lens _bgdDeploymentIds (\ s a -> s{_bgdDeploymentIds = a});

instance AWSRequest BatchGetDeployments where
        type Sv BatchGetDeployments = CodeDeploy
        type Rs BatchGetDeployments =
             BatchGetDeploymentsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 BatchGetDeploymentsResponse' <$>
                   x .?> "deploymentsInfo" .!@ mempty)

instance ToHeaders BatchGetDeployments where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.BatchGetDeployments" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON BatchGetDeployments where
        toJSON BatchGetDeployments'{..}
          = object ["deploymentIds" .= _bgdDeploymentIds]

instance ToPath BatchGetDeployments where
        toPath = const "/"

instance ToQuery BatchGetDeployments where
        toQuery = const mempty

-- | /See:/ 'batchGetDeploymentsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bgdrDeploymentsInfo'
newtype BatchGetDeploymentsResponse = BatchGetDeploymentsResponse'{_bgdrDeploymentsInfo :: [DeploymentInfo]} deriving (Eq, Read, Show)

-- | 'BatchGetDeploymentsResponse' smart constructor.
batchGetDeploymentsResponse :: BatchGetDeploymentsResponse
batchGetDeploymentsResponse = BatchGetDeploymentsResponse'{_bgdrDeploymentsInfo = mempty};

-- | Information about the deployments.
bgdrDeploymentsInfo :: Lens' BatchGetDeploymentsResponse [DeploymentInfo]
bgdrDeploymentsInfo = lens _bgdrDeploymentsInfo (\ s a -> s{_bgdrDeploymentsInfo = a});
