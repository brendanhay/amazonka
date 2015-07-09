{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.BatchGetDeployments
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
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
    , bgdrStatus
    ) where

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a batch get deployments operation.
--
-- /See:/ 'batchGetDeployments' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bgdDeploymentIds'
newtype BatchGetDeployments = BatchGetDeployments'
    { _bgdDeploymentIds :: Maybe [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'BatchGetDeployments' smart constructor.
batchGetDeployments :: BatchGetDeployments
batchGetDeployments =
    BatchGetDeployments'
    { _bgdDeploymentIds = Nothing
    }

-- | A list of deployment IDs, with multiple deployment IDs separated by
-- spaces.
bgdDeploymentIds :: Lens' BatchGetDeployments [Text]
bgdDeploymentIds = lens _bgdDeploymentIds (\ s a -> s{_bgdDeploymentIds = a}) . _Default;

instance AWSRequest BatchGetDeployments where
        type Sv BatchGetDeployments = CodeDeploy
        type Rs BatchGetDeployments =
             BatchGetDeploymentsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 BatchGetDeploymentsResponse' <$>
                   (x .?> "deploymentsInfo" .!@ mempty) <*>
                     (pure (fromEnum s)))

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

-- | Represents the output of a batch get deployments operation.
--
-- /See:/ 'batchGetDeploymentsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bgdrDeploymentsInfo'
--
-- * 'bgdrStatus'
data BatchGetDeploymentsResponse = BatchGetDeploymentsResponse'
    { _bgdrDeploymentsInfo :: !(Maybe [DeploymentInfo])
    , _bgdrStatus          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'BatchGetDeploymentsResponse' smart constructor.
batchGetDeploymentsResponse :: Int -> BatchGetDeploymentsResponse
batchGetDeploymentsResponse pStatus =
    BatchGetDeploymentsResponse'
    { _bgdrDeploymentsInfo = Nothing
    , _bgdrStatus = pStatus
    }

-- | Information about the deployments.
bgdrDeploymentsInfo :: Lens' BatchGetDeploymentsResponse [DeploymentInfo]
bgdrDeploymentsInfo = lens _bgdrDeploymentsInfo (\ s a -> s{_bgdrDeploymentsInfo = a}) . _Default;

-- | FIXME: Undocumented member.
bgdrStatus :: Lens' BatchGetDeploymentsResponse Int
bgdrStatus = lens _bgdrStatus (\ s a -> s{_bgdrStatus = a});
