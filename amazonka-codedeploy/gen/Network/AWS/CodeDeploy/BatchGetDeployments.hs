{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.BatchGetDeployments
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more deployments.
--
-- /See:/ <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_BatchGetDeployments.html AWS API Reference> for BatchGetDeployments.
module Network.AWS.CodeDeploy.BatchGetDeployments
    (
    -- * Creating a Request
      BatchGetDeployments
    , batchGetDeployments
    -- * Request Lenses
    , bgdDeploymentIds

    -- * Destructuring the Response
    , BatchGetDeploymentsResponse
    , batchGetDeploymentsResponse
    -- * Response Lenses
    , bgdrsDeploymentsInfo
    , bgdrsStatus
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.CodeDeploy.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

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
bgdDeploymentIds = lens _bgdDeploymentIds (\ s a -> s{_bgdDeploymentIds = a}) . _Default . _Coerce;

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
-- * 'bgdrsDeploymentsInfo'
--
-- * 'bgdrsStatus'
data BatchGetDeploymentsResponse = BatchGetDeploymentsResponse'
    { _bgdrsDeploymentsInfo :: !(Maybe [DeploymentInfo])
    , _bgdrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'BatchGetDeploymentsResponse' smart constructor.
batchGetDeploymentsResponse :: Int -> BatchGetDeploymentsResponse
batchGetDeploymentsResponse pStatus_ = 
    BatchGetDeploymentsResponse'
    { _bgdrsDeploymentsInfo = Nothing
    , _bgdrsStatus = pStatus_
    }

-- | Information about the deployments.
bgdrsDeploymentsInfo :: Lens' BatchGetDeploymentsResponse [DeploymentInfo]
bgdrsDeploymentsInfo = lens _bgdrsDeploymentsInfo (\ s a -> s{_bgdrsDeploymentsInfo = a}) . _Default . _Coerce;

-- | Undocumented member.
bgdrsStatus :: Lens' BatchGetDeploymentsResponse Int
bgdrsStatus = lens _bgdrsStatus (\ s a -> s{_bgdrsStatus = a});
