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
-- Module      : Network.AWS.CodeDeploy.ContinueDeployment
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For a blue/green deployment, starts the process of rerouting traffic from instances in the original environment to instances in the replacement environment without waiting for a specified wait time to elapse. (Traffic rerouting, which is achieved by registering instances in the replacement environment with the load balancer, can start as soon as all instances have a status of Ready.)
--
--
module Network.AWS.CodeDeploy.ContinueDeployment
    (
    -- * Creating a Request
      continueDeployment
    , ContinueDeployment
    -- * Request Lenses
    , cdDeploymentId

    -- * Destructuring the Response
    , continueDeploymentResponse
    , ContinueDeploymentResponse
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.CodeDeploy.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'continueDeployment' smart constructor.
newtype ContinueDeployment = ContinueDeployment'
  { _cdDeploymentId :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ContinueDeployment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdDeploymentId' - The deployment ID of the blue/green deployment for which you want to start rerouting traffic to the replacement environment.
continueDeployment
    :: ContinueDeployment
continueDeployment = ContinueDeployment' {_cdDeploymentId = Nothing}


-- | The deployment ID of the blue/green deployment for which you want to start rerouting traffic to the replacement environment.
cdDeploymentId :: Lens' ContinueDeployment (Maybe Text)
cdDeploymentId = lens _cdDeploymentId (\ s a -> s{_cdDeploymentId = a})

instance AWSRequest ContinueDeployment where
        type Rs ContinueDeployment =
             ContinueDeploymentResponse
        request = postJSON codeDeploy
        response = receiveNull ContinueDeploymentResponse'

instance Hashable ContinueDeployment where

instance NFData ContinueDeployment where

instance ToHeaders ContinueDeployment where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.ContinueDeployment" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ContinueDeployment where
        toJSON ContinueDeployment'{..}
          = object
              (catMaybes [("deploymentId" .=) <$> _cdDeploymentId])

instance ToPath ContinueDeployment where
        toPath = const "/"

instance ToQuery ContinueDeployment where
        toQuery = const mempty

-- | /See:/ 'continueDeploymentResponse' smart constructor.
data ContinueDeploymentResponse =
  ContinueDeploymentResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ContinueDeploymentResponse' with the minimum fields required to make a request.
--
continueDeploymentResponse
    :: ContinueDeploymentResponse
continueDeploymentResponse = ContinueDeploymentResponse'


instance NFData ContinueDeploymentResponse where
