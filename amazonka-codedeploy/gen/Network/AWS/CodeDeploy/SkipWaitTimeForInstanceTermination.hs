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
-- Module      : Network.AWS.CodeDeploy.SkipWaitTimeForInstanceTermination
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- In a blue/green deployment, overrides any specified wait time and starts terminating instances immediately after the traffic routing is completed.
--
--
module Network.AWS.CodeDeploy.SkipWaitTimeForInstanceTermination
    (
    -- * Creating a Request
      skipWaitTimeForInstanceTermination
    , SkipWaitTimeForInstanceTermination
    -- * Request Lenses
    , swtfitDeploymentId

    -- * Destructuring the Response
    , skipWaitTimeForInstanceTerminationResponse
    , SkipWaitTimeForInstanceTerminationResponse
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.CodeDeploy.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'skipWaitTimeForInstanceTermination' smart constructor.
newtype SkipWaitTimeForInstanceTermination = SkipWaitTimeForInstanceTermination'
  { _swtfitDeploymentId :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SkipWaitTimeForInstanceTermination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'swtfitDeploymentId' - The ID of the blue/green deployment for which you want to skip the instance termination wait time.
skipWaitTimeForInstanceTermination
    :: SkipWaitTimeForInstanceTermination
skipWaitTimeForInstanceTermination =
  SkipWaitTimeForInstanceTermination' {_swtfitDeploymentId = Nothing}


-- | The ID of the blue/green deployment for which you want to skip the instance termination wait time.
swtfitDeploymentId :: Lens' SkipWaitTimeForInstanceTermination (Maybe Text)
swtfitDeploymentId = lens _swtfitDeploymentId (\ s a -> s{_swtfitDeploymentId = a})

instance AWSRequest
           SkipWaitTimeForInstanceTermination
         where
        type Rs SkipWaitTimeForInstanceTermination =
             SkipWaitTimeForInstanceTerminationResponse
        request = postJSON codeDeploy
        response
          = receiveNull
              SkipWaitTimeForInstanceTerminationResponse'

instance Hashable SkipWaitTimeForInstanceTermination
         where

instance NFData SkipWaitTimeForInstanceTermination
         where

instance ToHeaders SkipWaitTimeForInstanceTermination
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.SkipWaitTimeForInstanceTermination"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SkipWaitTimeForInstanceTermination
         where
        toJSON SkipWaitTimeForInstanceTermination'{..}
          = object
              (catMaybes
                 [("deploymentId" .=) <$> _swtfitDeploymentId])

instance ToPath SkipWaitTimeForInstanceTermination
         where
        toPath = const "/"

instance ToQuery SkipWaitTimeForInstanceTermination
         where
        toQuery = const mempty

-- | /See:/ 'skipWaitTimeForInstanceTerminationResponse' smart constructor.
data SkipWaitTimeForInstanceTerminationResponse =
  SkipWaitTimeForInstanceTerminationResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SkipWaitTimeForInstanceTerminationResponse' with the minimum fields required to make a request.
--
skipWaitTimeForInstanceTerminationResponse
    :: SkipWaitTimeForInstanceTerminationResponse
skipWaitTimeForInstanceTerminationResponse =
  SkipWaitTimeForInstanceTerminationResponse'


instance NFData
           SkipWaitTimeForInstanceTerminationResponse
         where
