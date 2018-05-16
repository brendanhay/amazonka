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
-- Module      : Network.AWS.CodeDeploy.PutLifecycleEventHookExecutionStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the result of a Lambda validation function. The function validates one or both lifecycle events (@BeforeAllowTraffic@ and @AfterAllowTraffic@ ) and returns @Succeeded@ or @Failed@ .
--
--
module Network.AWS.CodeDeploy.PutLifecycleEventHookExecutionStatus
    (
    -- * Creating a Request
      putLifecycleEventHookExecutionStatus
    , PutLifecycleEventHookExecutionStatus
    -- * Request Lenses
    , plehesStatus
    , plehesDeploymentId
    , plehesLifecycleEventHookExecutionId

    -- * Destructuring the Response
    , putLifecycleEventHookExecutionStatusResponse
    , PutLifecycleEventHookExecutionStatusResponse
    -- * Response Lenses
    , plehesrsLifecycleEventHookExecutionId
    , plehesrsResponseStatus
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.CodeDeploy.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putLifecycleEventHookExecutionStatus' smart constructor.
data PutLifecycleEventHookExecutionStatus = PutLifecycleEventHookExecutionStatus'
  { _plehesStatus                        :: !(Maybe LifecycleEventStatus)
  , _plehesDeploymentId                  :: !(Maybe Text)
  , _plehesLifecycleEventHookExecutionId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutLifecycleEventHookExecutionStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'plehesStatus' - The result of a Lambda function that validates a deployment lifecycle event (@Succeeded@ or @Failed@ ).
--
-- * 'plehesDeploymentId' - The ID of the deployment. Pass this ID to a Lambda function that validates a deployment lifecycle event.
--
-- * 'plehesLifecycleEventHookExecutionId' - The execution ID of a deployment's lifecycle hook. A deployment lifecycle hook is specified in the @hooks@ section of the AppSpec file.
putLifecycleEventHookExecutionStatus
    :: PutLifecycleEventHookExecutionStatus
putLifecycleEventHookExecutionStatus =
  PutLifecycleEventHookExecutionStatus'
    { _plehesStatus = Nothing
    , _plehesDeploymentId = Nothing
    , _plehesLifecycleEventHookExecutionId = Nothing
    }


-- | The result of a Lambda function that validates a deployment lifecycle event (@Succeeded@ or @Failed@ ).
plehesStatus :: Lens' PutLifecycleEventHookExecutionStatus (Maybe LifecycleEventStatus)
plehesStatus = lens _plehesStatus (\ s a -> s{_plehesStatus = a})

-- | The ID of the deployment. Pass this ID to a Lambda function that validates a deployment lifecycle event.
plehesDeploymentId :: Lens' PutLifecycleEventHookExecutionStatus (Maybe Text)
plehesDeploymentId = lens _plehesDeploymentId (\ s a -> s{_plehesDeploymentId = a})

-- | The execution ID of a deployment's lifecycle hook. A deployment lifecycle hook is specified in the @hooks@ section of the AppSpec file.
plehesLifecycleEventHookExecutionId :: Lens' PutLifecycleEventHookExecutionStatus (Maybe Text)
plehesLifecycleEventHookExecutionId = lens _plehesLifecycleEventHookExecutionId (\ s a -> s{_plehesLifecycleEventHookExecutionId = a})

instance AWSRequest
           PutLifecycleEventHookExecutionStatus
         where
        type Rs PutLifecycleEventHookExecutionStatus =
             PutLifecycleEventHookExecutionStatusResponse
        request = postJSON codeDeploy
        response
          = receiveJSON
              (\ s h x ->
                 PutLifecycleEventHookExecutionStatusResponse' <$>
                   (x .?> "lifecycleEventHookExecutionId") <*>
                     (pure (fromEnum s)))

instance Hashable
           PutLifecycleEventHookExecutionStatus
         where

instance NFData PutLifecycleEventHookExecutionStatus
         where

instance ToHeaders
           PutLifecycleEventHookExecutionStatus
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.PutLifecycleEventHookExecutionStatus"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutLifecycleEventHookExecutionStatus
         where
        toJSON PutLifecycleEventHookExecutionStatus'{..}
          = object
              (catMaybes
                 [("status" .=) <$> _plehesStatus,
                  ("deploymentId" .=) <$> _plehesDeploymentId,
                  ("lifecycleEventHookExecutionId" .=) <$>
                    _plehesLifecycleEventHookExecutionId])

instance ToPath PutLifecycleEventHookExecutionStatus
         where
        toPath = const "/"

instance ToQuery PutLifecycleEventHookExecutionStatus
         where
        toQuery = const mempty

-- | /See:/ 'putLifecycleEventHookExecutionStatusResponse' smart constructor.
data PutLifecycleEventHookExecutionStatusResponse = PutLifecycleEventHookExecutionStatusResponse'
  { _plehesrsLifecycleEventHookExecutionId :: !(Maybe Text)
  , _plehesrsResponseStatus                :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutLifecycleEventHookExecutionStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'plehesrsLifecycleEventHookExecutionId' - The execution ID of the lifecycle event hook. A hook is specified in the @hooks@ section of the deployment's AppSpec file.
--
-- * 'plehesrsResponseStatus' - -- | The response status code.
putLifecycleEventHookExecutionStatusResponse
    :: Int -- ^ 'plehesrsResponseStatus'
    -> PutLifecycleEventHookExecutionStatusResponse
putLifecycleEventHookExecutionStatusResponse pResponseStatus_ =
  PutLifecycleEventHookExecutionStatusResponse'
    { _plehesrsLifecycleEventHookExecutionId = Nothing
    , _plehesrsResponseStatus = pResponseStatus_
    }


-- | The execution ID of the lifecycle event hook. A hook is specified in the @hooks@ section of the deployment's AppSpec file.
plehesrsLifecycleEventHookExecutionId :: Lens' PutLifecycleEventHookExecutionStatusResponse (Maybe Text)
plehesrsLifecycleEventHookExecutionId = lens _plehesrsLifecycleEventHookExecutionId (\ s a -> s{_plehesrsLifecycleEventHookExecutionId = a})

-- | -- | The response status code.
plehesrsResponseStatus :: Lens' PutLifecycleEventHookExecutionStatusResponse Int
plehesrsResponseStatus = lens _plehesrsResponseStatus (\ s a -> s{_plehesrsResponseStatus = a})

instance NFData
           PutLifecycleEventHookExecutionStatusResponse
         where
