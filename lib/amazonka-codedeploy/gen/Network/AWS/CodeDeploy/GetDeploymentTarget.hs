{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.GetDeploymentTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a deployment target.
module Network.AWS.CodeDeploy.GetDeploymentTarget
  ( -- * Creating a Request
    getDeploymentTarget,
    GetDeploymentTarget,

    -- * Request Lenses
    gdtTargetId,
    gdtDeploymentId,

    -- * Destructuring the Response
    getDeploymentTargetResponse,
    GetDeploymentTargetResponse,

    -- * Response Lenses
    gdtrsDeploymentTarget,
    gdtrsResponseStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDeploymentTarget' smart constructor.
data GetDeploymentTarget = GetDeploymentTarget'
  { _gdtTargetId ::
      !(Maybe Text),
    _gdtDeploymentId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetDeploymentTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdtTargetId' - The unique ID of a deployment target.
--
-- * 'gdtDeploymentId' - The unique ID of a deployment.
getDeploymentTarget ::
  GetDeploymentTarget
getDeploymentTarget =
  GetDeploymentTarget'
    { _gdtTargetId = Nothing,
      _gdtDeploymentId = Nothing
    }

-- | The unique ID of a deployment target.
gdtTargetId :: Lens' GetDeploymentTarget (Maybe Text)
gdtTargetId = lens _gdtTargetId (\s a -> s {_gdtTargetId = a})

-- | The unique ID of a deployment.
gdtDeploymentId :: Lens' GetDeploymentTarget (Maybe Text)
gdtDeploymentId = lens _gdtDeploymentId (\s a -> s {_gdtDeploymentId = a})

instance AWSRequest GetDeploymentTarget where
  type Rs GetDeploymentTarget = GetDeploymentTargetResponse
  request = postJSON codeDeploy
  response =
    receiveJSON
      ( \s h x ->
          GetDeploymentTargetResponse'
            <$> (x .?> "deploymentTarget") <*> (pure (fromEnum s))
      )

instance Hashable GetDeploymentTarget

instance NFData GetDeploymentTarget

instance ToHeaders GetDeploymentTarget where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeDeploy_20141006.GetDeploymentTarget" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetDeploymentTarget where
  toJSON GetDeploymentTarget' {..} =
    object
      ( catMaybes
          [ ("targetId" .=) <$> _gdtTargetId,
            ("deploymentId" .=) <$> _gdtDeploymentId
          ]
      )

instance ToPath GetDeploymentTarget where
  toPath = const "/"

instance ToQuery GetDeploymentTarget where
  toQuery = const mempty

-- | /See:/ 'getDeploymentTargetResponse' smart constructor.
data GetDeploymentTargetResponse = GetDeploymentTargetResponse'
  { _gdtrsDeploymentTarget ::
      !(Maybe DeploymentTarget),
    _gdtrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetDeploymentTargetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdtrsDeploymentTarget' - A deployment target that contains information about a deployment such as its status, lifecycle events, and when it was last updated. It also contains metadata about the deployment target. The deployment target metadata depends on the deployment target's type (@instanceTarget@ , @lambdaTarget@ , or @ecsTarget@ ).
--
-- * 'gdtrsResponseStatus' - -- | The response status code.
getDeploymentTargetResponse ::
  -- | 'gdtrsResponseStatus'
  Int ->
  GetDeploymentTargetResponse
getDeploymentTargetResponse pResponseStatus_ =
  GetDeploymentTargetResponse'
    { _gdtrsDeploymentTarget = Nothing,
      _gdtrsResponseStatus = pResponseStatus_
    }

-- | A deployment target that contains information about a deployment such as its status, lifecycle events, and when it was last updated. It also contains metadata about the deployment target. The deployment target metadata depends on the deployment target's type (@instanceTarget@ , @lambdaTarget@ , or @ecsTarget@ ).
gdtrsDeploymentTarget :: Lens' GetDeploymentTargetResponse (Maybe DeploymentTarget)
gdtrsDeploymentTarget = lens _gdtrsDeploymentTarget (\s a -> s {_gdtrsDeploymentTarget = a})

-- | -- | The response status code.
gdtrsResponseStatus :: Lens' GetDeploymentTargetResponse Int
gdtrsResponseStatus = lens _gdtrsResponseStatus (\s a -> s {_gdtrsResponseStatus = a})

instance NFData GetDeploymentTargetResponse
