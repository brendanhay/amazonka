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
-- Module      : Network.AWS.Greengrass.StopBulkDeployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the execution of a bulk deployment. This action returns a status of ''Stopping'' until the deployment is stopped. You cannot start a new bulk deployment while a previous deployment is in the ''Stopping'' state. This action doesn't rollback completed deployments or cancel pending deployments.
module Network.AWS.Greengrass.StopBulkDeployment
  ( -- * Creating a Request
    stopBulkDeployment,
    StopBulkDeployment,

    -- * Request Lenses
    sbdBulkDeploymentId,

    -- * Destructuring the Response
    stopBulkDeploymentResponse,
    StopBulkDeploymentResponse,

    -- * Response Lenses
    sbdrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopBulkDeployment' smart constructor.
newtype StopBulkDeployment = StopBulkDeployment'
  { _sbdBulkDeploymentId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopBulkDeployment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sbdBulkDeploymentId' - The ID of the bulk deployment.
stopBulkDeployment ::
  -- | 'sbdBulkDeploymentId'
  Text ->
  StopBulkDeployment
stopBulkDeployment pBulkDeploymentId_ =
  StopBulkDeployment' {_sbdBulkDeploymentId = pBulkDeploymentId_}

-- | The ID of the bulk deployment.
sbdBulkDeploymentId :: Lens' StopBulkDeployment Text
sbdBulkDeploymentId = lens _sbdBulkDeploymentId (\s a -> s {_sbdBulkDeploymentId = a})

instance AWSRequest StopBulkDeployment where
  type Rs StopBulkDeployment = StopBulkDeploymentResponse
  request = putJSON greengrass
  response =
    receiveEmpty
      (\s h x -> StopBulkDeploymentResponse' <$> (pure (fromEnum s)))

instance Hashable StopBulkDeployment

instance NFData StopBulkDeployment

instance ToHeaders StopBulkDeployment where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON StopBulkDeployment where
  toJSON = const (Object mempty)

instance ToPath StopBulkDeployment where
  toPath StopBulkDeployment' {..} =
    mconcat
      [ "/greengrass/bulk/deployments/",
        toBS _sbdBulkDeploymentId,
        "/$stop"
      ]

instance ToQuery StopBulkDeployment where
  toQuery = const mempty

-- | /See:/ 'stopBulkDeploymentResponse' smart constructor.
newtype StopBulkDeploymentResponse = StopBulkDeploymentResponse'
  { _sbdrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopBulkDeploymentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sbdrsResponseStatus' - -- | The response status code.
stopBulkDeploymentResponse ::
  -- | 'sbdrsResponseStatus'
  Int ->
  StopBulkDeploymentResponse
stopBulkDeploymentResponse pResponseStatus_ =
  StopBulkDeploymentResponse'
    { _sbdrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
sbdrsResponseStatus :: Lens' StopBulkDeploymentResponse Int
sbdrsResponseStatus = lens _sbdrsResponseStatus (\s a -> s {_sbdrsResponseStatus = a})

instance NFData StopBulkDeploymentResponse
