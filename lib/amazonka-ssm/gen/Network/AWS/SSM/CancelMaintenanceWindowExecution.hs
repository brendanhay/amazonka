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
-- Module      : Network.AWS.SSM.CancelMaintenanceWindowExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a maintenance window execution that is already in progress and cancels any tasks in the window that have not already starting running. (Tasks already in progress will continue to completion.)
module Network.AWS.SSM.CancelMaintenanceWindowExecution
  ( -- * Creating a Request
    cancelMaintenanceWindowExecution,
    CancelMaintenanceWindowExecution,

    -- * Request Lenses
    cmweWindowExecutionId,

    -- * Destructuring the Response
    cancelMaintenanceWindowExecutionResponse,
    CancelMaintenanceWindowExecutionResponse,

    -- * Response Lenses
    cmwersWindowExecutionId,
    cmwersResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types

-- | /See:/ 'cancelMaintenanceWindowExecution' smart constructor.
newtype CancelMaintenanceWindowExecution = CancelMaintenanceWindowExecution'
  { _cmweWindowExecutionId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CancelMaintenanceWindowExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmweWindowExecutionId' - The ID of the maintenance window execution to stop.
cancelMaintenanceWindowExecution ::
  -- | 'cmweWindowExecutionId'
  Text ->
  CancelMaintenanceWindowExecution
cancelMaintenanceWindowExecution pWindowExecutionId_ =
  CancelMaintenanceWindowExecution'
    { _cmweWindowExecutionId =
        pWindowExecutionId_
    }

-- | The ID of the maintenance window execution to stop.
cmweWindowExecutionId :: Lens' CancelMaintenanceWindowExecution Text
cmweWindowExecutionId = lens _cmweWindowExecutionId (\s a -> s {_cmweWindowExecutionId = a})

instance AWSRequest CancelMaintenanceWindowExecution where
  type
    Rs CancelMaintenanceWindowExecution =
      CancelMaintenanceWindowExecutionResponse
  request = postJSON ssm
  response =
    receiveJSON
      ( \s h x ->
          CancelMaintenanceWindowExecutionResponse'
            <$> (x .?> "WindowExecutionId") <*> (pure (fromEnum s))
      )

instance Hashable CancelMaintenanceWindowExecution

instance NFData CancelMaintenanceWindowExecution

instance ToHeaders CancelMaintenanceWindowExecution where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AmazonSSM.CancelMaintenanceWindowExecution" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CancelMaintenanceWindowExecution where
  toJSON CancelMaintenanceWindowExecution' {..} =
    object
      (catMaybes [Just ("WindowExecutionId" .= _cmweWindowExecutionId)])

instance ToPath CancelMaintenanceWindowExecution where
  toPath = const "/"

instance ToQuery CancelMaintenanceWindowExecution where
  toQuery = const mempty

-- | /See:/ 'cancelMaintenanceWindowExecutionResponse' smart constructor.
data CancelMaintenanceWindowExecutionResponse = CancelMaintenanceWindowExecutionResponse'
  { _cmwersWindowExecutionId ::
      !( Maybe
           Text
       ),
    _cmwersResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CancelMaintenanceWindowExecutionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmwersWindowExecutionId' - The ID of the maintenance window execution that has been stopped.
--
-- * 'cmwersResponseStatus' - -- | The response status code.
cancelMaintenanceWindowExecutionResponse ::
  -- | 'cmwersResponseStatus'
  Int ->
  CancelMaintenanceWindowExecutionResponse
cancelMaintenanceWindowExecutionResponse pResponseStatus_ =
  CancelMaintenanceWindowExecutionResponse'
    { _cmwersWindowExecutionId =
        Nothing,
      _cmwersResponseStatus = pResponseStatus_
    }

-- | The ID of the maintenance window execution that has been stopped.
cmwersWindowExecutionId :: Lens' CancelMaintenanceWindowExecutionResponse (Maybe Text)
cmwersWindowExecutionId = lens _cmwersWindowExecutionId (\s a -> s {_cmwersWindowExecutionId = a})

-- | -- | The response status code.
cmwersResponseStatus :: Lens' CancelMaintenanceWindowExecutionResponse Int
cmwersResponseStatus = lens _cmwersResponseStatus (\s a -> s {_cmwersResponseStatus = a})

instance NFData CancelMaintenanceWindowExecutionResponse
