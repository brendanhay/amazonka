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
-- Module      : Network.AWS.IoT.CancelAuditMitigationActionsTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a mitigation action task that is in progress. If the task is not in progress, an InvalidRequestException occurs.
module Network.AWS.IoT.CancelAuditMitigationActionsTask
  ( -- * Creating a Request
    cancelAuditMitigationActionsTask,
    CancelAuditMitigationActionsTask,

    -- * Request Lenses
    camatTaskId,

    -- * Destructuring the Response
    cancelAuditMitigationActionsTaskResponse,
    CancelAuditMitigationActionsTaskResponse,

    -- * Response Lenses
    camatrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'cancelAuditMitigationActionsTask' smart constructor.
newtype CancelAuditMitigationActionsTask = CancelAuditMitigationActionsTask'
  { _camatTaskId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CancelAuditMitigationActionsTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'camatTaskId' - The unique identifier for the task that you want to cancel.
cancelAuditMitigationActionsTask ::
  -- | 'camatTaskId'
  Text ->
  CancelAuditMitigationActionsTask
cancelAuditMitigationActionsTask pTaskId_ =
  CancelAuditMitigationActionsTask' {_camatTaskId = pTaskId_}

-- | The unique identifier for the task that you want to cancel.
camatTaskId :: Lens' CancelAuditMitigationActionsTask Text
camatTaskId = lens _camatTaskId (\s a -> s {_camatTaskId = a})

instance AWSRequest CancelAuditMitigationActionsTask where
  type
    Rs CancelAuditMitigationActionsTask =
      CancelAuditMitigationActionsTaskResponse
  request = putJSON ioT
  response =
    receiveEmpty
      ( \s h x ->
          CancelAuditMitigationActionsTaskResponse' <$> (pure (fromEnum s))
      )

instance Hashable CancelAuditMitigationActionsTask

instance NFData CancelAuditMitigationActionsTask

instance ToHeaders CancelAuditMitigationActionsTask where
  toHeaders = const mempty

instance ToJSON CancelAuditMitigationActionsTask where
  toJSON = const (Object mempty)

instance ToPath CancelAuditMitigationActionsTask where
  toPath CancelAuditMitigationActionsTask' {..} =
    mconcat
      ["/audit/mitigationactions/tasks/", toBS _camatTaskId, "/cancel"]

instance ToQuery CancelAuditMitigationActionsTask where
  toQuery = const mempty

-- | /See:/ 'cancelAuditMitigationActionsTaskResponse' smart constructor.
newtype CancelAuditMitigationActionsTaskResponse = CancelAuditMitigationActionsTaskResponse'
  { _camatrsResponseStatus ::
      Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'CancelAuditMitigationActionsTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'camatrsResponseStatus' - -- | The response status code.
cancelAuditMitigationActionsTaskResponse ::
  -- | 'camatrsResponseStatus'
  Int ->
  CancelAuditMitigationActionsTaskResponse
cancelAuditMitigationActionsTaskResponse pResponseStatus_ =
  CancelAuditMitigationActionsTaskResponse'
    { _camatrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
camatrsResponseStatus :: Lens' CancelAuditMitigationActionsTaskResponse Int
camatrsResponseStatus = lens _camatrsResponseStatus (\s a -> s {_camatrsResponseStatus = a})

instance NFData CancelAuditMitigationActionsTaskResponse
