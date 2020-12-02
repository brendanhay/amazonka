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
-- Module      : Network.AWS.IoT.CancelAuditTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an audit that is in progress. The audit can be either scheduled or on-demand. If the audit is not in progress, an "InvalidRequestException" occurs.
module Network.AWS.IoT.CancelAuditTask
  ( -- * Creating a Request
    cancelAuditTask,
    CancelAuditTask,

    -- * Request Lenses
    catTaskId,

    -- * Destructuring the Response
    cancelAuditTaskResponse,
    CancelAuditTaskResponse,

    -- * Response Lenses
    catrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'cancelAuditTask' smart constructor.
newtype CancelAuditTask = CancelAuditTask' {_catTaskId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CancelAuditTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'catTaskId' - The ID of the audit you want to cancel. You can only cancel an audit that is "IN_PROGRESS".
cancelAuditTask ::
  -- | 'catTaskId'
  Text ->
  CancelAuditTask
cancelAuditTask pTaskId_ = CancelAuditTask' {_catTaskId = pTaskId_}

-- | The ID of the audit you want to cancel. You can only cancel an audit that is "IN_PROGRESS".
catTaskId :: Lens' CancelAuditTask Text
catTaskId = lens _catTaskId (\s a -> s {_catTaskId = a})

instance AWSRequest CancelAuditTask where
  type Rs CancelAuditTask = CancelAuditTaskResponse
  request = putJSON ioT
  response =
    receiveEmpty
      (\s h x -> CancelAuditTaskResponse' <$> (pure (fromEnum s)))

instance Hashable CancelAuditTask

instance NFData CancelAuditTask

instance ToHeaders CancelAuditTask where
  toHeaders = const mempty

instance ToJSON CancelAuditTask where
  toJSON = const (Object mempty)

instance ToPath CancelAuditTask where
  toPath CancelAuditTask' {..} =
    mconcat ["/audit/tasks/", toBS _catTaskId, "/cancel"]

instance ToQuery CancelAuditTask where
  toQuery = const mempty

-- | /See:/ 'cancelAuditTaskResponse' smart constructor.
newtype CancelAuditTaskResponse = CancelAuditTaskResponse'
  { _catrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CancelAuditTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'catrsResponseStatus' - -- | The response status code.
cancelAuditTaskResponse ::
  -- | 'catrsResponseStatus'
  Int ->
  CancelAuditTaskResponse
cancelAuditTaskResponse pResponseStatus_ =
  CancelAuditTaskResponse' {_catrsResponseStatus = pResponseStatus_}

-- | -- | The response status code.
catrsResponseStatus :: Lens' CancelAuditTaskResponse Int
catrsResponseStatus = lens _catrsResponseStatus (\s a -> s {_catrsResponseStatus = a})

instance NFData CancelAuditTaskResponse
