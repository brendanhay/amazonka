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
-- Module      : Network.AWS.CloudWatchLogs.CancelExportTask
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the specified export task.
--
--
-- The task must be in the @PENDING@ or @RUNNING@ state.
--
module Network.AWS.CloudWatchLogs.CancelExportTask
    (
    -- * Creating a Request
      cancelExportTask
    , CancelExportTask
    -- * Request Lenses
    , cetTaskId

    -- * Destructuring the Response
    , cancelExportTaskResponse
    , CancelExportTaskResponse
    ) where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.CloudWatchLogs.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'cancelExportTask' smart constructor.
newtype CancelExportTask = CancelExportTask'
  { _cetTaskId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelExportTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cetTaskId' - The ID of the export task.
cancelExportTask
    :: Text -- ^ 'cetTaskId'
    -> CancelExportTask
cancelExportTask pTaskId_ = CancelExportTask' {_cetTaskId = pTaskId_}


-- | The ID of the export task.
cetTaskId :: Lens' CancelExportTask Text
cetTaskId = lens _cetTaskId (\ s a -> s{_cetTaskId = a})

instance AWSRequest CancelExportTask where
        type Rs CancelExportTask = CancelExportTaskResponse
        request = postJSON cloudWatchLogs
        response = receiveNull CancelExportTaskResponse'

instance Hashable CancelExportTask where

instance NFData CancelExportTask where

instance ToHeaders CancelExportTask where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.CancelExportTask" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CancelExportTask where
        toJSON CancelExportTask'{..}
          = object (catMaybes [Just ("taskId" .= _cetTaskId)])

instance ToPath CancelExportTask where
        toPath = const "/"

instance ToQuery CancelExportTask where
        toQuery = const mempty

-- | /See:/ 'cancelExportTaskResponse' smart constructor.
data CancelExportTaskResponse =
  CancelExportTaskResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelExportTaskResponse' with the minimum fields required to make a request.
--
cancelExportTaskResponse
    :: CancelExportTaskResponse
cancelExportTaskResponse = CancelExportTaskResponse'


instance NFData CancelExportTaskResponse where
