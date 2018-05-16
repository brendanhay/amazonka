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
-- Module      : Network.AWS.EC2.CancelExportTask
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an active export task. The request removes all artifacts of the export, including any partially-created Amazon S3 objects. If the export task is complete or is in the process of transferring the final disk image, the command fails and returns an error.
--
--
module Network.AWS.EC2.CancelExportTask
    (
    -- * Creating a Request
      cancelExportTask
    , CancelExportTask
    -- * Request Lenses
    , cetExportTaskId

    -- * Destructuring the Response
    , cancelExportTaskResponse
    , CancelExportTaskResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for CancelExportTask.
--
--
--
-- /See:/ 'cancelExportTask' smart constructor.
newtype CancelExportTask = CancelExportTask'
  { _cetExportTaskId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelExportTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cetExportTaskId' - The ID of the export task. This is the ID returned by @CreateInstanceExportTask@ .
cancelExportTask
    :: Text -- ^ 'cetExportTaskId'
    -> CancelExportTask
cancelExportTask pExportTaskId_ =
  CancelExportTask' {_cetExportTaskId = pExportTaskId_}


-- | The ID of the export task. This is the ID returned by @CreateInstanceExportTask@ .
cetExportTaskId :: Lens' CancelExportTask Text
cetExportTaskId = lens _cetExportTaskId (\ s a -> s{_cetExportTaskId = a})

instance AWSRequest CancelExportTask where
        type Rs CancelExportTask = CancelExportTaskResponse
        request = postQuery ec2
        response = receiveNull CancelExportTaskResponse'

instance Hashable CancelExportTask where

instance NFData CancelExportTask where

instance ToHeaders CancelExportTask where
        toHeaders = const mempty

instance ToPath CancelExportTask where
        toPath = const "/"

instance ToQuery CancelExportTask where
        toQuery CancelExportTask'{..}
          = mconcat
              ["Action" =: ("CancelExportTask" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "ExportTaskId" =: _cetExportTaskId]

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
