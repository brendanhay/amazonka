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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an active export task. The request removes all artifacts of the
-- export, including any partially-created Amazon S3 objects. If the export
-- task is complete or is in the process of transferring the final disk
-- image, the command fails and returns an error.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CancelExportTask.html AWS API Reference> for CancelExportTask.
module Network.AWS.EC2.CancelExportTask
    (
    -- * Creating a Request
      CancelExportTask
    , cancelExportTask
    -- * Request Lenses
    , cetExportTaskId

    -- * Destructuring the Response
    , CancelExportTaskResponse
    , cancelExportTaskResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'cancelExportTask' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cetExportTaskId'
newtype CancelExportTask = CancelExportTask'
    { _cetExportTaskId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CancelExportTask' smart constructor.
cancelExportTask :: Text -> CancelExportTask
cancelExportTask pExportTaskId_ =
    CancelExportTask'
    { _cetExportTaskId = pExportTaskId_
    }

-- | The ID of the export task. This is the ID returned by
-- @CreateInstanceExportTask@.
cetExportTaskId :: Lens' CancelExportTask Text
cetExportTaskId = lens _cetExportTaskId (\ s a -> s{_cetExportTaskId = a});

instance AWSRequest CancelExportTask where
        type Sv CancelExportTask = EC2
        type Rs CancelExportTask = CancelExportTaskResponse
        request = post
        response = receiveNull CancelExportTaskResponse'

instance ToHeaders CancelExportTask where
        toHeaders = const mempty

instance ToPath CancelExportTask where
        toPath = const "/"

instance ToQuery CancelExportTask where
        toQuery CancelExportTask'{..}
          = mconcat
              ["Action" =: ("CancelExportTask" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "ExportTaskId" =: _cetExportTaskId]

-- | /See:/ 'cancelExportTaskResponse' smart constructor.
data CancelExportTaskResponse =
    CancelExportTaskResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CancelExportTaskResponse' smart constructor.
cancelExportTaskResponse :: CancelExportTaskResponse
cancelExportTaskResponse = CancelExportTaskResponse'
