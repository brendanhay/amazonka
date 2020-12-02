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
-- Module      : Network.AWS.IoT.DeleteJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a job and its related job executions.
--
--
-- Deleting a job may take time, depending on the number of job executions created for the job and various other factors. While the job is being deleted, the status of the job will be shown as "DELETION_IN_PROGRESS". Attempting to delete or cancel a job whose status is already "DELETION_IN_PROGRESS" will result in an error.
--
-- Only 10 jobs may have status "DELETION_IN_PROGRESS" at the same time, or a LimitExceededException will occur.
module Network.AWS.IoT.DeleteJob
  ( -- * Creating a Request
    deleteJob,
    DeleteJob,

    -- * Request Lenses
    djForce,
    djNamespaceId,
    djJobId,

    -- * Destructuring the Response
    deleteJobResponse,
    DeleteJobResponse,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteJob' smart constructor.
data DeleteJob = DeleteJob'
  { _djForce :: !(Maybe Bool),
    _djNamespaceId :: !(Maybe Text),
    _djJobId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'djForce' - (Optional) When true, you can delete a job which is "IN_PROGRESS". Otherwise, you can only delete a job which is in a terminal state ("COMPLETED" or "CANCELED") or an exception will occur. The default is false.
--
-- * 'djNamespaceId' - The namespace used to indicate that a job is a customer-managed job. When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format. @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@
--
-- * 'djJobId' - The ID of the job to be deleted. After a job deletion is completed, you may reuse this jobId when you create a new job. However, this is not recommended, and you must ensure that your devices are not using the jobId to refer to the deleted job.
deleteJob ::
  -- | 'djJobId'
  Text ->
  DeleteJob
deleteJob pJobId_ =
  DeleteJob'
    { _djForce = Nothing,
      _djNamespaceId = Nothing,
      _djJobId = pJobId_
    }

-- | (Optional) When true, you can delete a job which is "IN_PROGRESS". Otherwise, you can only delete a job which is in a terminal state ("COMPLETED" or "CANCELED") or an exception will occur. The default is false.
djForce :: Lens' DeleteJob (Maybe Bool)
djForce = lens _djForce (\s a -> s {_djForce = a})

-- | The namespace used to indicate that a job is a customer-managed job. When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format. @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@
djNamespaceId :: Lens' DeleteJob (Maybe Text)
djNamespaceId = lens _djNamespaceId (\s a -> s {_djNamespaceId = a})

-- | The ID of the job to be deleted. After a job deletion is completed, you may reuse this jobId when you create a new job. However, this is not recommended, and you must ensure that your devices are not using the jobId to refer to the deleted job.
djJobId :: Lens' DeleteJob Text
djJobId = lens _djJobId (\s a -> s {_djJobId = a})

instance AWSRequest DeleteJob where
  type Rs DeleteJob = DeleteJobResponse
  request = delete ioT
  response = receiveNull DeleteJobResponse'

instance Hashable DeleteJob

instance NFData DeleteJob

instance ToHeaders DeleteJob where
  toHeaders = const mempty

instance ToPath DeleteJob where
  toPath DeleteJob' {..} = mconcat ["/jobs/", toBS _djJobId]

instance ToQuery DeleteJob where
  toQuery DeleteJob' {..} =
    mconcat ["force" =: _djForce, "namespaceId" =: _djNamespaceId]

-- | /See:/ 'deleteJobResponse' smart constructor.
data DeleteJobResponse = DeleteJobResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteJobResponse' with the minimum fields required to make a request.
deleteJobResponse ::
  DeleteJobResponse
deleteJobResponse = DeleteJobResponse'

instance NFData DeleteJobResponse
