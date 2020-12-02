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
-- Module      : Network.AWS.SMS.DeleteReplicationJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified replication job.
--
--
-- After you delete a replication job, there are no further replication runs. AWS deletes the contents of the Amazon S3 bucket used to store AWS SMS artifacts. The AMIs created by the replication runs are not deleted.
module Network.AWS.SMS.DeleteReplicationJob
  ( -- * Creating a Request
    deleteReplicationJob,
    DeleteReplicationJob,

    -- * Request Lenses
    drjReplicationJobId,

    -- * Destructuring the Response
    deleteReplicationJobResponse,
    DeleteReplicationJobResponse,

    -- * Response Lenses
    drjrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SMS.Types

-- | /See:/ 'deleteReplicationJob' smart constructor.
newtype DeleteReplicationJob = DeleteReplicationJob'
  { _drjReplicationJobId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteReplicationJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drjReplicationJobId' - The ID of the replication job.
deleteReplicationJob ::
  -- | 'drjReplicationJobId'
  Text ->
  DeleteReplicationJob
deleteReplicationJob pReplicationJobId_ =
  DeleteReplicationJob' {_drjReplicationJobId = pReplicationJobId_}

-- | The ID of the replication job.
drjReplicationJobId :: Lens' DeleteReplicationJob Text
drjReplicationJobId = lens _drjReplicationJobId (\s a -> s {_drjReplicationJobId = a})

instance AWSRequest DeleteReplicationJob where
  type Rs DeleteReplicationJob = DeleteReplicationJobResponse
  request = postJSON sms
  response =
    receiveEmpty
      (\s h x -> DeleteReplicationJobResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteReplicationJob

instance NFData DeleteReplicationJob

instance ToHeaders DeleteReplicationJob where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSServerMigrationService_V2016_10_24.DeleteReplicationJob" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteReplicationJob where
  toJSON DeleteReplicationJob' {..} =
    object
      (catMaybes [Just ("replicationJobId" .= _drjReplicationJobId)])

instance ToPath DeleteReplicationJob where
  toPath = const "/"

instance ToQuery DeleteReplicationJob where
  toQuery = const mempty

-- | /See:/ 'deleteReplicationJobResponse' smart constructor.
newtype DeleteReplicationJobResponse = DeleteReplicationJobResponse'
  { _drjrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteReplicationJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drjrsResponseStatus' - -- | The response status code.
deleteReplicationJobResponse ::
  -- | 'drjrsResponseStatus'
  Int ->
  DeleteReplicationJobResponse
deleteReplicationJobResponse pResponseStatus_ =
  DeleteReplicationJobResponse'
    { _drjrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
drjrsResponseStatus :: Lens' DeleteReplicationJobResponse Int
drjrsResponseStatus = lens _drjrsResponseStatus (\s a -> s {_drjrsResponseStatus = a})

instance NFData DeleteReplicationJobResponse
