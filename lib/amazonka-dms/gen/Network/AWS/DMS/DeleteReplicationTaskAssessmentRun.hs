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
-- Module      : Network.AWS.DMS.DeleteReplicationTaskAssessmentRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the record of a single premigration assessment run.
--
--
-- This operation removes all metadata that AWS DMS maintains about this assessment run. However, the operation leaves untouched all information about this assessment run that is stored in your Amazon S3 bucket.
module Network.AWS.DMS.DeleteReplicationTaskAssessmentRun
  ( -- * Creating a Request
    deleteReplicationTaskAssessmentRun,
    DeleteReplicationTaskAssessmentRun,

    -- * Request Lenses
    drtarReplicationTaskAssessmentRunARN,

    -- * Destructuring the Response
    deleteReplicationTaskAssessmentRunResponse,
    DeleteReplicationTaskAssessmentRunResponse,

    -- * Response Lenses
    drtarrsReplicationTaskAssessmentRun,
    drtarrsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'deleteReplicationTaskAssessmentRun' smart constructor.
newtype DeleteReplicationTaskAssessmentRun = DeleteReplicationTaskAssessmentRun'
  { _drtarReplicationTaskAssessmentRunARN ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteReplicationTaskAssessmentRun' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drtarReplicationTaskAssessmentRunARN' - Amazon Resource Name (ARN) of the premigration assessment run to be deleted.
deleteReplicationTaskAssessmentRun ::
  -- | 'drtarReplicationTaskAssessmentRunARN'
  Text ->
  DeleteReplicationTaskAssessmentRun
deleteReplicationTaskAssessmentRun
  pReplicationTaskAssessmentRunARN_ =
    DeleteReplicationTaskAssessmentRun'
      { _drtarReplicationTaskAssessmentRunARN =
          pReplicationTaskAssessmentRunARN_
      }

-- | Amazon Resource Name (ARN) of the premigration assessment run to be deleted.
drtarReplicationTaskAssessmentRunARN :: Lens' DeleteReplicationTaskAssessmentRun Text
drtarReplicationTaskAssessmentRunARN = lens _drtarReplicationTaskAssessmentRunARN (\s a -> s {_drtarReplicationTaskAssessmentRunARN = a})

instance AWSRequest DeleteReplicationTaskAssessmentRun where
  type
    Rs DeleteReplicationTaskAssessmentRun =
      DeleteReplicationTaskAssessmentRunResponse
  request = postJSON dms
  response =
    receiveJSON
      ( \s h x ->
          DeleteReplicationTaskAssessmentRunResponse'
            <$> (x .?> "ReplicationTaskAssessmentRun") <*> (pure (fromEnum s))
      )

instance Hashable DeleteReplicationTaskAssessmentRun

instance NFData DeleteReplicationTaskAssessmentRun

instance ToHeaders DeleteReplicationTaskAssessmentRun where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AmazonDMSv20160101.DeleteReplicationTaskAssessmentRun" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteReplicationTaskAssessmentRun where
  toJSON DeleteReplicationTaskAssessmentRun' {..} =
    object
      ( catMaybes
          [ Just
              ( "ReplicationTaskAssessmentRunArn"
                  .= _drtarReplicationTaskAssessmentRunARN
              )
          ]
      )

instance ToPath DeleteReplicationTaskAssessmentRun where
  toPath = const "/"

instance ToQuery DeleteReplicationTaskAssessmentRun where
  toQuery = const mempty

-- |
--
--
--
-- /See:/ 'deleteReplicationTaskAssessmentRunResponse' smart constructor.
data DeleteReplicationTaskAssessmentRunResponse = DeleteReplicationTaskAssessmentRunResponse'
  { _drtarrsReplicationTaskAssessmentRun ::
      !( Maybe
           ReplicationTaskAssessmentRun
       ),
    _drtarrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DeleteReplicationTaskAssessmentRunResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drtarrsReplicationTaskAssessmentRun' - The @ReplicationTaskAssessmentRun@ object for the deleted assessment run.
--
-- * 'drtarrsResponseStatus' - -- | The response status code.
deleteReplicationTaskAssessmentRunResponse ::
  -- | 'drtarrsResponseStatus'
  Int ->
  DeleteReplicationTaskAssessmentRunResponse
deleteReplicationTaskAssessmentRunResponse pResponseStatus_ =
  DeleteReplicationTaskAssessmentRunResponse'
    { _drtarrsReplicationTaskAssessmentRun =
        Nothing,
      _drtarrsResponseStatus = pResponseStatus_
    }

-- | The @ReplicationTaskAssessmentRun@ object for the deleted assessment run.
drtarrsReplicationTaskAssessmentRun :: Lens' DeleteReplicationTaskAssessmentRunResponse (Maybe ReplicationTaskAssessmentRun)
drtarrsReplicationTaskAssessmentRun = lens _drtarrsReplicationTaskAssessmentRun (\s a -> s {_drtarrsReplicationTaskAssessmentRun = a})

-- | -- | The response status code.
drtarrsResponseStatus :: Lens' DeleteReplicationTaskAssessmentRunResponse Int
drtarrsResponseStatus = lens _drtarrsResponseStatus (\s a -> s {_drtarrsResponseStatus = a})

instance NFData DeleteReplicationTaskAssessmentRunResponse
