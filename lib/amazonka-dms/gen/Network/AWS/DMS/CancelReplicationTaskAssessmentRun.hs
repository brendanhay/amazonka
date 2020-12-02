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
-- Module      : Network.AWS.DMS.CancelReplicationTaskAssessmentRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a single premigration assessment run.
--
--
-- This operation prevents any individual assessments from running if they haven't started running. It also attempts to cancel any individual assessments that are currently running.
module Network.AWS.DMS.CancelReplicationTaskAssessmentRun
  ( -- * Creating a Request
    cancelReplicationTaskAssessmentRun,
    CancelReplicationTaskAssessmentRun,

    -- * Request Lenses
    crtarReplicationTaskAssessmentRunARN,

    -- * Destructuring the Response
    cancelReplicationTaskAssessmentRunResponse,
    CancelReplicationTaskAssessmentRunResponse,

    -- * Response Lenses
    crtarrsReplicationTaskAssessmentRun,
    crtarrsResponseStatus,
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
-- /See:/ 'cancelReplicationTaskAssessmentRun' smart constructor.
newtype CancelReplicationTaskAssessmentRun = CancelReplicationTaskAssessmentRun'
  { _crtarReplicationTaskAssessmentRunARN ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CancelReplicationTaskAssessmentRun' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crtarReplicationTaskAssessmentRunARN' - Amazon Resource Name (ARN) of the premigration assessment run to be canceled.
cancelReplicationTaskAssessmentRun ::
  -- | 'crtarReplicationTaskAssessmentRunARN'
  Text ->
  CancelReplicationTaskAssessmentRun
cancelReplicationTaskAssessmentRun
  pReplicationTaskAssessmentRunARN_ =
    CancelReplicationTaskAssessmentRun'
      { _crtarReplicationTaskAssessmentRunARN =
          pReplicationTaskAssessmentRunARN_
      }

-- | Amazon Resource Name (ARN) of the premigration assessment run to be canceled.
crtarReplicationTaskAssessmentRunARN :: Lens' CancelReplicationTaskAssessmentRun Text
crtarReplicationTaskAssessmentRunARN = lens _crtarReplicationTaskAssessmentRunARN (\s a -> s {_crtarReplicationTaskAssessmentRunARN = a})

instance AWSRequest CancelReplicationTaskAssessmentRun where
  type
    Rs CancelReplicationTaskAssessmentRun =
      CancelReplicationTaskAssessmentRunResponse
  request = postJSON dms
  response =
    receiveJSON
      ( \s h x ->
          CancelReplicationTaskAssessmentRunResponse'
            <$> (x .?> "ReplicationTaskAssessmentRun") <*> (pure (fromEnum s))
      )

instance Hashable CancelReplicationTaskAssessmentRun

instance NFData CancelReplicationTaskAssessmentRun

instance ToHeaders CancelReplicationTaskAssessmentRun where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AmazonDMSv20160101.CancelReplicationTaskAssessmentRun" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CancelReplicationTaskAssessmentRun where
  toJSON CancelReplicationTaskAssessmentRun' {..} =
    object
      ( catMaybes
          [ Just
              ( "ReplicationTaskAssessmentRunArn"
                  .= _crtarReplicationTaskAssessmentRunARN
              )
          ]
      )

instance ToPath CancelReplicationTaskAssessmentRun where
  toPath = const "/"

instance ToQuery CancelReplicationTaskAssessmentRun where
  toQuery = const mempty

-- |
--
--
--
-- /See:/ 'cancelReplicationTaskAssessmentRunResponse' smart constructor.
data CancelReplicationTaskAssessmentRunResponse = CancelReplicationTaskAssessmentRunResponse'
  { _crtarrsReplicationTaskAssessmentRun ::
      !( Maybe
           ReplicationTaskAssessmentRun
       ),
    _crtarrsResponseStatus ::
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

-- | Creates a value of 'CancelReplicationTaskAssessmentRunResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crtarrsReplicationTaskAssessmentRun' - The @ReplicationTaskAssessmentRun@ object for the canceled assessment run.
--
-- * 'crtarrsResponseStatus' - -- | The response status code.
cancelReplicationTaskAssessmentRunResponse ::
  -- | 'crtarrsResponseStatus'
  Int ->
  CancelReplicationTaskAssessmentRunResponse
cancelReplicationTaskAssessmentRunResponse pResponseStatus_ =
  CancelReplicationTaskAssessmentRunResponse'
    { _crtarrsReplicationTaskAssessmentRun =
        Nothing,
      _crtarrsResponseStatus = pResponseStatus_
    }

-- | The @ReplicationTaskAssessmentRun@ object for the canceled assessment run.
crtarrsReplicationTaskAssessmentRun :: Lens' CancelReplicationTaskAssessmentRunResponse (Maybe ReplicationTaskAssessmentRun)
crtarrsReplicationTaskAssessmentRun = lens _crtarrsReplicationTaskAssessmentRun (\s a -> s {_crtarrsReplicationTaskAssessmentRun = a})

-- | -- | The response status code.
crtarrsResponseStatus :: Lens' CancelReplicationTaskAssessmentRunResponse Int
crtarrsResponseStatus = lens _crtarrsResponseStatus (\s a -> s {_crtarrsResponseStatus = a})

instance NFData CancelReplicationTaskAssessmentRunResponse
