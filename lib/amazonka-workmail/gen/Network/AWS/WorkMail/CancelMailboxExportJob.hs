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
-- Module      : Network.AWS.WorkMail.CancelMailboxExportJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a mailbox export job.
module Network.AWS.WorkMail.CancelMailboxExportJob
  ( -- * Creating a Request
    cancelMailboxExportJob,
    CancelMailboxExportJob,

    -- * Request Lenses
    cmejClientToken,
    cmejJobId,
    cmejOrganizationId,

    -- * Destructuring the Response
    cancelMailboxExportJobResponse,
    CancelMailboxExportJobResponse,

    -- * Response Lenses
    cmejrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'cancelMailboxExportJob' smart constructor.
data CancelMailboxExportJob = CancelMailboxExportJob'
  { _cmejClientToken ::
      !Text,
    _cmejJobId :: !Text,
    _cmejOrganizationId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CancelMailboxExportJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmejClientToken' - The idempotency token for the client request.
--
-- * 'cmejJobId' - The job ID.
--
-- * 'cmejOrganizationId' - The organization ID.
cancelMailboxExportJob ::
  -- | 'cmejClientToken'
  Text ->
  -- | 'cmejJobId'
  Text ->
  -- | 'cmejOrganizationId'
  Text ->
  CancelMailboxExportJob
cancelMailboxExportJob pClientToken_ pJobId_ pOrganizationId_ =
  CancelMailboxExportJob'
    { _cmejClientToken = pClientToken_,
      _cmejJobId = pJobId_,
      _cmejOrganizationId = pOrganizationId_
    }

-- | The idempotency token for the client request.
cmejClientToken :: Lens' CancelMailboxExportJob Text
cmejClientToken = lens _cmejClientToken (\s a -> s {_cmejClientToken = a})

-- | The job ID.
cmejJobId :: Lens' CancelMailboxExportJob Text
cmejJobId = lens _cmejJobId (\s a -> s {_cmejJobId = a})

-- | The organization ID.
cmejOrganizationId :: Lens' CancelMailboxExportJob Text
cmejOrganizationId = lens _cmejOrganizationId (\s a -> s {_cmejOrganizationId = a})

instance AWSRequest CancelMailboxExportJob where
  type Rs CancelMailboxExportJob = CancelMailboxExportJobResponse
  request = postJSON workMail
  response =
    receiveEmpty
      ( \s h x ->
          CancelMailboxExportJobResponse' <$> (pure (fromEnum s))
      )

instance Hashable CancelMailboxExportJob

instance NFData CancelMailboxExportJob

instance ToHeaders CancelMailboxExportJob where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("WorkMailService.CancelMailboxExportJob" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CancelMailboxExportJob where
  toJSON CancelMailboxExportJob' {..} =
    object
      ( catMaybes
          [ Just ("ClientToken" .= _cmejClientToken),
            Just ("JobId" .= _cmejJobId),
            Just ("OrganizationId" .= _cmejOrganizationId)
          ]
      )

instance ToPath CancelMailboxExportJob where
  toPath = const "/"

instance ToQuery CancelMailboxExportJob where
  toQuery = const mempty

-- | /See:/ 'cancelMailboxExportJobResponse' smart constructor.
newtype CancelMailboxExportJobResponse = CancelMailboxExportJobResponse'
  { _cmejrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CancelMailboxExportJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmejrsResponseStatus' - -- | The response status code.
cancelMailboxExportJobResponse ::
  -- | 'cmejrsResponseStatus'
  Int ->
  CancelMailboxExportJobResponse
cancelMailboxExportJobResponse pResponseStatus_ =
  CancelMailboxExportJobResponse'
    { _cmejrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
cmejrsResponseStatus :: Lens' CancelMailboxExportJobResponse Int
cmejrsResponseStatus = lens _cmejrsResponseStatus (\s a -> s {_cmejrsResponseStatus = a})

instance NFData CancelMailboxExportJobResponse
