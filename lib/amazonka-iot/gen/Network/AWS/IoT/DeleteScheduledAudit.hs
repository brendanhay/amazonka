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
-- Module      : Network.AWS.IoT.DeleteScheduledAudit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a scheduled audit.
module Network.AWS.IoT.DeleteScheduledAudit
  ( -- * Creating a Request
    deleteScheduledAudit,
    DeleteScheduledAudit,

    -- * Request Lenses
    dsaScheduledAuditName,

    -- * Destructuring the Response
    deleteScheduledAuditResponse,
    DeleteScheduledAuditResponse,

    -- * Response Lenses
    dsasrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteScheduledAudit' smart constructor.
newtype DeleteScheduledAudit = DeleteScheduledAudit'
  { _dsaScheduledAuditName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteScheduledAudit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsaScheduledAuditName' - The name of the scheduled audit you want to delete.
deleteScheduledAudit ::
  -- | 'dsaScheduledAuditName'
  Text ->
  DeleteScheduledAudit
deleteScheduledAudit pScheduledAuditName_ =
  DeleteScheduledAudit'
    { _dsaScheduledAuditName =
        pScheduledAuditName_
    }

-- | The name of the scheduled audit you want to delete.
dsaScheduledAuditName :: Lens' DeleteScheduledAudit Text
dsaScheduledAuditName = lens _dsaScheduledAuditName (\s a -> s {_dsaScheduledAuditName = a})

instance AWSRequest DeleteScheduledAudit where
  type Rs DeleteScheduledAudit = DeleteScheduledAuditResponse
  request = delete ioT
  response =
    receiveEmpty
      (\s h x -> DeleteScheduledAuditResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteScheduledAudit

instance NFData DeleteScheduledAudit

instance ToHeaders DeleteScheduledAudit where
  toHeaders = const mempty

instance ToPath DeleteScheduledAudit where
  toPath DeleteScheduledAudit' {..} =
    mconcat ["/audit/scheduledaudits/", toBS _dsaScheduledAuditName]

instance ToQuery DeleteScheduledAudit where
  toQuery = const mempty

-- | /See:/ 'deleteScheduledAuditResponse' smart constructor.
newtype DeleteScheduledAuditResponse = DeleteScheduledAuditResponse'
  { _dsasrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteScheduledAuditResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsasrsResponseStatus' - -- | The response status code.
deleteScheduledAuditResponse ::
  -- | 'dsasrsResponseStatus'
  Int ->
  DeleteScheduledAuditResponse
deleteScheduledAuditResponse pResponseStatus_ =
  DeleteScheduledAuditResponse'
    { _dsasrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
dsasrsResponseStatus :: Lens' DeleteScheduledAuditResponse Int
dsasrsResponseStatus = lens _dsasrsResponseStatus (\s a -> s {_dsasrsResponseStatus = a})

instance NFData DeleteScheduledAuditResponse
