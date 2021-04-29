{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteScheduledAudit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a scheduled audit.
module Network.AWS.IoT.DeleteScheduledAudit
  ( -- * Creating a Request
    DeleteScheduledAudit (..),
    newDeleteScheduledAudit,

    -- * Request Lenses
    deleteScheduledAudit_scheduledAuditName,

    -- * Destructuring the Response
    DeleteScheduledAuditResponse (..),
    newDeleteScheduledAuditResponse,

    -- * Response Lenses
    deleteScheduledAuditResponse_httpStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteScheduledAudit' smart constructor.
data DeleteScheduledAudit = DeleteScheduledAudit'
  { -- | The name of the scheduled audit you want to delete.
    scheduledAuditName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteScheduledAudit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scheduledAuditName', 'deleteScheduledAudit_scheduledAuditName' - The name of the scheduled audit you want to delete.
newDeleteScheduledAudit ::
  -- | 'scheduledAuditName'
  Prelude.Text ->
  DeleteScheduledAudit
newDeleteScheduledAudit pScheduledAuditName_ =
  DeleteScheduledAudit'
    { scheduledAuditName =
        pScheduledAuditName_
    }

-- | The name of the scheduled audit you want to delete.
deleteScheduledAudit_scheduledAuditName :: Lens.Lens' DeleteScheduledAudit Prelude.Text
deleteScheduledAudit_scheduledAuditName = Lens.lens (\DeleteScheduledAudit' {scheduledAuditName} -> scheduledAuditName) (\s@DeleteScheduledAudit' {} a -> s {scheduledAuditName = a} :: DeleteScheduledAudit)

instance Prelude.AWSRequest DeleteScheduledAudit where
  type
    Rs DeleteScheduledAudit =
      DeleteScheduledAuditResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteScheduledAuditResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteScheduledAudit

instance Prelude.NFData DeleteScheduledAudit

instance Prelude.ToHeaders DeleteScheduledAudit where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteScheduledAudit where
  toPath DeleteScheduledAudit' {..} =
    Prelude.mconcat
      [ "/audit/scheduledaudits/",
        Prelude.toBS scheduledAuditName
      ]

instance Prelude.ToQuery DeleteScheduledAudit where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteScheduledAuditResponse' smart constructor.
data DeleteScheduledAuditResponse = DeleteScheduledAuditResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteScheduledAuditResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteScheduledAuditResponse_httpStatus' - The response's http status code.
newDeleteScheduledAuditResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteScheduledAuditResponse
newDeleteScheduledAuditResponse pHttpStatus_ =
  DeleteScheduledAuditResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteScheduledAuditResponse_httpStatus :: Lens.Lens' DeleteScheduledAuditResponse Prelude.Int
deleteScheduledAuditResponse_httpStatus = Lens.lens (\DeleteScheduledAuditResponse' {httpStatus} -> httpStatus) (\s@DeleteScheduledAuditResponse' {} a -> s {httpStatus = a} :: DeleteScheduledAuditResponse)

instance Prelude.NFData DeleteScheduledAuditResponse
