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
-- Module      : Amazonka.IoT.DeleteScheduledAudit
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a scheduled audit.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DeleteScheduledAudit>
-- action.
module Amazonka.IoT.DeleteScheduledAudit
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteScheduledAudit' smart constructor.
data DeleteScheduledAudit = DeleteScheduledAudit'
  { -- | The name of the scheduled audit you want to delete.
    scheduledAuditName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeleteScheduledAudit where
  type
    AWSResponse DeleteScheduledAudit =
      DeleteScheduledAuditResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteScheduledAuditResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteScheduledAudit where
  hashWithSalt _salt DeleteScheduledAudit' {..} =
    _salt `Prelude.hashWithSalt` scheduledAuditName

instance Prelude.NFData DeleteScheduledAudit where
  rnf DeleteScheduledAudit' {..} =
    Prelude.rnf scheduledAuditName

instance Core.ToHeaders DeleteScheduledAudit where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteScheduledAudit where
  toPath DeleteScheduledAudit' {..} =
    Prelude.mconcat
      [ "/audit/scheduledaudits/",
        Core.toBS scheduledAuditName
      ]

instance Core.ToQuery DeleteScheduledAudit where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteScheduledAuditResponse' smart constructor.
data DeleteScheduledAuditResponse = DeleteScheduledAuditResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData DeleteScheduledAuditResponse where
  rnf DeleteScheduledAuditResponse' {..} =
    Prelude.rnf httpStatus
