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
-- Module      : Amazonka.InternetMonitor.DeleteMonitor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a monitor in Amazon CloudWatch Internet Monitor.
module Amazonka.InternetMonitor.DeleteMonitor
  ( -- * Creating a Request
    DeleteMonitor (..),
    newDeleteMonitor,

    -- * Request Lenses
    deleteMonitor_monitorName,

    -- * Destructuring the Response
    DeleteMonitorResponse (..),
    newDeleteMonitorResponse,

    -- * Response Lenses
    deleteMonitorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.InternetMonitor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteMonitor' smart constructor.
data DeleteMonitor = DeleteMonitor'
  { -- | The name of the monitor to delete.
    monitorName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMonitor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'monitorName', 'deleteMonitor_monitorName' - The name of the monitor to delete.
newDeleteMonitor ::
  -- | 'monitorName'
  Prelude.Text ->
  DeleteMonitor
newDeleteMonitor pMonitorName_ =
  DeleteMonitor' {monitorName = pMonitorName_}

-- | The name of the monitor to delete.
deleteMonitor_monitorName :: Lens.Lens' DeleteMonitor Prelude.Text
deleteMonitor_monitorName = Lens.lens (\DeleteMonitor' {monitorName} -> monitorName) (\s@DeleteMonitor' {} a -> s {monitorName = a} :: DeleteMonitor)

instance Core.AWSRequest DeleteMonitor where
  type
    AWSResponse DeleteMonitor =
      DeleteMonitorResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteMonitorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteMonitor where
  hashWithSalt _salt DeleteMonitor' {..} =
    _salt `Prelude.hashWithSalt` monitorName

instance Prelude.NFData DeleteMonitor where
  rnf DeleteMonitor' {..} = Prelude.rnf monitorName

instance Data.ToHeaders DeleteMonitor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteMonitor where
  toPath DeleteMonitor' {..} =
    Prelude.mconcat
      ["/v20210603/Monitors/", Data.toBS monitorName]

instance Data.ToQuery DeleteMonitor where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteMonitorResponse' smart constructor.
data DeleteMonitorResponse = DeleteMonitorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMonitorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteMonitorResponse_httpStatus' - The response's http status code.
newDeleteMonitorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteMonitorResponse
newDeleteMonitorResponse pHttpStatus_ =
  DeleteMonitorResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteMonitorResponse_httpStatus :: Lens.Lens' DeleteMonitorResponse Prelude.Int
deleteMonitorResponse_httpStatus = Lens.lens (\DeleteMonitorResponse' {httpStatus} -> httpStatus) (\s@DeleteMonitorResponse' {} a -> s {httpStatus = a} :: DeleteMonitorResponse)

instance Prelude.NFData DeleteMonitorResponse where
  rnf DeleteMonitorResponse' {..} =
    Prelude.rnf httpStatus
