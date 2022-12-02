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
-- Module      : Amazonka.Rum.DeleteAppMonitor
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing app monitor. This immediately stops the collection
-- of data.
module Amazonka.Rum.DeleteAppMonitor
  ( -- * Creating a Request
    DeleteAppMonitor (..),
    newDeleteAppMonitor,

    -- * Request Lenses
    deleteAppMonitor_name,

    -- * Destructuring the Response
    DeleteAppMonitorResponse (..),
    newDeleteAppMonitorResponse,

    -- * Response Lenses
    deleteAppMonitorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Rum.Types

-- | /See:/ 'newDeleteAppMonitor' smart constructor.
data DeleteAppMonitor = DeleteAppMonitor'
  { -- | The name of the app monitor to delete.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAppMonitor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteAppMonitor_name' - The name of the app monitor to delete.
newDeleteAppMonitor ::
  -- | 'name'
  Prelude.Text ->
  DeleteAppMonitor
newDeleteAppMonitor pName_ =
  DeleteAppMonitor' {name = pName_}

-- | The name of the app monitor to delete.
deleteAppMonitor_name :: Lens.Lens' DeleteAppMonitor Prelude.Text
deleteAppMonitor_name = Lens.lens (\DeleteAppMonitor' {name} -> name) (\s@DeleteAppMonitor' {} a -> s {name = a} :: DeleteAppMonitor)

instance Core.AWSRequest DeleteAppMonitor where
  type
    AWSResponse DeleteAppMonitor =
      DeleteAppMonitorResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAppMonitorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAppMonitor where
  hashWithSalt _salt DeleteAppMonitor' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteAppMonitor where
  rnf DeleteAppMonitor' {..} = Prelude.rnf name

instance Data.ToHeaders DeleteAppMonitor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteAppMonitor where
  toPath DeleteAppMonitor' {..} =
    Prelude.mconcat ["/appmonitor/", Data.toBS name]

instance Data.ToQuery DeleteAppMonitor where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAppMonitorResponse' smart constructor.
data DeleteAppMonitorResponse = DeleteAppMonitorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAppMonitorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAppMonitorResponse_httpStatus' - The response's http status code.
newDeleteAppMonitorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAppMonitorResponse
newDeleteAppMonitorResponse pHttpStatus_ =
  DeleteAppMonitorResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteAppMonitorResponse_httpStatus :: Lens.Lens' DeleteAppMonitorResponse Prelude.Int
deleteAppMonitorResponse_httpStatus = Lens.lens (\DeleteAppMonitorResponse' {httpStatus} -> httpStatus) (\s@DeleteAppMonitorResponse' {} a -> s {httpStatus = a} :: DeleteAppMonitorResponse)

instance Prelude.NFData DeleteAppMonitorResponse where
  rnf DeleteAppMonitorResponse' {..} =
    Prelude.rnf httpStatus
