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
-- Module      : Amazonka.Forecast.DeleteMonitor
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a monitor resource. You can only delete a monitor resource with
-- a status of @ACTIVE@, @ACTIVE_STOPPED@, @CREATE_FAILED@, or
-- @CREATE_STOPPED@.
module Amazonka.Forecast.DeleteMonitor
  ( -- * Creating a Request
    DeleteMonitor (..),
    newDeleteMonitor,

    -- * Request Lenses
    deleteMonitor_monitorArn,

    -- * Destructuring the Response
    DeleteMonitorResponse (..),
    newDeleteMonitorResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteMonitor' smart constructor.
data DeleteMonitor = DeleteMonitor'
  { -- | The Amazon Resource Name (ARN) of the monitor resource to delete.
    monitorArn :: Prelude.Text
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
-- 'monitorArn', 'deleteMonitor_monitorArn' - The Amazon Resource Name (ARN) of the monitor resource to delete.
newDeleteMonitor ::
  -- | 'monitorArn'
  Prelude.Text ->
  DeleteMonitor
newDeleteMonitor pMonitorArn_ =
  DeleteMonitor' {monitorArn = pMonitorArn_}

-- | The Amazon Resource Name (ARN) of the monitor resource to delete.
deleteMonitor_monitorArn :: Lens.Lens' DeleteMonitor Prelude.Text
deleteMonitor_monitorArn = Lens.lens (\DeleteMonitor' {monitorArn} -> monitorArn) (\s@DeleteMonitor' {} a -> s {monitorArn = a} :: DeleteMonitor)

instance Core.AWSRequest DeleteMonitor where
  type
    AWSResponse DeleteMonitor =
      DeleteMonitorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteMonitorResponse'

instance Prelude.Hashable DeleteMonitor where
  hashWithSalt _salt DeleteMonitor' {..} =
    _salt `Prelude.hashWithSalt` monitorArn

instance Prelude.NFData DeleteMonitor where
  rnf DeleteMonitor' {..} = Prelude.rnf monitorArn

instance Core.ToHeaders DeleteMonitor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonForecast.DeleteMonitor" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteMonitor where
  toJSON DeleteMonitor' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("MonitorArn" Core..= monitorArn)]
      )

instance Core.ToPath DeleteMonitor where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteMonitor where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteMonitorResponse' smart constructor.
data DeleteMonitorResponse = DeleteMonitorResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMonitorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteMonitorResponse ::
  DeleteMonitorResponse
newDeleteMonitorResponse = DeleteMonitorResponse'

instance Prelude.NFData DeleteMonitorResponse where
  rnf _ = ()
