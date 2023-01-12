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
-- Module      : Amazonka.AuditManager.GetControl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a control from Audit Manager.
module Amazonka.AuditManager.GetControl
  ( -- * Creating a Request
    GetControl (..),
    newGetControl,

    -- * Request Lenses
    getControl_controlId,

    -- * Destructuring the Response
    GetControlResponse (..),
    newGetControlResponse,

    -- * Response Lenses
    getControlResponse_control,
    getControlResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetControl' smart constructor.
data GetControl = GetControl'
  { -- | The identifier for the control.
    controlId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'controlId', 'getControl_controlId' - The identifier for the control.
newGetControl ::
  -- | 'controlId'
  Prelude.Text ->
  GetControl
newGetControl pControlId_ =
  GetControl' {controlId = pControlId_}

-- | The identifier for the control.
getControl_controlId :: Lens.Lens' GetControl Prelude.Text
getControl_controlId = Lens.lens (\GetControl' {controlId} -> controlId) (\s@GetControl' {} a -> s {controlId = a} :: GetControl)

instance Core.AWSRequest GetControl where
  type AWSResponse GetControl = GetControlResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetControlResponse'
            Prelude.<$> (x Data..?> "control")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetControl where
  hashWithSalt _salt GetControl' {..} =
    _salt `Prelude.hashWithSalt` controlId

instance Prelude.NFData GetControl where
  rnf GetControl' {..} = Prelude.rnf controlId

instance Data.ToHeaders GetControl where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetControl where
  toPath GetControl' {..} =
    Prelude.mconcat ["/controls/", Data.toBS controlId]

instance Data.ToQuery GetControl where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetControlResponse' smart constructor.
data GetControlResponse = GetControlResponse'
  { -- | The name of the control that the @GetControl@ API returned.
    control :: Prelude.Maybe Control,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetControlResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'control', 'getControlResponse_control' - The name of the control that the @GetControl@ API returned.
--
-- 'httpStatus', 'getControlResponse_httpStatus' - The response's http status code.
newGetControlResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetControlResponse
newGetControlResponse pHttpStatus_ =
  GetControlResponse'
    { control = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the control that the @GetControl@ API returned.
getControlResponse_control :: Lens.Lens' GetControlResponse (Prelude.Maybe Control)
getControlResponse_control = Lens.lens (\GetControlResponse' {control} -> control) (\s@GetControlResponse' {} a -> s {control = a} :: GetControlResponse)

-- | The response's http status code.
getControlResponse_httpStatus :: Lens.Lens' GetControlResponse Prelude.Int
getControlResponse_httpStatus = Lens.lens (\GetControlResponse' {httpStatus} -> httpStatus) (\s@GetControlResponse' {} a -> s {httpStatus = a} :: GetControlResponse)

instance Prelude.NFData GetControlResponse where
  rnf GetControlResponse' {..} =
    Prelude.rnf control
      `Prelude.seq` Prelude.rnf httpStatus
