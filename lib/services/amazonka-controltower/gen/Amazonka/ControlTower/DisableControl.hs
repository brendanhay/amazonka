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
-- Module      : Amazonka.ControlTower.DisableControl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API call turns off a control. It starts an asynchronous operation
-- that deletes AWS resources on the specified organizational unit and the
-- accounts it contains. The resources will vary according to the control
-- that you specify.
module Amazonka.ControlTower.DisableControl
  ( -- * Creating a Request
    DisableControl (..),
    newDisableControl,

    -- * Request Lenses
    disableControl_controlIdentifier,
    disableControl_targetIdentifier,

    -- * Destructuring the Response
    DisableControlResponse (..),
    newDisableControlResponse,

    -- * Response Lenses
    disableControlResponse_httpStatus,
    disableControlResponse_operationIdentifier,
  )
where

import Amazonka.ControlTower.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisableControl' smart constructor.
data DisableControl = DisableControl'
  { -- | The ARN of the control. Only __Strongly recommended__ and __Elective__
    -- controls are permitted, with the exception of the __Region deny__
    -- guardrail.
    controlIdentifier :: Prelude.Text,
    -- | The ARN of the organizational unit.
    targetIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'controlIdentifier', 'disableControl_controlIdentifier' - The ARN of the control. Only __Strongly recommended__ and __Elective__
-- controls are permitted, with the exception of the __Region deny__
-- guardrail.
--
-- 'targetIdentifier', 'disableControl_targetIdentifier' - The ARN of the organizational unit.
newDisableControl ::
  -- | 'controlIdentifier'
  Prelude.Text ->
  -- | 'targetIdentifier'
  Prelude.Text ->
  DisableControl
newDisableControl
  pControlIdentifier_
  pTargetIdentifier_ =
    DisableControl'
      { controlIdentifier =
          pControlIdentifier_,
        targetIdentifier = pTargetIdentifier_
      }

-- | The ARN of the control. Only __Strongly recommended__ and __Elective__
-- controls are permitted, with the exception of the __Region deny__
-- guardrail.
disableControl_controlIdentifier :: Lens.Lens' DisableControl Prelude.Text
disableControl_controlIdentifier = Lens.lens (\DisableControl' {controlIdentifier} -> controlIdentifier) (\s@DisableControl' {} a -> s {controlIdentifier = a} :: DisableControl)

-- | The ARN of the organizational unit.
disableControl_targetIdentifier :: Lens.Lens' DisableControl Prelude.Text
disableControl_targetIdentifier = Lens.lens (\DisableControl' {targetIdentifier} -> targetIdentifier) (\s@DisableControl' {} a -> s {targetIdentifier = a} :: DisableControl)

instance Core.AWSRequest DisableControl where
  type
    AWSResponse DisableControl =
      DisableControlResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisableControlResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "operationIdentifier")
      )

instance Prelude.Hashable DisableControl where
  hashWithSalt _salt DisableControl' {..} =
    _salt `Prelude.hashWithSalt` controlIdentifier
      `Prelude.hashWithSalt` targetIdentifier

instance Prelude.NFData DisableControl where
  rnf DisableControl' {..} =
    Prelude.rnf controlIdentifier
      `Prelude.seq` Prelude.rnf targetIdentifier

instance Data.ToHeaders DisableControl where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisableControl where
  toJSON DisableControl' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("controlIdentifier" Data..= controlIdentifier),
            Prelude.Just
              ("targetIdentifier" Data..= targetIdentifier)
          ]
      )

instance Data.ToPath DisableControl where
  toPath = Prelude.const "/disable-control"

instance Data.ToQuery DisableControl where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisableControlResponse' smart constructor.
data DisableControlResponse = DisableControlResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the asynchronous operation, which is used to track status. The
    -- operation is available for 90 days.
    operationIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableControlResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disableControlResponse_httpStatus' - The response's http status code.
--
-- 'operationIdentifier', 'disableControlResponse_operationIdentifier' - The ID of the asynchronous operation, which is used to track status. The
-- operation is available for 90 days.
newDisableControlResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'operationIdentifier'
  Prelude.Text ->
  DisableControlResponse
newDisableControlResponse
  pHttpStatus_
  pOperationIdentifier_ =
    DisableControlResponse'
      { httpStatus = pHttpStatus_,
        operationIdentifier = pOperationIdentifier_
      }

-- | The response's http status code.
disableControlResponse_httpStatus :: Lens.Lens' DisableControlResponse Prelude.Int
disableControlResponse_httpStatus = Lens.lens (\DisableControlResponse' {httpStatus} -> httpStatus) (\s@DisableControlResponse' {} a -> s {httpStatus = a} :: DisableControlResponse)

-- | The ID of the asynchronous operation, which is used to track status. The
-- operation is available for 90 days.
disableControlResponse_operationIdentifier :: Lens.Lens' DisableControlResponse Prelude.Text
disableControlResponse_operationIdentifier = Lens.lens (\DisableControlResponse' {operationIdentifier} -> operationIdentifier) (\s@DisableControlResponse' {} a -> s {operationIdentifier = a} :: DisableControlResponse)

instance Prelude.NFData DisableControlResponse where
  rnf DisableControlResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf operationIdentifier
