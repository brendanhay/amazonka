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
-- Module      : Amazonka.ControlTower.EnableControl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API call activates a control. It starts an asynchronous operation
-- that creates AWS resources on the specified organizational unit and the
-- accounts it contains. The resources created will vary according to the
-- control that you specify.
module Amazonka.ControlTower.EnableControl
  ( -- * Creating a Request
    EnableControl (..),
    newEnableControl,

    -- * Request Lenses
    enableControl_controlIdentifier,
    enableControl_targetIdentifier,

    -- * Destructuring the Response
    EnableControlResponse (..),
    newEnableControlResponse,

    -- * Response Lenses
    enableControlResponse_httpStatus,
    enableControlResponse_operationIdentifier,
  )
where

import Amazonka.ControlTower.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newEnableControl' smart constructor.
data EnableControl = EnableControl'
  { -- | The ARN of the control. Only __Strongly recommended__ and __Elective__
    -- controls are permitted, with the exception of the __Region deny__
    -- guardrail.
    controlIdentifier :: Prelude.Text,
    -- | The ARN of the organizational unit.
    targetIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'controlIdentifier', 'enableControl_controlIdentifier' - The ARN of the control. Only __Strongly recommended__ and __Elective__
-- controls are permitted, with the exception of the __Region deny__
-- guardrail.
--
-- 'targetIdentifier', 'enableControl_targetIdentifier' - The ARN of the organizational unit.
newEnableControl ::
  -- | 'controlIdentifier'
  Prelude.Text ->
  -- | 'targetIdentifier'
  Prelude.Text ->
  EnableControl
newEnableControl
  pControlIdentifier_
  pTargetIdentifier_ =
    EnableControl'
      { controlIdentifier =
          pControlIdentifier_,
        targetIdentifier = pTargetIdentifier_
      }

-- | The ARN of the control. Only __Strongly recommended__ and __Elective__
-- controls are permitted, with the exception of the __Region deny__
-- guardrail.
enableControl_controlIdentifier :: Lens.Lens' EnableControl Prelude.Text
enableControl_controlIdentifier = Lens.lens (\EnableControl' {controlIdentifier} -> controlIdentifier) (\s@EnableControl' {} a -> s {controlIdentifier = a} :: EnableControl)

-- | The ARN of the organizational unit.
enableControl_targetIdentifier :: Lens.Lens' EnableControl Prelude.Text
enableControl_targetIdentifier = Lens.lens (\EnableControl' {targetIdentifier} -> targetIdentifier) (\s@EnableControl' {} a -> s {targetIdentifier = a} :: EnableControl)

instance Core.AWSRequest EnableControl where
  type
    AWSResponse EnableControl =
      EnableControlResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          EnableControlResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "operationIdentifier")
      )

instance Prelude.Hashable EnableControl where
  hashWithSalt _salt EnableControl' {..} =
    _salt
      `Prelude.hashWithSalt` controlIdentifier
      `Prelude.hashWithSalt` targetIdentifier

instance Prelude.NFData EnableControl where
  rnf EnableControl' {..} =
    Prelude.rnf controlIdentifier
      `Prelude.seq` Prelude.rnf targetIdentifier

instance Data.ToHeaders EnableControl where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON EnableControl where
  toJSON EnableControl' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("controlIdentifier" Data..= controlIdentifier),
            Prelude.Just
              ("targetIdentifier" Data..= targetIdentifier)
          ]
      )

instance Data.ToPath EnableControl where
  toPath = Prelude.const "/enable-control"

instance Data.ToQuery EnableControl where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEnableControlResponse' smart constructor.
data EnableControlResponse = EnableControlResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the asynchronous operation, which is used to track status. The
    -- operation is available for 90 days.
    operationIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableControlResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'enableControlResponse_httpStatus' - The response's http status code.
--
-- 'operationIdentifier', 'enableControlResponse_operationIdentifier' - The ID of the asynchronous operation, which is used to track status. The
-- operation is available for 90 days.
newEnableControlResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'operationIdentifier'
  Prelude.Text ->
  EnableControlResponse
newEnableControlResponse
  pHttpStatus_
  pOperationIdentifier_ =
    EnableControlResponse'
      { httpStatus = pHttpStatus_,
        operationIdentifier = pOperationIdentifier_
      }

-- | The response's http status code.
enableControlResponse_httpStatus :: Lens.Lens' EnableControlResponse Prelude.Int
enableControlResponse_httpStatus = Lens.lens (\EnableControlResponse' {httpStatus} -> httpStatus) (\s@EnableControlResponse' {} a -> s {httpStatus = a} :: EnableControlResponse)

-- | The ID of the asynchronous operation, which is used to track status. The
-- operation is available for 90 days.
enableControlResponse_operationIdentifier :: Lens.Lens' EnableControlResponse Prelude.Text
enableControlResponse_operationIdentifier = Lens.lens (\EnableControlResponse' {operationIdentifier} -> operationIdentifier) (\s@EnableControlResponse' {} a -> s {operationIdentifier = a} :: EnableControlResponse)

instance Prelude.NFData EnableControlResponse where
  rnf EnableControlResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf operationIdentifier
