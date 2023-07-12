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
-- Module      : Amazonka.SecurityHub.UpdateStandardsControl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to control whether an individual security standard control is
-- enabled or disabled.
module Amazonka.SecurityHub.UpdateStandardsControl
  ( -- * Creating a Request
    UpdateStandardsControl (..),
    newUpdateStandardsControl,

    -- * Request Lenses
    updateStandardsControl_controlStatus,
    updateStandardsControl_disabledReason,
    updateStandardsControl_standardsControlArn,

    -- * Destructuring the Response
    UpdateStandardsControlResponse (..),
    newUpdateStandardsControlResponse,

    -- * Response Lenses
    updateStandardsControlResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newUpdateStandardsControl' smart constructor.
data UpdateStandardsControl = UpdateStandardsControl'
  { -- | The updated status of the security standard control.
    controlStatus :: Prelude.Maybe ControlStatus,
    -- | A description of the reason why you are disabling a security standard
    -- control. If you are disabling a control, then this is required.
    disabledReason :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the security standard control to enable or disable.
    standardsControlArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStandardsControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'controlStatus', 'updateStandardsControl_controlStatus' - The updated status of the security standard control.
--
-- 'disabledReason', 'updateStandardsControl_disabledReason' - A description of the reason why you are disabling a security standard
-- control. If you are disabling a control, then this is required.
--
-- 'standardsControlArn', 'updateStandardsControl_standardsControlArn' - The ARN of the security standard control to enable or disable.
newUpdateStandardsControl ::
  -- | 'standardsControlArn'
  Prelude.Text ->
  UpdateStandardsControl
newUpdateStandardsControl pStandardsControlArn_ =
  UpdateStandardsControl'
    { controlStatus =
        Prelude.Nothing,
      disabledReason = Prelude.Nothing,
      standardsControlArn = pStandardsControlArn_
    }

-- | The updated status of the security standard control.
updateStandardsControl_controlStatus :: Lens.Lens' UpdateStandardsControl (Prelude.Maybe ControlStatus)
updateStandardsControl_controlStatus = Lens.lens (\UpdateStandardsControl' {controlStatus} -> controlStatus) (\s@UpdateStandardsControl' {} a -> s {controlStatus = a} :: UpdateStandardsControl)

-- | A description of the reason why you are disabling a security standard
-- control. If you are disabling a control, then this is required.
updateStandardsControl_disabledReason :: Lens.Lens' UpdateStandardsControl (Prelude.Maybe Prelude.Text)
updateStandardsControl_disabledReason = Lens.lens (\UpdateStandardsControl' {disabledReason} -> disabledReason) (\s@UpdateStandardsControl' {} a -> s {disabledReason = a} :: UpdateStandardsControl)

-- | The ARN of the security standard control to enable or disable.
updateStandardsControl_standardsControlArn :: Lens.Lens' UpdateStandardsControl Prelude.Text
updateStandardsControl_standardsControlArn = Lens.lens (\UpdateStandardsControl' {standardsControlArn} -> standardsControlArn) (\s@UpdateStandardsControl' {} a -> s {standardsControlArn = a} :: UpdateStandardsControl)

instance Core.AWSRequest UpdateStandardsControl where
  type
    AWSResponse UpdateStandardsControl =
      UpdateStandardsControlResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateStandardsControlResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateStandardsControl where
  hashWithSalt _salt UpdateStandardsControl' {..} =
    _salt
      `Prelude.hashWithSalt` controlStatus
      `Prelude.hashWithSalt` disabledReason
      `Prelude.hashWithSalt` standardsControlArn

instance Prelude.NFData UpdateStandardsControl where
  rnf UpdateStandardsControl' {..} =
    Prelude.rnf controlStatus
      `Prelude.seq` Prelude.rnf disabledReason
      `Prelude.seq` Prelude.rnf standardsControlArn

instance Data.ToHeaders UpdateStandardsControl where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateStandardsControl where
  toJSON UpdateStandardsControl' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ControlStatus" Data..=) Prelude.<$> controlStatus,
            ("DisabledReason" Data..=)
              Prelude.<$> disabledReason
          ]
      )

instance Data.ToPath UpdateStandardsControl where
  toPath UpdateStandardsControl' {..} =
    Prelude.mconcat
      [ "/standards/control/",
        Data.toBS standardsControlArn
      ]

instance Data.ToQuery UpdateStandardsControl where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateStandardsControlResponse' smart constructor.
data UpdateStandardsControlResponse = UpdateStandardsControlResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStandardsControlResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateStandardsControlResponse_httpStatus' - The response's http status code.
newUpdateStandardsControlResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateStandardsControlResponse
newUpdateStandardsControlResponse pHttpStatus_ =
  UpdateStandardsControlResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateStandardsControlResponse_httpStatus :: Lens.Lens' UpdateStandardsControlResponse Prelude.Int
updateStandardsControlResponse_httpStatus = Lens.lens (\UpdateStandardsControlResponse' {httpStatus} -> httpStatus) (\s@UpdateStandardsControlResponse' {} a -> s {httpStatus = a} :: UpdateStandardsControlResponse)

instance
  Prelude.NFData
    UpdateStandardsControlResponse
  where
  rnf UpdateStandardsControlResponse' {..} =
    Prelude.rnf httpStatus
