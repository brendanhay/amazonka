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
-- Module      : Amazonka.RDS.ApplyPendingMaintenanceAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies a pending maintenance action to a resource (for example, to a DB
-- instance).
module Amazonka.RDS.ApplyPendingMaintenanceAction
  ( -- * Creating a Request
    ApplyPendingMaintenanceAction (..),
    newApplyPendingMaintenanceAction,

    -- * Request Lenses
    applyPendingMaintenanceAction_resourceIdentifier,
    applyPendingMaintenanceAction_applyAction,
    applyPendingMaintenanceAction_optInType,

    -- * Destructuring the Response
    ApplyPendingMaintenanceActionResponse (..),
    newApplyPendingMaintenanceActionResponse,

    -- * Response Lenses
    applyPendingMaintenanceActionResponse_resourcePendingMaintenanceActions,
    applyPendingMaintenanceActionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newApplyPendingMaintenanceAction' smart constructor.
data ApplyPendingMaintenanceAction = ApplyPendingMaintenanceAction'
  { -- | The RDS Amazon Resource Name (ARN) of the resource that the pending
    -- maintenance action applies to. For information about creating an ARN,
    -- see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an RDS Amazon Resource Name (ARN)>.
    resourceIdentifier :: Prelude.Text,
    -- | The pending maintenance action to apply to this resource.
    --
    -- Valid values: @system-update@, @db-upgrade@, @hardware-maintenance@,
    -- @ca-certificate-rotation@
    applyAction :: Prelude.Text,
    -- | A value that specifies the type of opt-in request, or undoes an opt-in
    -- request. An opt-in request of type @immediate@ can\'t be undone.
    --
    -- Valid values:
    --
    -- -   @immediate@ - Apply the maintenance action immediately.
    --
    -- -   @next-maintenance@ - Apply the maintenance action during the next
    --     maintenance window for the resource.
    --
    -- -   @undo-opt-in@ - Cancel any existing @next-maintenance@ opt-in
    --     requests.
    optInType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplyPendingMaintenanceAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceIdentifier', 'applyPendingMaintenanceAction_resourceIdentifier' - The RDS Amazon Resource Name (ARN) of the resource that the pending
-- maintenance action applies to. For information about creating an ARN,
-- see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an RDS Amazon Resource Name (ARN)>.
--
-- 'applyAction', 'applyPendingMaintenanceAction_applyAction' - The pending maintenance action to apply to this resource.
--
-- Valid values: @system-update@, @db-upgrade@, @hardware-maintenance@,
-- @ca-certificate-rotation@
--
-- 'optInType', 'applyPendingMaintenanceAction_optInType' - A value that specifies the type of opt-in request, or undoes an opt-in
-- request. An opt-in request of type @immediate@ can\'t be undone.
--
-- Valid values:
--
-- -   @immediate@ - Apply the maintenance action immediately.
--
-- -   @next-maintenance@ - Apply the maintenance action during the next
--     maintenance window for the resource.
--
-- -   @undo-opt-in@ - Cancel any existing @next-maintenance@ opt-in
--     requests.
newApplyPendingMaintenanceAction ::
  -- | 'resourceIdentifier'
  Prelude.Text ->
  -- | 'applyAction'
  Prelude.Text ->
  -- | 'optInType'
  Prelude.Text ->
  ApplyPendingMaintenanceAction
newApplyPendingMaintenanceAction
  pResourceIdentifier_
  pApplyAction_
  pOptInType_ =
    ApplyPendingMaintenanceAction'
      { resourceIdentifier =
          pResourceIdentifier_,
        applyAction = pApplyAction_,
        optInType = pOptInType_
      }

-- | The RDS Amazon Resource Name (ARN) of the resource that the pending
-- maintenance action applies to. For information about creating an ARN,
-- see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an RDS Amazon Resource Name (ARN)>.
applyPendingMaintenanceAction_resourceIdentifier :: Lens.Lens' ApplyPendingMaintenanceAction Prelude.Text
applyPendingMaintenanceAction_resourceIdentifier = Lens.lens (\ApplyPendingMaintenanceAction' {resourceIdentifier} -> resourceIdentifier) (\s@ApplyPendingMaintenanceAction' {} a -> s {resourceIdentifier = a} :: ApplyPendingMaintenanceAction)

-- | The pending maintenance action to apply to this resource.
--
-- Valid values: @system-update@, @db-upgrade@, @hardware-maintenance@,
-- @ca-certificate-rotation@
applyPendingMaintenanceAction_applyAction :: Lens.Lens' ApplyPendingMaintenanceAction Prelude.Text
applyPendingMaintenanceAction_applyAction = Lens.lens (\ApplyPendingMaintenanceAction' {applyAction} -> applyAction) (\s@ApplyPendingMaintenanceAction' {} a -> s {applyAction = a} :: ApplyPendingMaintenanceAction)

-- | A value that specifies the type of opt-in request, or undoes an opt-in
-- request. An opt-in request of type @immediate@ can\'t be undone.
--
-- Valid values:
--
-- -   @immediate@ - Apply the maintenance action immediately.
--
-- -   @next-maintenance@ - Apply the maintenance action during the next
--     maintenance window for the resource.
--
-- -   @undo-opt-in@ - Cancel any existing @next-maintenance@ opt-in
--     requests.
applyPendingMaintenanceAction_optInType :: Lens.Lens' ApplyPendingMaintenanceAction Prelude.Text
applyPendingMaintenanceAction_optInType = Lens.lens (\ApplyPendingMaintenanceAction' {optInType} -> optInType) (\s@ApplyPendingMaintenanceAction' {} a -> s {optInType = a} :: ApplyPendingMaintenanceAction)

instance
  Core.AWSRequest
    ApplyPendingMaintenanceAction
  where
  type
    AWSResponse ApplyPendingMaintenanceAction =
      ApplyPendingMaintenanceActionResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ApplyPendingMaintenanceActionResult"
      ( \s h x ->
          ApplyPendingMaintenanceActionResponse'
            Prelude.<$> (x Data..@? "ResourcePendingMaintenanceActions")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ApplyPendingMaintenanceAction
  where
  hashWithSalt _salt ApplyPendingMaintenanceAction' {..} =
    _salt `Prelude.hashWithSalt` resourceIdentifier
      `Prelude.hashWithSalt` applyAction
      `Prelude.hashWithSalt` optInType

instance Prelude.NFData ApplyPendingMaintenanceAction where
  rnf ApplyPendingMaintenanceAction' {..} =
    Prelude.rnf resourceIdentifier
      `Prelude.seq` Prelude.rnf applyAction
      `Prelude.seq` Prelude.rnf optInType

instance Data.ToHeaders ApplyPendingMaintenanceAction where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ApplyPendingMaintenanceAction where
  toPath = Prelude.const "/"

instance Data.ToQuery ApplyPendingMaintenanceAction where
  toQuery ApplyPendingMaintenanceAction' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "ApplyPendingMaintenanceAction" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "ResourceIdentifier" Data.=: resourceIdentifier,
        "ApplyAction" Data.=: applyAction,
        "OptInType" Data.=: optInType
      ]

-- | /See:/ 'newApplyPendingMaintenanceActionResponse' smart constructor.
data ApplyPendingMaintenanceActionResponse = ApplyPendingMaintenanceActionResponse'
  { resourcePendingMaintenanceActions :: Prelude.Maybe ResourcePendingMaintenanceActions,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplyPendingMaintenanceActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourcePendingMaintenanceActions', 'applyPendingMaintenanceActionResponse_resourcePendingMaintenanceActions' - Undocumented member.
--
-- 'httpStatus', 'applyPendingMaintenanceActionResponse_httpStatus' - The response's http status code.
newApplyPendingMaintenanceActionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ApplyPendingMaintenanceActionResponse
newApplyPendingMaintenanceActionResponse pHttpStatus_ =
  ApplyPendingMaintenanceActionResponse'
    { resourcePendingMaintenanceActions =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
applyPendingMaintenanceActionResponse_resourcePendingMaintenanceActions :: Lens.Lens' ApplyPendingMaintenanceActionResponse (Prelude.Maybe ResourcePendingMaintenanceActions)
applyPendingMaintenanceActionResponse_resourcePendingMaintenanceActions = Lens.lens (\ApplyPendingMaintenanceActionResponse' {resourcePendingMaintenanceActions} -> resourcePendingMaintenanceActions) (\s@ApplyPendingMaintenanceActionResponse' {} a -> s {resourcePendingMaintenanceActions = a} :: ApplyPendingMaintenanceActionResponse)

-- | The response's http status code.
applyPendingMaintenanceActionResponse_httpStatus :: Lens.Lens' ApplyPendingMaintenanceActionResponse Prelude.Int
applyPendingMaintenanceActionResponse_httpStatus = Lens.lens (\ApplyPendingMaintenanceActionResponse' {httpStatus} -> httpStatus) (\s@ApplyPendingMaintenanceActionResponse' {} a -> s {httpStatus = a} :: ApplyPendingMaintenanceActionResponse)

instance
  Prelude.NFData
    ApplyPendingMaintenanceActionResponse
  where
  rnf ApplyPendingMaintenanceActionResponse' {..} =
    Prelude.rnf resourcePendingMaintenanceActions
      `Prelude.seq` Prelude.rnf httpStatus
