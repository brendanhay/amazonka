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
-- Module      : Amazonka.EC2.ModifyInstanceMaintenanceOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the recovery behavior of your instance to disable simplified
-- automatic recovery or set the recovery behavior to default. The default
-- configuration will not enable simplified automatic recovery for an
-- unsupported instance type. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-recover.html#instance-configuration-recovery Simplified automatic recovery>.
module Amazonka.EC2.ModifyInstanceMaintenanceOptions
  ( -- * Creating a Request
    ModifyInstanceMaintenanceOptions (..),
    newModifyInstanceMaintenanceOptions,

    -- * Request Lenses
    modifyInstanceMaintenanceOptions_autoRecovery,
    modifyInstanceMaintenanceOptions_dryRun,
    modifyInstanceMaintenanceOptions_instanceId,

    -- * Destructuring the Response
    ModifyInstanceMaintenanceOptionsResponse (..),
    newModifyInstanceMaintenanceOptionsResponse,

    -- * Response Lenses
    modifyInstanceMaintenanceOptionsResponse_autoRecovery,
    modifyInstanceMaintenanceOptionsResponse_instanceId,
    modifyInstanceMaintenanceOptionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyInstanceMaintenanceOptions' smart constructor.
data ModifyInstanceMaintenanceOptions = ModifyInstanceMaintenanceOptions'
  { -- | Disables the automatic recovery behavior of your instance or sets it to
    -- default.
    autoRecovery :: Prelude.Maybe InstanceAutoRecoveryState,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyInstanceMaintenanceOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoRecovery', 'modifyInstanceMaintenanceOptions_autoRecovery' - Disables the automatic recovery behavior of your instance or sets it to
-- default.
--
-- 'dryRun', 'modifyInstanceMaintenanceOptions_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'instanceId', 'modifyInstanceMaintenanceOptions_instanceId' - The ID of the instance.
newModifyInstanceMaintenanceOptions ::
  -- | 'instanceId'
  Prelude.Text ->
  ModifyInstanceMaintenanceOptions
newModifyInstanceMaintenanceOptions pInstanceId_ =
  ModifyInstanceMaintenanceOptions'
    { autoRecovery =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | Disables the automatic recovery behavior of your instance or sets it to
-- default.
modifyInstanceMaintenanceOptions_autoRecovery :: Lens.Lens' ModifyInstanceMaintenanceOptions (Prelude.Maybe InstanceAutoRecoveryState)
modifyInstanceMaintenanceOptions_autoRecovery = Lens.lens (\ModifyInstanceMaintenanceOptions' {autoRecovery} -> autoRecovery) (\s@ModifyInstanceMaintenanceOptions' {} a -> s {autoRecovery = a} :: ModifyInstanceMaintenanceOptions)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyInstanceMaintenanceOptions_dryRun :: Lens.Lens' ModifyInstanceMaintenanceOptions (Prelude.Maybe Prelude.Bool)
modifyInstanceMaintenanceOptions_dryRun = Lens.lens (\ModifyInstanceMaintenanceOptions' {dryRun} -> dryRun) (\s@ModifyInstanceMaintenanceOptions' {} a -> s {dryRun = a} :: ModifyInstanceMaintenanceOptions)

-- | The ID of the instance.
modifyInstanceMaintenanceOptions_instanceId :: Lens.Lens' ModifyInstanceMaintenanceOptions Prelude.Text
modifyInstanceMaintenanceOptions_instanceId = Lens.lens (\ModifyInstanceMaintenanceOptions' {instanceId} -> instanceId) (\s@ModifyInstanceMaintenanceOptions' {} a -> s {instanceId = a} :: ModifyInstanceMaintenanceOptions)

instance
  Core.AWSRequest
    ModifyInstanceMaintenanceOptions
  where
  type
    AWSResponse ModifyInstanceMaintenanceOptions =
      ModifyInstanceMaintenanceOptionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyInstanceMaintenanceOptionsResponse'
            Prelude.<$> (x Data..@? "autoRecovery")
            Prelude.<*> (x Data..@? "instanceId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyInstanceMaintenanceOptions
  where
  hashWithSalt
    _salt
    ModifyInstanceMaintenanceOptions' {..} =
      _salt
        `Prelude.hashWithSalt` autoRecovery
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` instanceId

instance
  Prelude.NFData
    ModifyInstanceMaintenanceOptions
  where
  rnf ModifyInstanceMaintenanceOptions' {..} =
    Prelude.rnf autoRecovery
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf instanceId

instance
  Data.ToHeaders
    ModifyInstanceMaintenanceOptions
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyInstanceMaintenanceOptions where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ModifyInstanceMaintenanceOptions
  where
  toQuery ModifyInstanceMaintenanceOptions' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "ModifyInstanceMaintenanceOptions" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "AutoRecovery" Data.=: autoRecovery,
        "DryRun" Data.=: dryRun,
        "InstanceId" Data.=: instanceId
      ]

-- | /See:/ 'newModifyInstanceMaintenanceOptionsResponse' smart constructor.
data ModifyInstanceMaintenanceOptionsResponse = ModifyInstanceMaintenanceOptionsResponse'
  { -- | Provides information on the current automatic recovery behavior of your
    -- instance.
    autoRecovery :: Prelude.Maybe InstanceAutoRecoveryState,
    -- | The ID of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyInstanceMaintenanceOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoRecovery', 'modifyInstanceMaintenanceOptionsResponse_autoRecovery' - Provides information on the current automatic recovery behavior of your
-- instance.
--
-- 'instanceId', 'modifyInstanceMaintenanceOptionsResponse_instanceId' - The ID of the instance.
--
-- 'httpStatus', 'modifyInstanceMaintenanceOptionsResponse_httpStatus' - The response's http status code.
newModifyInstanceMaintenanceOptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyInstanceMaintenanceOptionsResponse
newModifyInstanceMaintenanceOptionsResponse
  pHttpStatus_ =
    ModifyInstanceMaintenanceOptionsResponse'
      { autoRecovery =
          Prelude.Nothing,
        instanceId = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Provides information on the current automatic recovery behavior of your
-- instance.
modifyInstanceMaintenanceOptionsResponse_autoRecovery :: Lens.Lens' ModifyInstanceMaintenanceOptionsResponse (Prelude.Maybe InstanceAutoRecoveryState)
modifyInstanceMaintenanceOptionsResponse_autoRecovery = Lens.lens (\ModifyInstanceMaintenanceOptionsResponse' {autoRecovery} -> autoRecovery) (\s@ModifyInstanceMaintenanceOptionsResponse' {} a -> s {autoRecovery = a} :: ModifyInstanceMaintenanceOptionsResponse)

-- | The ID of the instance.
modifyInstanceMaintenanceOptionsResponse_instanceId :: Lens.Lens' ModifyInstanceMaintenanceOptionsResponse (Prelude.Maybe Prelude.Text)
modifyInstanceMaintenanceOptionsResponse_instanceId = Lens.lens (\ModifyInstanceMaintenanceOptionsResponse' {instanceId} -> instanceId) (\s@ModifyInstanceMaintenanceOptionsResponse' {} a -> s {instanceId = a} :: ModifyInstanceMaintenanceOptionsResponse)

-- | The response's http status code.
modifyInstanceMaintenanceOptionsResponse_httpStatus :: Lens.Lens' ModifyInstanceMaintenanceOptionsResponse Prelude.Int
modifyInstanceMaintenanceOptionsResponse_httpStatus = Lens.lens (\ModifyInstanceMaintenanceOptionsResponse' {httpStatus} -> httpStatus) (\s@ModifyInstanceMaintenanceOptionsResponse' {} a -> s {httpStatus = a} :: ModifyInstanceMaintenanceOptionsResponse)

instance
  Prelude.NFData
    ModifyInstanceMaintenanceOptionsResponse
  where
  rnf ModifyInstanceMaintenanceOptionsResponse' {..} =
    Prelude.rnf autoRecovery
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf httpStatus
