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
-- Module      : Amazonka.VPCLattice.UpdateTargetGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified target group.
module Amazonka.VPCLattice.UpdateTargetGroup
  ( -- * Creating a Request
    UpdateTargetGroup (..),
    newUpdateTargetGroup,

    -- * Request Lenses
    updateTargetGroup_healthCheck,
    updateTargetGroup_targetGroupIdentifier,

    -- * Destructuring the Response
    UpdateTargetGroupResponse (..),
    newUpdateTargetGroupResponse,

    -- * Response Lenses
    updateTargetGroupResponse_arn,
    updateTargetGroupResponse_config,
    updateTargetGroupResponse_id,
    updateTargetGroupResponse_name,
    updateTargetGroupResponse_status,
    updateTargetGroupResponse_type,
    updateTargetGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newUpdateTargetGroup' smart constructor.
data UpdateTargetGroup = UpdateTargetGroup'
  { -- | The health check configuration.
    healthCheck :: HealthCheckConfig,
    -- | The ID or Amazon Resource Name (ARN) of the target group.
    targetGroupIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTargetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'healthCheck', 'updateTargetGroup_healthCheck' - The health check configuration.
--
-- 'targetGroupIdentifier', 'updateTargetGroup_targetGroupIdentifier' - The ID or Amazon Resource Name (ARN) of the target group.
newUpdateTargetGroup ::
  -- | 'healthCheck'
  HealthCheckConfig ->
  -- | 'targetGroupIdentifier'
  Prelude.Text ->
  UpdateTargetGroup
newUpdateTargetGroup
  pHealthCheck_
  pTargetGroupIdentifier_ =
    UpdateTargetGroup'
      { healthCheck = pHealthCheck_,
        targetGroupIdentifier = pTargetGroupIdentifier_
      }

-- | The health check configuration.
updateTargetGroup_healthCheck :: Lens.Lens' UpdateTargetGroup HealthCheckConfig
updateTargetGroup_healthCheck = Lens.lens (\UpdateTargetGroup' {healthCheck} -> healthCheck) (\s@UpdateTargetGroup' {} a -> s {healthCheck = a} :: UpdateTargetGroup)

-- | The ID or Amazon Resource Name (ARN) of the target group.
updateTargetGroup_targetGroupIdentifier :: Lens.Lens' UpdateTargetGroup Prelude.Text
updateTargetGroup_targetGroupIdentifier = Lens.lens (\UpdateTargetGroup' {targetGroupIdentifier} -> targetGroupIdentifier) (\s@UpdateTargetGroup' {} a -> s {targetGroupIdentifier = a} :: UpdateTargetGroup)

instance Core.AWSRequest UpdateTargetGroup where
  type
    AWSResponse UpdateTargetGroup =
      UpdateTargetGroupResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTargetGroupResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "config")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "type")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTargetGroup where
  hashWithSalt _salt UpdateTargetGroup' {..} =
    _salt
      `Prelude.hashWithSalt` healthCheck
      `Prelude.hashWithSalt` targetGroupIdentifier

instance Prelude.NFData UpdateTargetGroup where
  rnf UpdateTargetGroup' {..} =
    Prelude.rnf healthCheck
      `Prelude.seq` Prelude.rnf targetGroupIdentifier

instance Data.ToHeaders UpdateTargetGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateTargetGroup where
  toJSON UpdateTargetGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("healthCheck" Data..= healthCheck)]
      )

instance Data.ToPath UpdateTargetGroup where
  toPath UpdateTargetGroup' {..} =
    Prelude.mconcat
      ["/targetgroups/", Data.toBS targetGroupIdentifier]

instance Data.ToQuery UpdateTargetGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTargetGroupResponse' smart constructor.
data UpdateTargetGroupResponse = UpdateTargetGroupResponse'
  { -- | The Amazon Resource Name (ARN) of the target group.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The target group configuration.
    config :: Prelude.Maybe TargetGroupConfig,
    -- | The ID of the target group.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the target group.
    name :: Prelude.Maybe Prelude.Text,
    -- | The status.
    status :: Prelude.Maybe TargetGroupStatus,
    -- | The target group type.
    type' :: Prelude.Maybe TargetGroupType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTargetGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateTargetGroupResponse_arn' - The Amazon Resource Name (ARN) of the target group.
--
-- 'config', 'updateTargetGroupResponse_config' - The target group configuration.
--
-- 'id', 'updateTargetGroupResponse_id' - The ID of the target group.
--
-- 'name', 'updateTargetGroupResponse_name' - The name of the target group.
--
-- 'status', 'updateTargetGroupResponse_status' - The status.
--
-- 'type'', 'updateTargetGroupResponse_type' - The target group type.
--
-- 'httpStatus', 'updateTargetGroupResponse_httpStatus' - The response's http status code.
newUpdateTargetGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateTargetGroupResponse
newUpdateTargetGroupResponse pHttpStatus_ =
  UpdateTargetGroupResponse'
    { arn = Prelude.Nothing,
      config = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing,
      type' = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the target group.
updateTargetGroupResponse_arn :: Lens.Lens' UpdateTargetGroupResponse (Prelude.Maybe Prelude.Text)
updateTargetGroupResponse_arn = Lens.lens (\UpdateTargetGroupResponse' {arn} -> arn) (\s@UpdateTargetGroupResponse' {} a -> s {arn = a} :: UpdateTargetGroupResponse)

-- | The target group configuration.
updateTargetGroupResponse_config :: Lens.Lens' UpdateTargetGroupResponse (Prelude.Maybe TargetGroupConfig)
updateTargetGroupResponse_config = Lens.lens (\UpdateTargetGroupResponse' {config} -> config) (\s@UpdateTargetGroupResponse' {} a -> s {config = a} :: UpdateTargetGroupResponse)

-- | The ID of the target group.
updateTargetGroupResponse_id :: Lens.Lens' UpdateTargetGroupResponse (Prelude.Maybe Prelude.Text)
updateTargetGroupResponse_id = Lens.lens (\UpdateTargetGroupResponse' {id} -> id) (\s@UpdateTargetGroupResponse' {} a -> s {id = a} :: UpdateTargetGroupResponse)

-- | The name of the target group.
updateTargetGroupResponse_name :: Lens.Lens' UpdateTargetGroupResponse (Prelude.Maybe Prelude.Text)
updateTargetGroupResponse_name = Lens.lens (\UpdateTargetGroupResponse' {name} -> name) (\s@UpdateTargetGroupResponse' {} a -> s {name = a} :: UpdateTargetGroupResponse)

-- | The status.
updateTargetGroupResponse_status :: Lens.Lens' UpdateTargetGroupResponse (Prelude.Maybe TargetGroupStatus)
updateTargetGroupResponse_status = Lens.lens (\UpdateTargetGroupResponse' {status} -> status) (\s@UpdateTargetGroupResponse' {} a -> s {status = a} :: UpdateTargetGroupResponse)

-- | The target group type.
updateTargetGroupResponse_type :: Lens.Lens' UpdateTargetGroupResponse (Prelude.Maybe TargetGroupType)
updateTargetGroupResponse_type = Lens.lens (\UpdateTargetGroupResponse' {type'} -> type') (\s@UpdateTargetGroupResponse' {} a -> s {type' = a} :: UpdateTargetGroupResponse)

-- | The response's http status code.
updateTargetGroupResponse_httpStatus :: Lens.Lens' UpdateTargetGroupResponse Prelude.Int
updateTargetGroupResponse_httpStatus = Lens.lens (\UpdateTargetGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateTargetGroupResponse' {} a -> s {httpStatus = a} :: UpdateTargetGroupResponse)

instance Prelude.NFData UpdateTargetGroupResponse where
  rnf UpdateTargetGroupResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf config
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf httpStatus
