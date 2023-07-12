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
-- Module      : Amazonka.IoT.UpdateBillingGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates information about the billing group.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions UpdateBillingGroup>
-- action.
module Amazonka.IoT.UpdateBillingGroup
  ( -- * Creating a Request
    UpdateBillingGroup (..),
    newUpdateBillingGroup,

    -- * Request Lenses
    updateBillingGroup_expectedVersion,
    updateBillingGroup_billingGroupName,
    updateBillingGroup_billingGroupProperties,

    -- * Destructuring the Response
    UpdateBillingGroupResponse (..),
    newUpdateBillingGroupResponse,

    -- * Response Lenses
    updateBillingGroupResponse_version,
    updateBillingGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateBillingGroup' smart constructor.
data UpdateBillingGroup = UpdateBillingGroup'
  { -- | The expected version of the billing group. If the version of the billing
    -- group does not match the expected version specified in the request, the
    -- @UpdateBillingGroup@ request is rejected with a
    -- @VersionConflictException@.
    expectedVersion :: Prelude.Maybe Prelude.Integer,
    -- | The name of the billing group.
    billingGroupName :: Prelude.Text,
    -- | The properties of the billing group.
    billingGroupProperties :: BillingGroupProperties
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBillingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedVersion', 'updateBillingGroup_expectedVersion' - The expected version of the billing group. If the version of the billing
-- group does not match the expected version specified in the request, the
-- @UpdateBillingGroup@ request is rejected with a
-- @VersionConflictException@.
--
-- 'billingGroupName', 'updateBillingGroup_billingGroupName' - The name of the billing group.
--
-- 'billingGroupProperties', 'updateBillingGroup_billingGroupProperties' - The properties of the billing group.
newUpdateBillingGroup ::
  -- | 'billingGroupName'
  Prelude.Text ->
  -- | 'billingGroupProperties'
  BillingGroupProperties ->
  UpdateBillingGroup
newUpdateBillingGroup
  pBillingGroupName_
  pBillingGroupProperties_ =
    UpdateBillingGroup'
      { expectedVersion =
          Prelude.Nothing,
        billingGroupName = pBillingGroupName_,
        billingGroupProperties = pBillingGroupProperties_
      }

-- | The expected version of the billing group. If the version of the billing
-- group does not match the expected version specified in the request, the
-- @UpdateBillingGroup@ request is rejected with a
-- @VersionConflictException@.
updateBillingGroup_expectedVersion :: Lens.Lens' UpdateBillingGroup (Prelude.Maybe Prelude.Integer)
updateBillingGroup_expectedVersion = Lens.lens (\UpdateBillingGroup' {expectedVersion} -> expectedVersion) (\s@UpdateBillingGroup' {} a -> s {expectedVersion = a} :: UpdateBillingGroup)

-- | The name of the billing group.
updateBillingGroup_billingGroupName :: Lens.Lens' UpdateBillingGroup Prelude.Text
updateBillingGroup_billingGroupName = Lens.lens (\UpdateBillingGroup' {billingGroupName} -> billingGroupName) (\s@UpdateBillingGroup' {} a -> s {billingGroupName = a} :: UpdateBillingGroup)

-- | The properties of the billing group.
updateBillingGroup_billingGroupProperties :: Lens.Lens' UpdateBillingGroup BillingGroupProperties
updateBillingGroup_billingGroupProperties = Lens.lens (\UpdateBillingGroup' {billingGroupProperties} -> billingGroupProperties) (\s@UpdateBillingGroup' {} a -> s {billingGroupProperties = a} :: UpdateBillingGroup)

instance Core.AWSRequest UpdateBillingGroup where
  type
    AWSResponse UpdateBillingGroup =
      UpdateBillingGroupResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBillingGroupResponse'
            Prelude.<$> (x Data..?> "version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateBillingGroup where
  hashWithSalt _salt UpdateBillingGroup' {..} =
    _salt
      `Prelude.hashWithSalt` expectedVersion
      `Prelude.hashWithSalt` billingGroupName
      `Prelude.hashWithSalt` billingGroupProperties

instance Prelude.NFData UpdateBillingGroup where
  rnf UpdateBillingGroup' {..} =
    Prelude.rnf expectedVersion
      `Prelude.seq` Prelude.rnf billingGroupName
      `Prelude.seq` Prelude.rnf billingGroupProperties

instance Data.ToHeaders UpdateBillingGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateBillingGroup where
  toJSON UpdateBillingGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("expectedVersion" Data..=)
              Prelude.<$> expectedVersion,
            Prelude.Just
              ( "billingGroupProperties"
                  Data..= billingGroupProperties
              )
          ]
      )

instance Data.ToPath UpdateBillingGroup where
  toPath UpdateBillingGroup' {..} =
    Prelude.mconcat
      ["/billing-groups/", Data.toBS billingGroupName]

instance Data.ToQuery UpdateBillingGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateBillingGroupResponse' smart constructor.
data UpdateBillingGroupResponse = UpdateBillingGroupResponse'
  { -- | The latest version of the billing group.
    version :: Prelude.Maybe Prelude.Integer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBillingGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 'updateBillingGroupResponse_version' - The latest version of the billing group.
--
-- 'httpStatus', 'updateBillingGroupResponse_httpStatus' - The response's http status code.
newUpdateBillingGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateBillingGroupResponse
newUpdateBillingGroupResponse pHttpStatus_ =
  UpdateBillingGroupResponse'
    { version =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The latest version of the billing group.
updateBillingGroupResponse_version :: Lens.Lens' UpdateBillingGroupResponse (Prelude.Maybe Prelude.Integer)
updateBillingGroupResponse_version = Lens.lens (\UpdateBillingGroupResponse' {version} -> version) (\s@UpdateBillingGroupResponse' {} a -> s {version = a} :: UpdateBillingGroupResponse)

-- | The response's http status code.
updateBillingGroupResponse_httpStatus :: Lens.Lens' UpdateBillingGroupResponse Prelude.Int
updateBillingGroupResponse_httpStatus = Lens.lens (\UpdateBillingGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateBillingGroupResponse' {} a -> s {httpStatus = a} :: UpdateBillingGroupResponse)

instance Prelude.NFData UpdateBillingGroupResponse where
  rnf UpdateBillingGroupResponse' {..} =
    Prelude.rnf version
      `Prelude.seq` Prelude.rnf httpStatus
