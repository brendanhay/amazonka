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
-- Module      : Network.AWS.IoT.UpdateBillingGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates information about the billing group.
module Network.AWS.IoT.UpdateBillingGroup
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

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateBillingGroup' smart constructor.
data UpdateBillingGroup = UpdateBillingGroup'
  { -- | The expected version of the billing group. If the version of the billing
    -- group does not match the expected version specified in the request, the
    -- @UpdateBillingGroup@ request is rejected with a
    -- @VersionConflictException@.
    expectedVersion :: Core.Maybe Core.Integer,
    -- | The name of the billing group.
    billingGroupName :: Core.Text,
    -- | The properties of the billing group.
    billingGroupProperties :: BillingGroupProperties
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'billingGroupProperties'
  BillingGroupProperties ->
  UpdateBillingGroup
newUpdateBillingGroup
  pBillingGroupName_
  pBillingGroupProperties_ =
    UpdateBillingGroup'
      { expectedVersion = Core.Nothing,
        billingGroupName = pBillingGroupName_,
        billingGroupProperties = pBillingGroupProperties_
      }

-- | The expected version of the billing group. If the version of the billing
-- group does not match the expected version specified in the request, the
-- @UpdateBillingGroup@ request is rejected with a
-- @VersionConflictException@.
updateBillingGroup_expectedVersion :: Lens.Lens' UpdateBillingGroup (Core.Maybe Core.Integer)
updateBillingGroup_expectedVersion = Lens.lens (\UpdateBillingGroup' {expectedVersion} -> expectedVersion) (\s@UpdateBillingGroup' {} a -> s {expectedVersion = a} :: UpdateBillingGroup)

-- | The name of the billing group.
updateBillingGroup_billingGroupName :: Lens.Lens' UpdateBillingGroup Core.Text
updateBillingGroup_billingGroupName = Lens.lens (\UpdateBillingGroup' {billingGroupName} -> billingGroupName) (\s@UpdateBillingGroup' {} a -> s {billingGroupName = a} :: UpdateBillingGroup)

-- | The properties of the billing group.
updateBillingGroup_billingGroupProperties :: Lens.Lens' UpdateBillingGroup BillingGroupProperties
updateBillingGroup_billingGroupProperties = Lens.lens (\UpdateBillingGroup' {billingGroupProperties} -> billingGroupProperties) (\s@UpdateBillingGroup' {} a -> s {billingGroupProperties = a} :: UpdateBillingGroup)

instance Core.AWSRequest UpdateBillingGroup where
  type
    AWSResponse UpdateBillingGroup =
      UpdateBillingGroupResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBillingGroupResponse'
            Core.<$> (x Core..?> "version")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateBillingGroup

instance Core.NFData UpdateBillingGroup

instance Core.ToHeaders UpdateBillingGroup where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON UpdateBillingGroup where
  toJSON UpdateBillingGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ ("expectedVersion" Core..=)
              Core.<$> expectedVersion,
            Core.Just
              ( "billingGroupProperties"
                  Core..= billingGroupProperties
              )
          ]
      )

instance Core.ToPath UpdateBillingGroup where
  toPath UpdateBillingGroup' {..} =
    Core.mconcat
      ["/billing-groups/", Core.toBS billingGroupName]

instance Core.ToQuery UpdateBillingGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateBillingGroupResponse' smart constructor.
data UpdateBillingGroupResponse = UpdateBillingGroupResponse'
  { -- | The latest version of the billing group.
    version :: Core.Maybe Core.Integer,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateBillingGroupResponse
newUpdateBillingGroupResponse pHttpStatus_ =
  UpdateBillingGroupResponse'
    { version = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The latest version of the billing group.
updateBillingGroupResponse_version :: Lens.Lens' UpdateBillingGroupResponse (Core.Maybe Core.Integer)
updateBillingGroupResponse_version = Lens.lens (\UpdateBillingGroupResponse' {version} -> version) (\s@UpdateBillingGroupResponse' {} a -> s {version = a} :: UpdateBillingGroupResponse)

-- | The response's http status code.
updateBillingGroupResponse_httpStatus :: Lens.Lens' UpdateBillingGroupResponse Core.Int
updateBillingGroupResponse_httpStatus = Lens.lens (\UpdateBillingGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateBillingGroupResponse' {} a -> s {httpStatus = a} :: UpdateBillingGroupResponse)

instance Core.NFData UpdateBillingGroupResponse
