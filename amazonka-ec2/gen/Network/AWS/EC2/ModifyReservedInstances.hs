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
-- Module      : Network.AWS.EC2.ModifyReservedInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the Availability Zone, instance count, instance type, or
-- network platform (EC2-Classic or EC2-VPC) of your Reserved Instances.
-- The Reserved Instances to be modified must be identical, except for
-- Availability Zone, network platform, and instance type.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-modifying.html Modifying Reserved Instances>
-- in the /Amazon EC2 User Guide/.
module Network.AWS.EC2.ModifyReservedInstances
  ( -- * Creating a Request
    ModifyReservedInstances (..),
    newModifyReservedInstances,

    -- * Request Lenses
    modifyReservedInstances_clientToken,
    modifyReservedInstances_reservedInstancesIds,
    modifyReservedInstances_targetConfigurations,

    -- * Destructuring the Response
    ModifyReservedInstancesResponse (..),
    newModifyReservedInstancesResponse,

    -- * Response Lenses
    modifyReservedInstancesResponse_reservedInstancesModificationId,
    modifyReservedInstancesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for ModifyReservedInstances.
--
-- /See:/ 'newModifyReservedInstances' smart constructor.
data ModifyReservedInstances = ModifyReservedInstances'
  { -- | A unique, case-sensitive token you provide to ensure idempotency of your
    -- modification request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Core.Maybe Core.Text,
    -- | The IDs of the Reserved Instances to modify.
    reservedInstancesIds :: [Core.Text],
    -- | The configuration settings for the Reserved Instances to modify.
    targetConfigurations :: [ReservedInstancesConfiguration]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyReservedInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'modifyReservedInstances_clientToken' - A unique, case-sensitive token you provide to ensure idempotency of your
-- modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- 'reservedInstancesIds', 'modifyReservedInstances_reservedInstancesIds' - The IDs of the Reserved Instances to modify.
--
-- 'targetConfigurations', 'modifyReservedInstances_targetConfigurations' - The configuration settings for the Reserved Instances to modify.
newModifyReservedInstances ::
  ModifyReservedInstances
newModifyReservedInstances =
  ModifyReservedInstances'
    { clientToken =
        Core.Nothing,
      reservedInstancesIds = Core.mempty,
      targetConfigurations = Core.mempty
    }

-- | A unique, case-sensitive token you provide to ensure idempotency of your
-- modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
modifyReservedInstances_clientToken :: Lens.Lens' ModifyReservedInstances (Core.Maybe Core.Text)
modifyReservedInstances_clientToken = Lens.lens (\ModifyReservedInstances' {clientToken} -> clientToken) (\s@ModifyReservedInstances' {} a -> s {clientToken = a} :: ModifyReservedInstances)

-- | The IDs of the Reserved Instances to modify.
modifyReservedInstances_reservedInstancesIds :: Lens.Lens' ModifyReservedInstances [Core.Text]
modifyReservedInstances_reservedInstancesIds = Lens.lens (\ModifyReservedInstances' {reservedInstancesIds} -> reservedInstancesIds) (\s@ModifyReservedInstances' {} a -> s {reservedInstancesIds = a} :: ModifyReservedInstances) Core.. Lens._Coerce

-- | The configuration settings for the Reserved Instances to modify.
modifyReservedInstances_targetConfigurations :: Lens.Lens' ModifyReservedInstances [ReservedInstancesConfiguration]
modifyReservedInstances_targetConfigurations = Lens.lens (\ModifyReservedInstances' {targetConfigurations} -> targetConfigurations) (\s@ModifyReservedInstances' {} a -> s {targetConfigurations = a} :: ModifyReservedInstances) Core.. Lens._Coerce

instance Core.AWSRequest ModifyReservedInstances where
  type
    AWSResponse ModifyReservedInstances =
      ModifyReservedInstancesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyReservedInstancesResponse'
            Core.<$> (x Core..@? "reservedInstancesModificationId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyReservedInstances

instance Core.NFData ModifyReservedInstances

instance Core.ToHeaders ModifyReservedInstances where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ModifyReservedInstances where
  toPath = Core.const "/"

instance Core.ToQuery ModifyReservedInstances where
  toQuery ModifyReservedInstances' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ModifyReservedInstances" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "ClientToken" Core.=: clientToken,
        Core.toQueryList
          "ReservedInstancesId"
          reservedInstancesIds,
        Core.toQueryList
          "ReservedInstancesConfigurationSetItemType"
          targetConfigurations
      ]

-- | Contains the output of ModifyReservedInstances.
--
-- /See:/ 'newModifyReservedInstancesResponse' smart constructor.
data ModifyReservedInstancesResponse = ModifyReservedInstancesResponse'
  { -- | The ID for the modification.
    reservedInstancesModificationId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyReservedInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservedInstancesModificationId', 'modifyReservedInstancesResponse_reservedInstancesModificationId' - The ID for the modification.
--
-- 'httpStatus', 'modifyReservedInstancesResponse_httpStatus' - The response's http status code.
newModifyReservedInstancesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ModifyReservedInstancesResponse
newModifyReservedInstancesResponse pHttpStatus_ =
  ModifyReservedInstancesResponse'
    { reservedInstancesModificationId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID for the modification.
modifyReservedInstancesResponse_reservedInstancesModificationId :: Lens.Lens' ModifyReservedInstancesResponse (Core.Maybe Core.Text)
modifyReservedInstancesResponse_reservedInstancesModificationId = Lens.lens (\ModifyReservedInstancesResponse' {reservedInstancesModificationId} -> reservedInstancesModificationId) (\s@ModifyReservedInstancesResponse' {} a -> s {reservedInstancesModificationId = a} :: ModifyReservedInstancesResponse)

-- | The response's http status code.
modifyReservedInstancesResponse_httpStatus :: Lens.Lens' ModifyReservedInstancesResponse Core.Int
modifyReservedInstancesResponse_httpStatus = Lens.lens (\ModifyReservedInstancesResponse' {httpStatus} -> httpStatus) (\s@ModifyReservedInstancesResponse' {} a -> s {httpStatus = a} :: ModifyReservedInstancesResponse)

instance Core.NFData ModifyReservedInstancesResponse
