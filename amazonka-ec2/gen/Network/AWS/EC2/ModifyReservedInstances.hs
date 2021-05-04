{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for ModifyReservedInstances.
--
-- /See:/ 'newModifyReservedInstances' smart constructor.
data ModifyReservedInstances = ModifyReservedInstances'
  { -- | A unique, case-sensitive token you provide to ensure idempotency of your
    -- modification request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the Reserved Instances to modify.
    reservedInstancesIds :: [Prelude.Text],
    -- | The configuration settings for the Reserved Instances to modify.
    targetConfigurations :: [ReservedInstancesConfiguration]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      reservedInstancesIds = Prelude.mempty,
      targetConfigurations = Prelude.mempty
    }

-- | A unique, case-sensitive token you provide to ensure idempotency of your
-- modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
modifyReservedInstances_clientToken :: Lens.Lens' ModifyReservedInstances (Prelude.Maybe Prelude.Text)
modifyReservedInstances_clientToken = Lens.lens (\ModifyReservedInstances' {clientToken} -> clientToken) (\s@ModifyReservedInstances' {} a -> s {clientToken = a} :: ModifyReservedInstances)

-- | The IDs of the Reserved Instances to modify.
modifyReservedInstances_reservedInstancesIds :: Lens.Lens' ModifyReservedInstances [Prelude.Text]
modifyReservedInstances_reservedInstancesIds = Lens.lens (\ModifyReservedInstances' {reservedInstancesIds} -> reservedInstancesIds) (\s@ModifyReservedInstances' {} a -> s {reservedInstancesIds = a} :: ModifyReservedInstances) Prelude.. Prelude._Coerce

-- | The configuration settings for the Reserved Instances to modify.
modifyReservedInstances_targetConfigurations :: Lens.Lens' ModifyReservedInstances [ReservedInstancesConfiguration]
modifyReservedInstances_targetConfigurations = Lens.lens (\ModifyReservedInstances' {targetConfigurations} -> targetConfigurations) (\s@ModifyReservedInstances' {} a -> s {targetConfigurations = a} :: ModifyReservedInstances) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest ModifyReservedInstances where
  type
    Rs ModifyReservedInstances =
      ModifyReservedInstancesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyReservedInstancesResponse'
            Prelude.<$> (x Prelude..@? "reservedInstancesModificationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyReservedInstances

instance Prelude.NFData ModifyReservedInstances

instance Prelude.ToHeaders ModifyReservedInstances where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ModifyReservedInstances where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ModifyReservedInstances where
  toQuery ModifyReservedInstances' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("ModifyReservedInstances" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "ClientToken" Prelude.=: clientToken,
        Prelude.toQueryList
          "ReservedInstancesId"
          reservedInstancesIds,
        Prelude.toQueryList
          "ReservedInstancesConfigurationSetItemType"
          targetConfigurations
      ]

-- | Contains the output of ModifyReservedInstances.
--
-- /See:/ 'newModifyReservedInstancesResponse' smart constructor.
data ModifyReservedInstancesResponse = ModifyReservedInstancesResponse'
  { -- | The ID for the modification.
    reservedInstancesModificationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ModifyReservedInstancesResponse
newModifyReservedInstancesResponse pHttpStatus_ =
  ModifyReservedInstancesResponse'
    { reservedInstancesModificationId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID for the modification.
modifyReservedInstancesResponse_reservedInstancesModificationId :: Lens.Lens' ModifyReservedInstancesResponse (Prelude.Maybe Prelude.Text)
modifyReservedInstancesResponse_reservedInstancesModificationId = Lens.lens (\ModifyReservedInstancesResponse' {reservedInstancesModificationId} -> reservedInstancesModificationId) (\s@ModifyReservedInstancesResponse' {} a -> s {reservedInstancesModificationId = a} :: ModifyReservedInstancesResponse)

-- | The response's http status code.
modifyReservedInstancesResponse_httpStatus :: Lens.Lens' ModifyReservedInstancesResponse Prelude.Int
modifyReservedInstancesResponse_httpStatus = Lens.lens (\ModifyReservedInstancesResponse' {httpStatus} -> httpStatus) (\s@ModifyReservedInstancesResponse' {} a -> s {httpStatus = a} :: ModifyReservedInstancesResponse)

instance
  Prelude.NFData
    ModifyReservedInstancesResponse
