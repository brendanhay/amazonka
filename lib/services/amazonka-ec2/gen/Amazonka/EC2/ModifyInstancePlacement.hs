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
-- Module      : Amazonka.EC2.ModifyInstancePlacement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the placement attributes for a specified instance. You can do
-- the following:
--
-- -   Modify the affinity between an instance and a
--     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-hosts-overview.html Dedicated Host>.
--     When affinity is set to @host@ and the instance is not associated
--     with a specific Dedicated Host, the next time the instance is
--     launched, it is automatically associated with the host on which it
--     lands. If the instance is restarted or rebooted, this relationship
--     persists.
--
-- -   Change the Dedicated Host with which an instance is associated.
--
-- -   Change the instance tenancy of an instance.
--
-- -   Move an instance to or from a
--     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html placement group>.
--
-- At least one attribute for affinity, host ID, tenancy, or placement
-- group name must be specified in the request. Affinity and tenancy can be
-- modified in the same request.
--
-- To modify the host ID, tenancy, placement group, or partition for an
-- instance, the instance must be in the @stopped@ state.
module Amazonka.EC2.ModifyInstancePlacement
  ( -- * Creating a Request
    ModifyInstancePlacement (..),
    newModifyInstancePlacement,

    -- * Request Lenses
    modifyInstancePlacement_affinity,
    modifyInstancePlacement_groupId,
    modifyInstancePlacement_groupName,
    modifyInstancePlacement_hostId,
    modifyInstancePlacement_hostResourceGroupArn,
    modifyInstancePlacement_partitionNumber,
    modifyInstancePlacement_tenancy,
    modifyInstancePlacement_instanceId,

    -- * Destructuring the Response
    ModifyInstancePlacementResponse (..),
    newModifyInstancePlacementResponse,

    -- * Response Lenses
    modifyInstancePlacementResponse_return,
    modifyInstancePlacementResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyInstancePlacement' smart constructor.
data ModifyInstancePlacement = ModifyInstancePlacement'
  { -- | The affinity setting for the instance.
    affinity :: Prelude.Maybe Affinity,
    -- | The Group Id of a placement group. You must specify the Placement Group
    -- __Group Id__ to launch an instance in a shared placement group.
    groupId :: Prelude.Maybe Prelude.Text,
    -- | The name of the placement group in which to place the instance. For
    -- spread placement groups, the instance must have a tenancy of @default@.
    -- For cluster and partition placement groups, the instance must have a
    -- tenancy of @default@ or @dedicated@.
    --
    -- To remove an instance from a placement group, specify an empty string
    -- (\"\").
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Dedicated Host with which to associate the instance.
    hostId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the host resource group in which to place the instance.
    hostResourceGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The number of the partition in which to place the instance. Valid only
    -- if the placement group strategy is set to @partition@.
    partitionNumber :: Prelude.Maybe Prelude.Int,
    -- | The tenancy for the instance.
    --
    -- For T3 instances, you can\'t change the tenancy from @dedicated@ to
    -- @host@, or from @host@ to @dedicated@. Attempting to make one of these
    -- unsupported tenancy changes results in the @InvalidTenancy@ error code.
    tenancy :: Prelude.Maybe HostTenancy,
    -- | The ID of the instance that you are modifying.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyInstancePlacement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'affinity', 'modifyInstancePlacement_affinity' - The affinity setting for the instance.
--
-- 'groupId', 'modifyInstancePlacement_groupId' - The Group Id of a placement group. You must specify the Placement Group
-- __Group Id__ to launch an instance in a shared placement group.
--
-- 'groupName', 'modifyInstancePlacement_groupName' - The name of the placement group in which to place the instance. For
-- spread placement groups, the instance must have a tenancy of @default@.
-- For cluster and partition placement groups, the instance must have a
-- tenancy of @default@ or @dedicated@.
--
-- To remove an instance from a placement group, specify an empty string
-- (\"\").
--
-- 'hostId', 'modifyInstancePlacement_hostId' - The ID of the Dedicated Host with which to associate the instance.
--
-- 'hostResourceGroupArn', 'modifyInstancePlacement_hostResourceGroupArn' - The ARN of the host resource group in which to place the instance.
--
-- 'partitionNumber', 'modifyInstancePlacement_partitionNumber' - The number of the partition in which to place the instance. Valid only
-- if the placement group strategy is set to @partition@.
--
-- 'tenancy', 'modifyInstancePlacement_tenancy' - The tenancy for the instance.
--
-- For T3 instances, you can\'t change the tenancy from @dedicated@ to
-- @host@, or from @host@ to @dedicated@. Attempting to make one of these
-- unsupported tenancy changes results in the @InvalidTenancy@ error code.
--
-- 'instanceId', 'modifyInstancePlacement_instanceId' - The ID of the instance that you are modifying.
newModifyInstancePlacement ::
  -- | 'instanceId'
  Prelude.Text ->
  ModifyInstancePlacement
newModifyInstancePlacement pInstanceId_ =
  ModifyInstancePlacement'
    { affinity =
        Prelude.Nothing,
      groupId = Prelude.Nothing,
      groupName = Prelude.Nothing,
      hostId = Prelude.Nothing,
      hostResourceGroupArn = Prelude.Nothing,
      partitionNumber = Prelude.Nothing,
      tenancy = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | The affinity setting for the instance.
modifyInstancePlacement_affinity :: Lens.Lens' ModifyInstancePlacement (Prelude.Maybe Affinity)
modifyInstancePlacement_affinity = Lens.lens (\ModifyInstancePlacement' {affinity} -> affinity) (\s@ModifyInstancePlacement' {} a -> s {affinity = a} :: ModifyInstancePlacement)

-- | The Group Id of a placement group. You must specify the Placement Group
-- __Group Id__ to launch an instance in a shared placement group.
modifyInstancePlacement_groupId :: Lens.Lens' ModifyInstancePlacement (Prelude.Maybe Prelude.Text)
modifyInstancePlacement_groupId = Lens.lens (\ModifyInstancePlacement' {groupId} -> groupId) (\s@ModifyInstancePlacement' {} a -> s {groupId = a} :: ModifyInstancePlacement)

-- | The name of the placement group in which to place the instance. For
-- spread placement groups, the instance must have a tenancy of @default@.
-- For cluster and partition placement groups, the instance must have a
-- tenancy of @default@ or @dedicated@.
--
-- To remove an instance from a placement group, specify an empty string
-- (\"\").
modifyInstancePlacement_groupName :: Lens.Lens' ModifyInstancePlacement (Prelude.Maybe Prelude.Text)
modifyInstancePlacement_groupName = Lens.lens (\ModifyInstancePlacement' {groupName} -> groupName) (\s@ModifyInstancePlacement' {} a -> s {groupName = a} :: ModifyInstancePlacement)

-- | The ID of the Dedicated Host with which to associate the instance.
modifyInstancePlacement_hostId :: Lens.Lens' ModifyInstancePlacement (Prelude.Maybe Prelude.Text)
modifyInstancePlacement_hostId = Lens.lens (\ModifyInstancePlacement' {hostId} -> hostId) (\s@ModifyInstancePlacement' {} a -> s {hostId = a} :: ModifyInstancePlacement)

-- | The ARN of the host resource group in which to place the instance.
modifyInstancePlacement_hostResourceGroupArn :: Lens.Lens' ModifyInstancePlacement (Prelude.Maybe Prelude.Text)
modifyInstancePlacement_hostResourceGroupArn = Lens.lens (\ModifyInstancePlacement' {hostResourceGroupArn} -> hostResourceGroupArn) (\s@ModifyInstancePlacement' {} a -> s {hostResourceGroupArn = a} :: ModifyInstancePlacement)

-- | The number of the partition in which to place the instance. Valid only
-- if the placement group strategy is set to @partition@.
modifyInstancePlacement_partitionNumber :: Lens.Lens' ModifyInstancePlacement (Prelude.Maybe Prelude.Int)
modifyInstancePlacement_partitionNumber = Lens.lens (\ModifyInstancePlacement' {partitionNumber} -> partitionNumber) (\s@ModifyInstancePlacement' {} a -> s {partitionNumber = a} :: ModifyInstancePlacement)

-- | The tenancy for the instance.
--
-- For T3 instances, you can\'t change the tenancy from @dedicated@ to
-- @host@, or from @host@ to @dedicated@. Attempting to make one of these
-- unsupported tenancy changes results in the @InvalidTenancy@ error code.
modifyInstancePlacement_tenancy :: Lens.Lens' ModifyInstancePlacement (Prelude.Maybe HostTenancy)
modifyInstancePlacement_tenancy = Lens.lens (\ModifyInstancePlacement' {tenancy} -> tenancy) (\s@ModifyInstancePlacement' {} a -> s {tenancy = a} :: ModifyInstancePlacement)

-- | The ID of the instance that you are modifying.
modifyInstancePlacement_instanceId :: Lens.Lens' ModifyInstancePlacement Prelude.Text
modifyInstancePlacement_instanceId = Lens.lens (\ModifyInstancePlacement' {instanceId} -> instanceId) (\s@ModifyInstancePlacement' {} a -> s {instanceId = a} :: ModifyInstancePlacement)

instance Core.AWSRequest ModifyInstancePlacement where
  type
    AWSResponse ModifyInstancePlacement =
      ModifyInstancePlacementResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyInstancePlacementResponse'
            Prelude.<$> (x Data..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyInstancePlacement where
  hashWithSalt _salt ModifyInstancePlacement' {..} =
    _salt
      `Prelude.hashWithSalt` affinity
      `Prelude.hashWithSalt` groupId
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` hostId
      `Prelude.hashWithSalt` hostResourceGroupArn
      `Prelude.hashWithSalt` partitionNumber
      `Prelude.hashWithSalt` tenancy
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData ModifyInstancePlacement where
  rnf ModifyInstancePlacement' {..} =
    Prelude.rnf affinity
      `Prelude.seq` Prelude.rnf groupId
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf hostId
      `Prelude.seq` Prelude.rnf hostResourceGroupArn
      `Prelude.seq` Prelude.rnf partitionNumber
      `Prelude.seq` Prelude.rnf tenancy
      `Prelude.seq` Prelude.rnf instanceId

instance Data.ToHeaders ModifyInstancePlacement where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyInstancePlacement where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyInstancePlacement where
  toQuery ModifyInstancePlacement' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyInstancePlacement" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "Affinity" Data.=: affinity,
        "GroupId" Data.=: groupId,
        "GroupName" Data.=: groupName,
        "HostId" Data.=: hostId,
        "HostResourceGroupArn" Data.=: hostResourceGroupArn,
        "PartitionNumber" Data.=: partitionNumber,
        "Tenancy" Data.=: tenancy,
        "InstanceId" Data.=: instanceId
      ]

-- | /See:/ 'newModifyInstancePlacementResponse' smart constructor.
data ModifyInstancePlacementResponse = ModifyInstancePlacementResponse'
  { -- | Is @true@ if the request succeeds, and an error otherwise.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyInstancePlacementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'modifyInstancePlacementResponse_return' - Is @true@ if the request succeeds, and an error otherwise.
--
-- 'httpStatus', 'modifyInstancePlacementResponse_httpStatus' - The response's http status code.
newModifyInstancePlacementResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyInstancePlacementResponse
newModifyInstancePlacementResponse pHttpStatus_ =
  ModifyInstancePlacementResponse'
    { return' =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Is @true@ if the request succeeds, and an error otherwise.
modifyInstancePlacementResponse_return :: Lens.Lens' ModifyInstancePlacementResponse (Prelude.Maybe Prelude.Bool)
modifyInstancePlacementResponse_return = Lens.lens (\ModifyInstancePlacementResponse' {return'} -> return') (\s@ModifyInstancePlacementResponse' {} a -> s {return' = a} :: ModifyInstancePlacementResponse)

-- | The response's http status code.
modifyInstancePlacementResponse_httpStatus :: Lens.Lens' ModifyInstancePlacementResponse Prelude.Int
modifyInstancePlacementResponse_httpStatus = Lens.lens (\ModifyInstancePlacementResponse' {httpStatus} -> httpStatus) (\s@ModifyInstancePlacementResponse' {} a -> s {httpStatus = a} :: ModifyInstancePlacementResponse)

instance
  Prelude.NFData
    ModifyInstancePlacementResponse
  where
  rnf ModifyInstancePlacementResponse' {..} =
    Prelude.rnf return'
      `Prelude.seq` Prelude.rnf httpStatus
