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
-- Module      : Network.AWS.EC2.ModifyInstancePlacement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
-- -   Change the instance tenancy of an instance from @host@ to
--     @dedicated@, or from @dedicated@ to @host@.
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
module Network.AWS.EC2.ModifyInstancePlacement
  ( -- * Creating a Request
    ModifyInstancePlacement (..),
    newModifyInstancePlacement,

    -- * Request Lenses
    modifyInstancePlacement_groupName,
    modifyInstancePlacement_tenancy,
    modifyInstancePlacement_affinity,
    modifyInstancePlacement_partitionNumber,
    modifyInstancePlacement_hostResourceGroupArn,
    modifyInstancePlacement_hostId,
    modifyInstancePlacement_instanceId,

    -- * Destructuring the Response
    ModifyInstancePlacementResponse (..),
    newModifyInstancePlacementResponse,

    -- * Response Lenses
    modifyInstancePlacementResponse_return,
    modifyInstancePlacementResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyInstancePlacement' smart constructor.
data ModifyInstancePlacement = ModifyInstancePlacement'
  { -- | The name of the placement group in which to place the instance. For
    -- spread placement groups, the instance must have a tenancy of @default@.
    -- For cluster and partition placement groups, the instance must have a
    -- tenancy of @default@ or @dedicated@.
    --
    -- To remove an instance from a placement group, specify an empty string
    -- (\"\").
    groupName :: Core.Maybe Core.Text,
    -- | The tenancy for the instance.
    tenancy :: Core.Maybe HostTenancy,
    -- | The affinity setting for the instance.
    affinity :: Core.Maybe Affinity,
    -- | Reserved for future use.
    partitionNumber :: Core.Maybe Core.Int,
    -- | The ARN of the host resource group in which to place the instance.
    hostResourceGroupArn :: Core.Maybe Core.Text,
    -- | The ID of the Dedicated Host with which to associate the instance.
    hostId :: Core.Maybe Core.Text,
    -- | The ID of the instance that you are modifying.
    instanceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyInstancePlacement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'modifyInstancePlacement_groupName' - The name of the placement group in which to place the instance. For
-- spread placement groups, the instance must have a tenancy of @default@.
-- For cluster and partition placement groups, the instance must have a
-- tenancy of @default@ or @dedicated@.
--
-- To remove an instance from a placement group, specify an empty string
-- (\"\").
--
-- 'tenancy', 'modifyInstancePlacement_tenancy' - The tenancy for the instance.
--
-- 'affinity', 'modifyInstancePlacement_affinity' - The affinity setting for the instance.
--
-- 'partitionNumber', 'modifyInstancePlacement_partitionNumber' - Reserved for future use.
--
-- 'hostResourceGroupArn', 'modifyInstancePlacement_hostResourceGroupArn' - The ARN of the host resource group in which to place the instance.
--
-- 'hostId', 'modifyInstancePlacement_hostId' - The ID of the Dedicated Host with which to associate the instance.
--
-- 'instanceId', 'modifyInstancePlacement_instanceId' - The ID of the instance that you are modifying.
newModifyInstancePlacement ::
  -- | 'instanceId'
  Core.Text ->
  ModifyInstancePlacement
newModifyInstancePlacement pInstanceId_ =
  ModifyInstancePlacement'
    { groupName = Core.Nothing,
      tenancy = Core.Nothing,
      affinity = Core.Nothing,
      partitionNumber = Core.Nothing,
      hostResourceGroupArn = Core.Nothing,
      hostId = Core.Nothing,
      instanceId = pInstanceId_
    }

-- | The name of the placement group in which to place the instance. For
-- spread placement groups, the instance must have a tenancy of @default@.
-- For cluster and partition placement groups, the instance must have a
-- tenancy of @default@ or @dedicated@.
--
-- To remove an instance from a placement group, specify an empty string
-- (\"\").
modifyInstancePlacement_groupName :: Lens.Lens' ModifyInstancePlacement (Core.Maybe Core.Text)
modifyInstancePlacement_groupName = Lens.lens (\ModifyInstancePlacement' {groupName} -> groupName) (\s@ModifyInstancePlacement' {} a -> s {groupName = a} :: ModifyInstancePlacement)

-- | The tenancy for the instance.
modifyInstancePlacement_tenancy :: Lens.Lens' ModifyInstancePlacement (Core.Maybe HostTenancy)
modifyInstancePlacement_tenancy = Lens.lens (\ModifyInstancePlacement' {tenancy} -> tenancy) (\s@ModifyInstancePlacement' {} a -> s {tenancy = a} :: ModifyInstancePlacement)

-- | The affinity setting for the instance.
modifyInstancePlacement_affinity :: Lens.Lens' ModifyInstancePlacement (Core.Maybe Affinity)
modifyInstancePlacement_affinity = Lens.lens (\ModifyInstancePlacement' {affinity} -> affinity) (\s@ModifyInstancePlacement' {} a -> s {affinity = a} :: ModifyInstancePlacement)

-- | Reserved for future use.
modifyInstancePlacement_partitionNumber :: Lens.Lens' ModifyInstancePlacement (Core.Maybe Core.Int)
modifyInstancePlacement_partitionNumber = Lens.lens (\ModifyInstancePlacement' {partitionNumber} -> partitionNumber) (\s@ModifyInstancePlacement' {} a -> s {partitionNumber = a} :: ModifyInstancePlacement)

-- | The ARN of the host resource group in which to place the instance.
modifyInstancePlacement_hostResourceGroupArn :: Lens.Lens' ModifyInstancePlacement (Core.Maybe Core.Text)
modifyInstancePlacement_hostResourceGroupArn = Lens.lens (\ModifyInstancePlacement' {hostResourceGroupArn} -> hostResourceGroupArn) (\s@ModifyInstancePlacement' {} a -> s {hostResourceGroupArn = a} :: ModifyInstancePlacement)

-- | The ID of the Dedicated Host with which to associate the instance.
modifyInstancePlacement_hostId :: Lens.Lens' ModifyInstancePlacement (Core.Maybe Core.Text)
modifyInstancePlacement_hostId = Lens.lens (\ModifyInstancePlacement' {hostId} -> hostId) (\s@ModifyInstancePlacement' {} a -> s {hostId = a} :: ModifyInstancePlacement)

-- | The ID of the instance that you are modifying.
modifyInstancePlacement_instanceId :: Lens.Lens' ModifyInstancePlacement Core.Text
modifyInstancePlacement_instanceId = Lens.lens (\ModifyInstancePlacement' {instanceId} -> instanceId) (\s@ModifyInstancePlacement' {} a -> s {instanceId = a} :: ModifyInstancePlacement)

instance Core.AWSRequest ModifyInstancePlacement where
  type
    AWSResponse ModifyInstancePlacement =
      ModifyInstancePlacementResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyInstancePlacementResponse'
            Core.<$> (x Core..@? "return")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyInstancePlacement

instance Core.NFData ModifyInstancePlacement

instance Core.ToHeaders ModifyInstancePlacement where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ModifyInstancePlacement where
  toPath = Core.const "/"

instance Core.ToQuery ModifyInstancePlacement where
  toQuery ModifyInstancePlacement' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ModifyInstancePlacement" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "GroupName" Core.=: groupName,
        "Tenancy" Core.=: tenancy,
        "Affinity" Core.=: affinity,
        "PartitionNumber" Core.=: partitionNumber,
        "HostResourceGroupArn" Core.=: hostResourceGroupArn,
        "HostId" Core.=: hostId,
        "InstanceId" Core.=: instanceId
      ]

-- | /See:/ 'newModifyInstancePlacementResponse' smart constructor.
data ModifyInstancePlacementResponse = ModifyInstancePlacementResponse'
  { -- | Is @true@ if the request succeeds, and an error otherwise.
    return' :: Core.Maybe Core.Bool,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ModifyInstancePlacementResponse
newModifyInstancePlacementResponse pHttpStatus_ =
  ModifyInstancePlacementResponse'
    { return' =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Is @true@ if the request succeeds, and an error otherwise.
modifyInstancePlacementResponse_return :: Lens.Lens' ModifyInstancePlacementResponse (Core.Maybe Core.Bool)
modifyInstancePlacementResponse_return = Lens.lens (\ModifyInstancePlacementResponse' {return'} -> return') (\s@ModifyInstancePlacementResponse' {} a -> s {return' = a} :: ModifyInstancePlacementResponse)

-- | The response's http status code.
modifyInstancePlacementResponse_httpStatus :: Lens.Lens' ModifyInstancePlacementResponse Core.Int
modifyInstancePlacementResponse_httpStatus = Lens.lens (\ModifyInstancePlacementResponse' {httpStatus} -> httpStatus) (\s@ModifyInstancePlacementResponse' {} a -> s {httpStatus = a} :: ModifyInstancePlacementResponse)

instance Core.NFData ModifyInstancePlacementResponse
