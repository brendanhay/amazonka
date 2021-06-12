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
-- Module      : Network.AWS.EC2.ModifyHosts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify the auto-placement setting of a Dedicated Host. When
-- auto-placement is enabled, any instances that you launch with a tenancy
-- of @host@ but without a specific host ID are placed onto any available
-- Dedicated Host in your account that has auto-placement enabled. When
-- auto-placement is disabled, you need to provide a host ID to have the
-- instance launch onto a specific host. If no host ID is provided, the
-- instance is launched onto a suitable host with auto-placement enabled.
--
-- You can also use this API action to modify a Dedicated Host to support
-- either multiple instance types in an instance family, or to support a
-- specific instance type only.
module Network.AWS.EC2.ModifyHosts
  ( -- * Creating a Request
    ModifyHosts (..),
    newModifyHosts,

    -- * Request Lenses
    modifyHosts_instanceFamily,
    modifyHosts_instanceType,
    modifyHosts_autoPlacement,
    modifyHosts_hostRecovery,
    modifyHosts_hostIds,

    -- * Destructuring the Response
    ModifyHostsResponse (..),
    newModifyHostsResponse,

    -- * Response Lenses
    modifyHostsResponse_unsuccessful,
    modifyHostsResponse_successful,
    modifyHostsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyHosts' smart constructor.
data ModifyHosts = ModifyHosts'
  { -- | Specifies the instance family to be supported by the Dedicated Host.
    -- Specify this parameter to modify a Dedicated Host to support multiple
    -- instance types within its current instance family.
    --
    -- If you want to modify a Dedicated Host to support a specific instance
    -- type only, omit this parameter and specify __InstanceType__ instead. You
    -- cannot specify __InstanceFamily__ and __InstanceType__ in the same
    -- request.
    instanceFamily :: Core.Maybe Core.Text,
    -- | Specifies the instance type to be supported by the Dedicated Host.
    -- Specify this parameter to modify a Dedicated Host to support only a
    -- specific instance type.
    --
    -- If you want to modify a Dedicated Host to support multiple instance
    -- types in its current instance family, omit this parameter and specify
    -- __InstanceFamily__ instead. You cannot specify __InstanceType__ and
    -- __InstanceFamily__ in the same request.
    instanceType :: Core.Maybe Core.Text,
    -- | Specify whether to enable or disable auto-placement.
    autoPlacement :: Core.Maybe AutoPlacement,
    -- | Indicates whether to enable or disable host recovery for the Dedicated
    -- Host. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-hosts-recovery.html Host recovery>
    -- in the /Amazon EC2 User Guide/.
    hostRecovery :: Core.Maybe HostRecovery,
    -- | The IDs of the Dedicated Hosts to modify.
    hostIds :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyHosts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceFamily', 'modifyHosts_instanceFamily' - Specifies the instance family to be supported by the Dedicated Host.
-- Specify this parameter to modify a Dedicated Host to support multiple
-- instance types within its current instance family.
--
-- If you want to modify a Dedicated Host to support a specific instance
-- type only, omit this parameter and specify __InstanceType__ instead. You
-- cannot specify __InstanceFamily__ and __InstanceType__ in the same
-- request.
--
-- 'instanceType', 'modifyHosts_instanceType' - Specifies the instance type to be supported by the Dedicated Host.
-- Specify this parameter to modify a Dedicated Host to support only a
-- specific instance type.
--
-- If you want to modify a Dedicated Host to support multiple instance
-- types in its current instance family, omit this parameter and specify
-- __InstanceFamily__ instead. You cannot specify __InstanceType__ and
-- __InstanceFamily__ in the same request.
--
-- 'autoPlacement', 'modifyHosts_autoPlacement' - Specify whether to enable or disable auto-placement.
--
-- 'hostRecovery', 'modifyHosts_hostRecovery' - Indicates whether to enable or disable host recovery for the Dedicated
-- Host. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-hosts-recovery.html Host recovery>
-- in the /Amazon EC2 User Guide/.
--
-- 'hostIds', 'modifyHosts_hostIds' - The IDs of the Dedicated Hosts to modify.
newModifyHosts ::
  ModifyHosts
newModifyHosts =
  ModifyHosts'
    { instanceFamily = Core.Nothing,
      instanceType = Core.Nothing,
      autoPlacement = Core.Nothing,
      hostRecovery = Core.Nothing,
      hostIds = Core.mempty
    }

-- | Specifies the instance family to be supported by the Dedicated Host.
-- Specify this parameter to modify a Dedicated Host to support multiple
-- instance types within its current instance family.
--
-- If you want to modify a Dedicated Host to support a specific instance
-- type only, omit this parameter and specify __InstanceType__ instead. You
-- cannot specify __InstanceFamily__ and __InstanceType__ in the same
-- request.
modifyHosts_instanceFamily :: Lens.Lens' ModifyHosts (Core.Maybe Core.Text)
modifyHosts_instanceFamily = Lens.lens (\ModifyHosts' {instanceFamily} -> instanceFamily) (\s@ModifyHosts' {} a -> s {instanceFamily = a} :: ModifyHosts)

-- | Specifies the instance type to be supported by the Dedicated Host.
-- Specify this parameter to modify a Dedicated Host to support only a
-- specific instance type.
--
-- If you want to modify a Dedicated Host to support multiple instance
-- types in its current instance family, omit this parameter and specify
-- __InstanceFamily__ instead. You cannot specify __InstanceType__ and
-- __InstanceFamily__ in the same request.
modifyHosts_instanceType :: Lens.Lens' ModifyHosts (Core.Maybe Core.Text)
modifyHosts_instanceType = Lens.lens (\ModifyHosts' {instanceType} -> instanceType) (\s@ModifyHosts' {} a -> s {instanceType = a} :: ModifyHosts)

-- | Specify whether to enable or disable auto-placement.
modifyHosts_autoPlacement :: Lens.Lens' ModifyHosts (Core.Maybe AutoPlacement)
modifyHosts_autoPlacement = Lens.lens (\ModifyHosts' {autoPlacement} -> autoPlacement) (\s@ModifyHosts' {} a -> s {autoPlacement = a} :: ModifyHosts)

-- | Indicates whether to enable or disable host recovery for the Dedicated
-- Host. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-hosts-recovery.html Host recovery>
-- in the /Amazon EC2 User Guide/.
modifyHosts_hostRecovery :: Lens.Lens' ModifyHosts (Core.Maybe HostRecovery)
modifyHosts_hostRecovery = Lens.lens (\ModifyHosts' {hostRecovery} -> hostRecovery) (\s@ModifyHosts' {} a -> s {hostRecovery = a} :: ModifyHosts)

-- | The IDs of the Dedicated Hosts to modify.
modifyHosts_hostIds :: Lens.Lens' ModifyHosts [Core.Text]
modifyHosts_hostIds = Lens.lens (\ModifyHosts' {hostIds} -> hostIds) (\s@ModifyHosts' {} a -> s {hostIds = a} :: ModifyHosts) Core.. Lens._Coerce

instance Core.AWSRequest ModifyHosts where
  type AWSResponse ModifyHosts = ModifyHostsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyHostsResponse'
            Core.<$> ( x Core..@? "unsuccessful" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> ( x Core..@? "successful" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyHosts

instance Core.NFData ModifyHosts

instance Core.ToHeaders ModifyHosts where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ModifyHosts where
  toPath = Core.const "/"

instance Core.ToQuery ModifyHosts where
  toQuery ModifyHosts' {..} =
    Core.mconcat
      [ "Action" Core.=: ("ModifyHosts" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "InstanceFamily" Core.=: instanceFamily,
        "InstanceType" Core.=: instanceType,
        "AutoPlacement" Core.=: autoPlacement,
        "HostRecovery" Core.=: hostRecovery,
        Core.toQueryList "HostId" hostIds
      ]

-- | /See:/ 'newModifyHostsResponse' smart constructor.
data ModifyHostsResponse = ModifyHostsResponse'
  { -- | The IDs of the Dedicated Hosts that could not be modified. Check whether
    -- the setting you requested can be used.
    unsuccessful :: Core.Maybe [UnsuccessfulItem],
    -- | The IDs of the Dedicated Hosts that were successfully modified.
    successful :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyHostsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unsuccessful', 'modifyHostsResponse_unsuccessful' - The IDs of the Dedicated Hosts that could not be modified. Check whether
-- the setting you requested can be used.
--
-- 'successful', 'modifyHostsResponse_successful' - The IDs of the Dedicated Hosts that were successfully modified.
--
-- 'httpStatus', 'modifyHostsResponse_httpStatus' - The response's http status code.
newModifyHostsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ModifyHostsResponse
newModifyHostsResponse pHttpStatus_ =
  ModifyHostsResponse'
    { unsuccessful = Core.Nothing,
      successful = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The IDs of the Dedicated Hosts that could not be modified. Check whether
-- the setting you requested can be used.
modifyHostsResponse_unsuccessful :: Lens.Lens' ModifyHostsResponse (Core.Maybe [UnsuccessfulItem])
modifyHostsResponse_unsuccessful = Lens.lens (\ModifyHostsResponse' {unsuccessful} -> unsuccessful) (\s@ModifyHostsResponse' {} a -> s {unsuccessful = a} :: ModifyHostsResponse) Core.. Lens.mapping Lens._Coerce

-- | The IDs of the Dedicated Hosts that were successfully modified.
modifyHostsResponse_successful :: Lens.Lens' ModifyHostsResponse (Core.Maybe [Core.Text])
modifyHostsResponse_successful = Lens.lens (\ModifyHostsResponse' {successful} -> successful) (\s@ModifyHostsResponse' {} a -> s {successful = a} :: ModifyHostsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
modifyHostsResponse_httpStatus :: Lens.Lens' ModifyHostsResponse Core.Int
modifyHostsResponse_httpStatus = Lens.lens (\ModifyHostsResponse' {httpStatus} -> httpStatus) (\s@ModifyHostsResponse' {} a -> s {httpStatus = a} :: ModifyHostsResponse)

instance Core.NFData ModifyHostsResponse
