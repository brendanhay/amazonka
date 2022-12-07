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
-- Module      : Amazonka.EC2.ModifyHosts
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.EC2.ModifyHosts
  ( -- * Creating a Request
    ModifyHosts (..),
    newModifyHosts,

    -- * Request Lenses
    modifyHosts_autoPlacement,
    modifyHosts_hostRecovery,
    modifyHosts_instanceType,
    modifyHosts_instanceFamily,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyHosts' smart constructor.
data ModifyHosts = ModifyHosts'
  { -- | Specify whether to enable or disable auto-placement.
    autoPlacement :: Prelude.Maybe AutoPlacement,
    -- | Indicates whether to enable or disable host recovery for the Dedicated
    -- Host. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-hosts-recovery.html Host recovery>
    -- in the /Amazon EC2 User Guide/.
    hostRecovery :: Prelude.Maybe HostRecovery,
    -- | Specifies the instance type to be supported by the Dedicated Host.
    -- Specify this parameter to modify a Dedicated Host to support only a
    -- specific instance type.
    --
    -- If you want to modify a Dedicated Host to support multiple instance
    -- types in its current instance family, omit this parameter and specify
    -- __InstanceFamily__ instead. You cannot specify __InstanceType__ and
    -- __InstanceFamily__ in the same request.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | Specifies the instance family to be supported by the Dedicated Host.
    -- Specify this parameter to modify a Dedicated Host to support multiple
    -- instance types within its current instance family.
    --
    -- If you want to modify a Dedicated Host to support a specific instance
    -- type only, omit this parameter and specify __InstanceType__ instead. You
    -- cannot specify __InstanceFamily__ and __InstanceType__ in the same
    -- request.
    instanceFamily :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the Dedicated Hosts to modify.
    hostIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyHosts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoPlacement', 'modifyHosts_autoPlacement' - Specify whether to enable or disable auto-placement.
--
-- 'hostRecovery', 'modifyHosts_hostRecovery' - Indicates whether to enable or disable host recovery for the Dedicated
-- Host. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-hosts-recovery.html Host recovery>
-- in the /Amazon EC2 User Guide/.
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
-- 'instanceFamily', 'modifyHosts_instanceFamily' - Specifies the instance family to be supported by the Dedicated Host.
-- Specify this parameter to modify a Dedicated Host to support multiple
-- instance types within its current instance family.
--
-- If you want to modify a Dedicated Host to support a specific instance
-- type only, omit this parameter and specify __InstanceType__ instead. You
-- cannot specify __InstanceFamily__ and __InstanceType__ in the same
-- request.
--
-- 'hostIds', 'modifyHosts_hostIds' - The IDs of the Dedicated Hosts to modify.
newModifyHosts ::
  ModifyHosts
newModifyHosts =
  ModifyHosts'
    { autoPlacement = Prelude.Nothing,
      hostRecovery = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      instanceFamily = Prelude.Nothing,
      hostIds = Prelude.mempty
    }

-- | Specify whether to enable or disable auto-placement.
modifyHosts_autoPlacement :: Lens.Lens' ModifyHosts (Prelude.Maybe AutoPlacement)
modifyHosts_autoPlacement = Lens.lens (\ModifyHosts' {autoPlacement} -> autoPlacement) (\s@ModifyHosts' {} a -> s {autoPlacement = a} :: ModifyHosts)

-- | Indicates whether to enable or disable host recovery for the Dedicated
-- Host. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-hosts-recovery.html Host recovery>
-- in the /Amazon EC2 User Guide/.
modifyHosts_hostRecovery :: Lens.Lens' ModifyHosts (Prelude.Maybe HostRecovery)
modifyHosts_hostRecovery = Lens.lens (\ModifyHosts' {hostRecovery} -> hostRecovery) (\s@ModifyHosts' {} a -> s {hostRecovery = a} :: ModifyHosts)

-- | Specifies the instance type to be supported by the Dedicated Host.
-- Specify this parameter to modify a Dedicated Host to support only a
-- specific instance type.
--
-- If you want to modify a Dedicated Host to support multiple instance
-- types in its current instance family, omit this parameter and specify
-- __InstanceFamily__ instead. You cannot specify __InstanceType__ and
-- __InstanceFamily__ in the same request.
modifyHosts_instanceType :: Lens.Lens' ModifyHosts (Prelude.Maybe Prelude.Text)
modifyHosts_instanceType = Lens.lens (\ModifyHosts' {instanceType} -> instanceType) (\s@ModifyHosts' {} a -> s {instanceType = a} :: ModifyHosts)

-- | Specifies the instance family to be supported by the Dedicated Host.
-- Specify this parameter to modify a Dedicated Host to support multiple
-- instance types within its current instance family.
--
-- If you want to modify a Dedicated Host to support a specific instance
-- type only, omit this parameter and specify __InstanceType__ instead. You
-- cannot specify __InstanceFamily__ and __InstanceType__ in the same
-- request.
modifyHosts_instanceFamily :: Lens.Lens' ModifyHosts (Prelude.Maybe Prelude.Text)
modifyHosts_instanceFamily = Lens.lens (\ModifyHosts' {instanceFamily} -> instanceFamily) (\s@ModifyHosts' {} a -> s {instanceFamily = a} :: ModifyHosts)

-- | The IDs of the Dedicated Hosts to modify.
modifyHosts_hostIds :: Lens.Lens' ModifyHosts [Prelude.Text]
modifyHosts_hostIds = Lens.lens (\ModifyHosts' {hostIds} -> hostIds) (\s@ModifyHosts' {} a -> s {hostIds = a} :: ModifyHosts) Prelude.. Lens.coerced

instance Core.AWSRequest ModifyHosts where
  type AWSResponse ModifyHosts = ModifyHostsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyHostsResponse'
            Prelude.<$> ( x Data..@? "unsuccessful" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> ( x Data..@? "successful" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyHosts where
  hashWithSalt _salt ModifyHosts' {..} =
    _salt `Prelude.hashWithSalt` autoPlacement
      `Prelude.hashWithSalt` hostRecovery
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` instanceFamily
      `Prelude.hashWithSalt` hostIds

instance Prelude.NFData ModifyHosts where
  rnf ModifyHosts' {..} =
    Prelude.rnf autoPlacement
      `Prelude.seq` Prelude.rnf hostRecovery
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf instanceFamily
      `Prelude.seq` Prelude.rnf hostIds

instance Data.ToHeaders ModifyHosts where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyHosts where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyHosts where
  toQuery ModifyHosts' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyHosts" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "AutoPlacement" Data.=: autoPlacement,
        "HostRecovery" Data.=: hostRecovery,
        "InstanceType" Data.=: instanceType,
        "InstanceFamily" Data.=: instanceFamily,
        Data.toQueryList "HostId" hostIds
      ]

-- | /See:/ 'newModifyHostsResponse' smart constructor.
data ModifyHostsResponse = ModifyHostsResponse'
  { -- | The IDs of the Dedicated Hosts that could not be modified. Check whether
    -- the setting you requested can be used.
    unsuccessful :: Prelude.Maybe [UnsuccessfulItem],
    -- | The IDs of the Dedicated Hosts that were successfully modified.
    successful :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ModifyHostsResponse
newModifyHostsResponse pHttpStatus_ =
  ModifyHostsResponse'
    { unsuccessful =
        Prelude.Nothing,
      successful = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The IDs of the Dedicated Hosts that could not be modified. Check whether
-- the setting you requested can be used.
modifyHostsResponse_unsuccessful :: Lens.Lens' ModifyHostsResponse (Prelude.Maybe [UnsuccessfulItem])
modifyHostsResponse_unsuccessful = Lens.lens (\ModifyHostsResponse' {unsuccessful} -> unsuccessful) (\s@ModifyHostsResponse' {} a -> s {unsuccessful = a} :: ModifyHostsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The IDs of the Dedicated Hosts that were successfully modified.
modifyHostsResponse_successful :: Lens.Lens' ModifyHostsResponse (Prelude.Maybe [Prelude.Text])
modifyHostsResponse_successful = Lens.lens (\ModifyHostsResponse' {successful} -> successful) (\s@ModifyHostsResponse' {} a -> s {successful = a} :: ModifyHostsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
modifyHostsResponse_httpStatus :: Lens.Lens' ModifyHostsResponse Prelude.Int
modifyHostsResponse_httpStatus = Lens.lens (\ModifyHostsResponse' {httpStatus} -> httpStatus) (\s@ModifyHostsResponse' {} a -> s {httpStatus = a} :: ModifyHostsResponse)

instance Prelude.NFData ModifyHostsResponse where
  rnf ModifyHostsResponse' {..} =
    Prelude.rnf unsuccessful
      `Prelude.seq` Prelude.rnf successful
      `Prelude.seq` Prelude.rnf httpStatus
