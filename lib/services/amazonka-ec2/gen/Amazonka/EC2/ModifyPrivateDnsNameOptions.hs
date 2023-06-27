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
-- Module      : Amazonka.EC2.ModifyPrivateDnsNameOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the options for instance hostnames for the specified instance.
module Amazonka.EC2.ModifyPrivateDnsNameOptions
  ( -- * Creating a Request
    ModifyPrivateDnsNameOptions (..),
    newModifyPrivateDnsNameOptions,

    -- * Request Lenses
    modifyPrivateDnsNameOptions_dryRun,
    modifyPrivateDnsNameOptions_enableResourceNameDnsAAAARecord,
    modifyPrivateDnsNameOptions_enableResourceNameDnsARecord,
    modifyPrivateDnsNameOptions_privateDnsHostnameType,
    modifyPrivateDnsNameOptions_instanceId,

    -- * Destructuring the Response
    ModifyPrivateDnsNameOptionsResponse (..),
    newModifyPrivateDnsNameOptionsResponse,

    -- * Response Lenses
    modifyPrivateDnsNameOptionsResponse_return,
    modifyPrivateDnsNameOptionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyPrivateDnsNameOptions' smart constructor.
data ModifyPrivateDnsNameOptions = ModifyPrivateDnsNameOptions'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether to respond to DNS queries for instance hostnames with
    -- DNS AAAA records.
    enableResourceNameDnsAAAARecord :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether to respond to DNS queries for instance hostnames with
    -- DNS A records.
    enableResourceNameDnsARecord :: Prelude.Maybe Prelude.Bool,
    -- | The type of hostname for EC2 instances. For IPv4 only subnets, an
    -- instance DNS name must be based on the instance IPv4 address. For IPv6
    -- only subnets, an instance DNS name must be based on the instance ID. For
    -- dual-stack subnets, you can specify whether DNS names use the instance
    -- IPv4 address or the instance ID.
    privateDnsHostnameType :: Prelude.Maybe HostnameType,
    -- | The ID of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyPrivateDnsNameOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'modifyPrivateDnsNameOptions_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'enableResourceNameDnsAAAARecord', 'modifyPrivateDnsNameOptions_enableResourceNameDnsAAAARecord' - Indicates whether to respond to DNS queries for instance hostnames with
-- DNS AAAA records.
--
-- 'enableResourceNameDnsARecord', 'modifyPrivateDnsNameOptions_enableResourceNameDnsARecord' - Indicates whether to respond to DNS queries for instance hostnames with
-- DNS A records.
--
-- 'privateDnsHostnameType', 'modifyPrivateDnsNameOptions_privateDnsHostnameType' - The type of hostname for EC2 instances. For IPv4 only subnets, an
-- instance DNS name must be based on the instance IPv4 address. For IPv6
-- only subnets, an instance DNS name must be based on the instance ID. For
-- dual-stack subnets, you can specify whether DNS names use the instance
-- IPv4 address or the instance ID.
--
-- 'instanceId', 'modifyPrivateDnsNameOptions_instanceId' - The ID of the instance.
newModifyPrivateDnsNameOptions ::
  -- | 'instanceId'
  Prelude.Text ->
  ModifyPrivateDnsNameOptions
newModifyPrivateDnsNameOptions pInstanceId_ =
  ModifyPrivateDnsNameOptions'
    { dryRun =
        Prelude.Nothing,
      enableResourceNameDnsAAAARecord =
        Prelude.Nothing,
      enableResourceNameDnsARecord = Prelude.Nothing,
      privateDnsHostnameType = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyPrivateDnsNameOptions_dryRun :: Lens.Lens' ModifyPrivateDnsNameOptions (Prelude.Maybe Prelude.Bool)
modifyPrivateDnsNameOptions_dryRun = Lens.lens (\ModifyPrivateDnsNameOptions' {dryRun} -> dryRun) (\s@ModifyPrivateDnsNameOptions' {} a -> s {dryRun = a} :: ModifyPrivateDnsNameOptions)

-- | Indicates whether to respond to DNS queries for instance hostnames with
-- DNS AAAA records.
modifyPrivateDnsNameOptions_enableResourceNameDnsAAAARecord :: Lens.Lens' ModifyPrivateDnsNameOptions (Prelude.Maybe Prelude.Bool)
modifyPrivateDnsNameOptions_enableResourceNameDnsAAAARecord = Lens.lens (\ModifyPrivateDnsNameOptions' {enableResourceNameDnsAAAARecord} -> enableResourceNameDnsAAAARecord) (\s@ModifyPrivateDnsNameOptions' {} a -> s {enableResourceNameDnsAAAARecord = a} :: ModifyPrivateDnsNameOptions)

-- | Indicates whether to respond to DNS queries for instance hostnames with
-- DNS A records.
modifyPrivateDnsNameOptions_enableResourceNameDnsARecord :: Lens.Lens' ModifyPrivateDnsNameOptions (Prelude.Maybe Prelude.Bool)
modifyPrivateDnsNameOptions_enableResourceNameDnsARecord = Lens.lens (\ModifyPrivateDnsNameOptions' {enableResourceNameDnsARecord} -> enableResourceNameDnsARecord) (\s@ModifyPrivateDnsNameOptions' {} a -> s {enableResourceNameDnsARecord = a} :: ModifyPrivateDnsNameOptions)

-- | The type of hostname for EC2 instances. For IPv4 only subnets, an
-- instance DNS name must be based on the instance IPv4 address. For IPv6
-- only subnets, an instance DNS name must be based on the instance ID. For
-- dual-stack subnets, you can specify whether DNS names use the instance
-- IPv4 address or the instance ID.
modifyPrivateDnsNameOptions_privateDnsHostnameType :: Lens.Lens' ModifyPrivateDnsNameOptions (Prelude.Maybe HostnameType)
modifyPrivateDnsNameOptions_privateDnsHostnameType = Lens.lens (\ModifyPrivateDnsNameOptions' {privateDnsHostnameType} -> privateDnsHostnameType) (\s@ModifyPrivateDnsNameOptions' {} a -> s {privateDnsHostnameType = a} :: ModifyPrivateDnsNameOptions)

-- | The ID of the instance.
modifyPrivateDnsNameOptions_instanceId :: Lens.Lens' ModifyPrivateDnsNameOptions Prelude.Text
modifyPrivateDnsNameOptions_instanceId = Lens.lens (\ModifyPrivateDnsNameOptions' {instanceId} -> instanceId) (\s@ModifyPrivateDnsNameOptions' {} a -> s {instanceId = a} :: ModifyPrivateDnsNameOptions)

instance Core.AWSRequest ModifyPrivateDnsNameOptions where
  type
    AWSResponse ModifyPrivateDnsNameOptions =
      ModifyPrivateDnsNameOptionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyPrivateDnsNameOptionsResponse'
            Prelude.<$> (x Data..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyPrivateDnsNameOptions where
  hashWithSalt _salt ModifyPrivateDnsNameOptions' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` enableResourceNameDnsAAAARecord
      `Prelude.hashWithSalt` enableResourceNameDnsARecord
      `Prelude.hashWithSalt` privateDnsHostnameType
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData ModifyPrivateDnsNameOptions where
  rnf ModifyPrivateDnsNameOptions' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf enableResourceNameDnsAAAARecord
      `Prelude.seq` Prelude.rnf enableResourceNameDnsARecord
      `Prelude.seq` Prelude.rnf privateDnsHostnameType
      `Prelude.seq` Prelude.rnf instanceId

instance Data.ToHeaders ModifyPrivateDnsNameOptions where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyPrivateDnsNameOptions where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyPrivateDnsNameOptions where
  toQuery ModifyPrivateDnsNameOptions' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "ModifyPrivateDnsNameOptions" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "EnableResourceNameDnsAAAARecord"
          Data.=: enableResourceNameDnsAAAARecord,
        "EnableResourceNameDnsARecord"
          Data.=: enableResourceNameDnsARecord,
        "PrivateDnsHostnameType"
          Data.=: privateDnsHostnameType,
        "InstanceId" Data.=: instanceId
      ]

-- | /See:/ 'newModifyPrivateDnsNameOptionsResponse' smart constructor.
data ModifyPrivateDnsNameOptionsResponse = ModifyPrivateDnsNameOptionsResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyPrivateDnsNameOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'modifyPrivateDnsNameOptionsResponse_return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- 'httpStatus', 'modifyPrivateDnsNameOptionsResponse_httpStatus' - The response's http status code.
newModifyPrivateDnsNameOptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyPrivateDnsNameOptionsResponse
newModifyPrivateDnsNameOptionsResponse pHttpStatus_ =
  ModifyPrivateDnsNameOptionsResponse'
    { return' =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
modifyPrivateDnsNameOptionsResponse_return :: Lens.Lens' ModifyPrivateDnsNameOptionsResponse (Prelude.Maybe Prelude.Bool)
modifyPrivateDnsNameOptionsResponse_return = Lens.lens (\ModifyPrivateDnsNameOptionsResponse' {return'} -> return') (\s@ModifyPrivateDnsNameOptionsResponse' {} a -> s {return' = a} :: ModifyPrivateDnsNameOptionsResponse)

-- | The response's http status code.
modifyPrivateDnsNameOptionsResponse_httpStatus :: Lens.Lens' ModifyPrivateDnsNameOptionsResponse Prelude.Int
modifyPrivateDnsNameOptionsResponse_httpStatus = Lens.lens (\ModifyPrivateDnsNameOptionsResponse' {httpStatus} -> httpStatus) (\s@ModifyPrivateDnsNameOptionsResponse' {} a -> s {httpStatus = a} :: ModifyPrivateDnsNameOptionsResponse)

instance
  Prelude.NFData
    ModifyPrivateDnsNameOptionsResponse
  where
  rnf ModifyPrivateDnsNameOptionsResponse' {..} =
    Prelude.rnf return'
      `Prelude.seq` Prelude.rnf httpStatus
