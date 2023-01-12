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
-- Module      : Amazonka.DirectoryService.AddIpRoutes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- If the DNS server for your self-managed domain uses a publicly
-- addressable IP address, you must add a CIDR address block to correctly
-- route traffic to and from your Microsoft AD on Amazon Web Services.
-- /AddIpRoutes/ adds this address block. You can also use /AddIpRoutes/ to
-- facilitate routing traffic that uses public IP ranges from your
-- Microsoft AD on Amazon Web Services to a peer VPC.
--
-- Before you call /AddIpRoutes/, ensure that all of the required
-- permissions have been explicitly granted through a policy. For details
-- about what permissions are required to run the /AddIpRoutes/ operation,
-- see
-- <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/UsingWithDS_IAM_ResourcePermissions.html Directory Service API Permissions: Actions, Resources, and Conditions Reference>.
module Amazonka.DirectoryService.AddIpRoutes
  ( -- * Creating a Request
    AddIpRoutes (..),
    newAddIpRoutes,

    -- * Request Lenses
    addIpRoutes_updateSecurityGroupForDirectoryControllers,
    addIpRoutes_directoryId,
    addIpRoutes_ipRoutes,

    -- * Destructuring the Response
    AddIpRoutesResponse (..),
    newAddIpRoutesResponse,

    -- * Response Lenses
    addIpRoutesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAddIpRoutes' smart constructor.
data AddIpRoutes = AddIpRoutes'
  { -- | If set to true, updates the inbound and outbound rules of the security
    -- group that has the description: \"Amazon Web Services created security
    -- group for /directory ID/ directory controllers.\" Following are the new
    -- rules:
    --
    -- Inbound:
    --
    -- -   Type: Custom UDP Rule, Protocol: UDP, Range: 88, Source: 0.0.0.0\/0
    --
    -- -   Type: Custom UDP Rule, Protocol: UDP, Range: 123, Source: 0.0.0.0\/0
    --
    -- -   Type: Custom UDP Rule, Protocol: UDP, Range: 138, Source: 0.0.0.0\/0
    --
    -- -   Type: Custom UDP Rule, Protocol: UDP, Range: 389, Source: 0.0.0.0\/0
    --
    -- -   Type: Custom UDP Rule, Protocol: UDP, Range: 464, Source: 0.0.0.0\/0
    --
    -- -   Type: Custom UDP Rule, Protocol: UDP, Range: 445, Source: 0.0.0.0\/0
    --
    -- -   Type: Custom TCP Rule, Protocol: TCP, Range: 88, Source: 0.0.0.0\/0
    --
    -- -   Type: Custom TCP Rule, Protocol: TCP, Range: 135, Source: 0.0.0.0\/0
    --
    -- -   Type: Custom TCP Rule, Protocol: TCP, Range: 445, Source: 0.0.0.0\/0
    --
    -- -   Type: Custom TCP Rule, Protocol: TCP, Range: 464, Source: 0.0.0.0\/0
    --
    -- -   Type: Custom TCP Rule, Protocol: TCP, Range: 636, Source: 0.0.0.0\/0
    --
    -- -   Type: Custom TCP Rule, Protocol: TCP, Range: 1024-65535, Source:
    --     0.0.0.0\/0
    --
    -- -   Type: Custom TCP Rule, Protocol: TCP, Range: 3268-33269, Source:
    --     0.0.0.0\/0
    --
    -- -   Type: DNS (UDP), Protocol: UDP, Range: 53, Source: 0.0.0.0\/0
    --
    -- -   Type: DNS (TCP), Protocol: TCP, Range: 53, Source: 0.0.0.0\/0
    --
    -- -   Type: LDAP, Protocol: TCP, Range: 389, Source: 0.0.0.0\/0
    --
    -- -   Type: All ICMP, Protocol: All, Range: N\/A, Source: 0.0.0.0\/0
    --
    -- Outbound:
    --
    -- -   Type: All traffic, Protocol: All, Range: All, Destination:
    --     0.0.0.0\/0
    --
    -- These security rules impact an internal network interface that is not
    -- exposed publicly.
    updateSecurityGroupForDirectoryControllers :: Prelude.Maybe Prelude.Bool,
    -- | Identifier (ID) of the directory to which to add the address block.
    directoryId :: Prelude.Text,
    -- | IP address blocks, using CIDR format, of the traffic to route. This is
    -- often the IP address block of the DNS server used for your self-managed
    -- domain.
    ipRoutes :: [IpRoute]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddIpRoutes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'updateSecurityGroupForDirectoryControllers', 'addIpRoutes_updateSecurityGroupForDirectoryControllers' - If set to true, updates the inbound and outbound rules of the security
-- group that has the description: \"Amazon Web Services created security
-- group for /directory ID/ directory controllers.\" Following are the new
-- rules:
--
-- Inbound:
--
-- -   Type: Custom UDP Rule, Protocol: UDP, Range: 88, Source: 0.0.0.0\/0
--
-- -   Type: Custom UDP Rule, Protocol: UDP, Range: 123, Source: 0.0.0.0\/0
--
-- -   Type: Custom UDP Rule, Protocol: UDP, Range: 138, Source: 0.0.0.0\/0
--
-- -   Type: Custom UDP Rule, Protocol: UDP, Range: 389, Source: 0.0.0.0\/0
--
-- -   Type: Custom UDP Rule, Protocol: UDP, Range: 464, Source: 0.0.0.0\/0
--
-- -   Type: Custom UDP Rule, Protocol: UDP, Range: 445, Source: 0.0.0.0\/0
--
-- -   Type: Custom TCP Rule, Protocol: TCP, Range: 88, Source: 0.0.0.0\/0
--
-- -   Type: Custom TCP Rule, Protocol: TCP, Range: 135, Source: 0.0.0.0\/0
--
-- -   Type: Custom TCP Rule, Protocol: TCP, Range: 445, Source: 0.0.0.0\/0
--
-- -   Type: Custom TCP Rule, Protocol: TCP, Range: 464, Source: 0.0.0.0\/0
--
-- -   Type: Custom TCP Rule, Protocol: TCP, Range: 636, Source: 0.0.0.0\/0
--
-- -   Type: Custom TCP Rule, Protocol: TCP, Range: 1024-65535, Source:
--     0.0.0.0\/0
--
-- -   Type: Custom TCP Rule, Protocol: TCP, Range: 3268-33269, Source:
--     0.0.0.0\/0
--
-- -   Type: DNS (UDP), Protocol: UDP, Range: 53, Source: 0.0.0.0\/0
--
-- -   Type: DNS (TCP), Protocol: TCP, Range: 53, Source: 0.0.0.0\/0
--
-- -   Type: LDAP, Protocol: TCP, Range: 389, Source: 0.0.0.0\/0
--
-- -   Type: All ICMP, Protocol: All, Range: N\/A, Source: 0.0.0.0\/0
--
-- Outbound:
--
-- -   Type: All traffic, Protocol: All, Range: All, Destination:
--     0.0.0.0\/0
--
-- These security rules impact an internal network interface that is not
-- exposed publicly.
--
-- 'directoryId', 'addIpRoutes_directoryId' - Identifier (ID) of the directory to which to add the address block.
--
-- 'ipRoutes', 'addIpRoutes_ipRoutes' - IP address blocks, using CIDR format, of the traffic to route. This is
-- often the IP address block of the DNS server used for your self-managed
-- domain.
newAddIpRoutes ::
  -- | 'directoryId'
  Prelude.Text ->
  AddIpRoutes
newAddIpRoutes pDirectoryId_ =
  AddIpRoutes'
    { updateSecurityGroupForDirectoryControllers =
        Prelude.Nothing,
      directoryId = pDirectoryId_,
      ipRoutes = Prelude.mempty
    }

-- | If set to true, updates the inbound and outbound rules of the security
-- group that has the description: \"Amazon Web Services created security
-- group for /directory ID/ directory controllers.\" Following are the new
-- rules:
--
-- Inbound:
--
-- -   Type: Custom UDP Rule, Protocol: UDP, Range: 88, Source: 0.0.0.0\/0
--
-- -   Type: Custom UDP Rule, Protocol: UDP, Range: 123, Source: 0.0.0.0\/0
--
-- -   Type: Custom UDP Rule, Protocol: UDP, Range: 138, Source: 0.0.0.0\/0
--
-- -   Type: Custom UDP Rule, Protocol: UDP, Range: 389, Source: 0.0.0.0\/0
--
-- -   Type: Custom UDP Rule, Protocol: UDP, Range: 464, Source: 0.0.0.0\/0
--
-- -   Type: Custom UDP Rule, Protocol: UDP, Range: 445, Source: 0.0.0.0\/0
--
-- -   Type: Custom TCP Rule, Protocol: TCP, Range: 88, Source: 0.0.0.0\/0
--
-- -   Type: Custom TCP Rule, Protocol: TCP, Range: 135, Source: 0.0.0.0\/0
--
-- -   Type: Custom TCP Rule, Protocol: TCP, Range: 445, Source: 0.0.0.0\/0
--
-- -   Type: Custom TCP Rule, Protocol: TCP, Range: 464, Source: 0.0.0.0\/0
--
-- -   Type: Custom TCP Rule, Protocol: TCP, Range: 636, Source: 0.0.0.0\/0
--
-- -   Type: Custom TCP Rule, Protocol: TCP, Range: 1024-65535, Source:
--     0.0.0.0\/0
--
-- -   Type: Custom TCP Rule, Protocol: TCP, Range: 3268-33269, Source:
--     0.0.0.0\/0
--
-- -   Type: DNS (UDP), Protocol: UDP, Range: 53, Source: 0.0.0.0\/0
--
-- -   Type: DNS (TCP), Protocol: TCP, Range: 53, Source: 0.0.0.0\/0
--
-- -   Type: LDAP, Protocol: TCP, Range: 389, Source: 0.0.0.0\/0
--
-- -   Type: All ICMP, Protocol: All, Range: N\/A, Source: 0.0.0.0\/0
--
-- Outbound:
--
-- -   Type: All traffic, Protocol: All, Range: All, Destination:
--     0.0.0.0\/0
--
-- These security rules impact an internal network interface that is not
-- exposed publicly.
addIpRoutes_updateSecurityGroupForDirectoryControllers :: Lens.Lens' AddIpRoutes (Prelude.Maybe Prelude.Bool)
addIpRoutes_updateSecurityGroupForDirectoryControllers = Lens.lens (\AddIpRoutes' {updateSecurityGroupForDirectoryControllers} -> updateSecurityGroupForDirectoryControllers) (\s@AddIpRoutes' {} a -> s {updateSecurityGroupForDirectoryControllers = a} :: AddIpRoutes)

-- | Identifier (ID) of the directory to which to add the address block.
addIpRoutes_directoryId :: Lens.Lens' AddIpRoutes Prelude.Text
addIpRoutes_directoryId = Lens.lens (\AddIpRoutes' {directoryId} -> directoryId) (\s@AddIpRoutes' {} a -> s {directoryId = a} :: AddIpRoutes)

-- | IP address blocks, using CIDR format, of the traffic to route. This is
-- often the IP address block of the DNS server used for your self-managed
-- domain.
addIpRoutes_ipRoutes :: Lens.Lens' AddIpRoutes [IpRoute]
addIpRoutes_ipRoutes = Lens.lens (\AddIpRoutes' {ipRoutes} -> ipRoutes) (\s@AddIpRoutes' {} a -> s {ipRoutes = a} :: AddIpRoutes) Prelude.. Lens.coerced

instance Core.AWSRequest AddIpRoutes where
  type AWSResponse AddIpRoutes = AddIpRoutesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AddIpRoutesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddIpRoutes where
  hashWithSalt _salt AddIpRoutes' {..} =
    _salt
      `Prelude.hashWithSalt` updateSecurityGroupForDirectoryControllers
      `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` ipRoutes

instance Prelude.NFData AddIpRoutes where
  rnf AddIpRoutes' {..} =
    Prelude.rnf
      updateSecurityGroupForDirectoryControllers
      `Prelude.seq` Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf ipRoutes

instance Data.ToHeaders AddIpRoutes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DirectoryService_20150416.AddIpRoutes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AddIpRoutes where
  toJSON AddIpRoutes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ( "UpdateSecurityGroupForDirectoryControllers"
                Data..=
            )
              Prelude.<$> updateSecurityGroupForDirectoryControllers,
            Prelude.Just ("DirectoryId" Data..= directoryId),
            Prelude.Just ("IpRoutes" Data..= ipRoutes)
          ]
      )

instance Data.ToPath AddIpRoutes where
  toPath = Prelude.const "/"

instance Data.ToQuery AddIpRoutes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddIpRoutesResponse' smart constructor.
data AddIpRoutesResponse = AddIpRoutesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddIpRoutesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'addIpRoutesResponse_httpStatus' - The response's http status code.
newAddIpRoutesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddIpRoutesResponse
newAddIpRoutesResponse pHttpStatus_ =
  AddIpRoutesResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
addIpRoutesResponse_httpStatus :: Lens.Lens' AddIpRoutesResponse Prelude.Int
addIpRoutesResponse_httpStatus = Lens.lens (\AddIpRoutesResponse' {httpStatus} -> httpStatus) (\s@AddIpRoutesResponse' {} a -> s {httpStatus = a} :: AddIpRoutesResponse)

instance Prelude.NFData AddIpRoutesResponse where
  rnf AddIpRoutesResponse' {..} = Prelude.rnf httpStatus
