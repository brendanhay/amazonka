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
-- Module      : Network.AWS.DirectoryService.AddIpRoutes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- If the DNS server for your on-premises domain uses a publicly
-- addressable IP address, you must add a CIDR address block to correctly
-- route traffic to and from your Microsoft AD on Amazon Web Services.
-- /AddIpRoutes/ adds this address block. You can also use /AddIpRoutes/ to
-- facilitate routing traffic that uses public IP ranges from your
-- Microsoft AD on AWS to a peer VPC.
--
-- Before you call /AddIpRoutes/, ensure that all of the required
-- permissions have been explicitly granted through a policy. For details
-- about what permissions are required to run the /AddIpRoutes/ operation,
-- see
-- <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/UsingWithDS_IAM_ResourcePermissions.html AWS Directory Service API Permissions: Actions, Resources, and Conditions Reference>.
module Network.AWS.DirectoryService.AddIpRoutes
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

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAddIpRoutes' smart constructor.
data AddIpRoutes = AddIpRoutes'
  { -- | If set to true, updates the inbound and outbound rules of the security
    -- group that has the description: \"AWS created security group for
    -- /directory ID/ directory controllers.\" Following are the new rules:
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
    updateSecurityGroupForDirectoryControllers :: Core.Maybe Core.Bool,
    -- | Identifier (ID) of the directory to which to add the address block.
    directoryId :: Core.Text,
    -- | IP address blocks, using CIDR format, of the traffic to route. This is
    -- often the IP address block of the DNS server used for your on-premises
    -- domain.
    ipRoutes :: [IpRoute]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddIpRoutes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'updateSecurityGroupForDirectoryControllers', 'addIpRoutes_updateSecurityGroupForDirectoryControllers' - If set to true, updates the inbound and outbound rules of the security
-- group that has the description: \"AWS created security group for
-- /directory ID/ directory controllers.\" Following are the new rules:
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
-- often the IP address block of the DNS server used for your on-premises
-- domain.
newAddIpRoutes ::
  -- | 'directoryId'
  Core.Text ->
  AddIpRoutes
newAddIpRoutes pDirectoryId_ =
  AddIpRoutes'
    { updateSecurityGroupForDirectoryControllers =
        Core.Nothing,
      directoryId = pDirectoryId_,
      ipRoutes = Core.mempty
    }

-- | If set to true, updates the inbound and outbound rules of the security
-- group that has the description: \"AWS created security group for
-- /directory ID/ directory controllers.\" Following are the new rules:
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
addIpRoutes_updateSecurityGroupForDirectoryControllers :: Lens.Lens' AddIpRoutes (Core.Maybe Core.Bool)
addIpRoutes_updateSecurityGroupForDirectoryControllers = Lens.lens (\AddIpRoutes' {updateSecurityGroupForDirectoryControllers} -> updateSecurityGroupForDirectoryControllers) (\s@AddIpRoutes' {} a -> s {updateSecurityGroupForDirectoryControllers = a} :: AddIpRoutes)

-- | Identifier (ID) of the directory to which to add the address block.
addIpRoutes_directoryId :: Lens.Lens' AddIpRoutes Core.Text
addIpRoutes_directoryId = Lens.lens (\AddIpRoutes' {directoryId} -> directoryId) (\s@AddIpRoutes' {} a -> s {directoryId = a} :: AddIpRoutes)

-- | IP address blocks, using CIDR format, of the traffic to route. This is
-- often the IP address block of the DNS server used for your on-premises
-- domain.
addIpRoutes_ipRoutes :: Lens.Lens' AddIpRoutes [IpRoute]
addIpRoutes_ipRoutes = Lens.lens (\AddIpRoutes' {ipRoutes} -> ipRoutes) (\s@AddIpRoutes' {} a -> s {ipRoutes = a} :: AddIpRoutes) Core.. Lens._Coerce

instance Core.AWSRequest AddIpRoutes where
  type AWSResponse AddIpRoutes = AddIpRoutesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AddIpRoutesResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AddIpRoutes

instance Core.NFData AddIpRoutes

instance Core.ToHeaders AddIpRoutes where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.AddIpRoutes" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AddIpRoutes where
  toJSON AddIpRoutes' {..} =
    Core.object
      ( Core.catMaybes
          [ ( "UpdateSecurityGroupForDirectoryControllers"
                Core..=
            )
              Core.<$> updateSecurityGroupForDirectoryControllers,
            Core.Just ("DirectoryId" Core..= directoryId),
            Core.Just ("IpRoutes" Core..= ipRoutes)
          ]
      )

instance Core.ToPath AddIpRoutes where
  toPath = Core.const "/"

instance Core.ToQuery AddIpRoutes where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAddIpRoutesResponse' smart constructor.
data AddIpRoutesResponse = AddIpRoutesResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  AddIpRoutesResponse
newAddIpRoutesResponse pHttpStatus_ =
  AddIpRoutesResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
addIpRoutesResponse_httpStatus :: Lens.Lens' AddIpRoutesResponse Core.Int
addIpRoutesResponse_httpStatus = Lens.lens (\AddIpRoutesResponse' {httpStatus} -> httpStatus) (\s@AddIpRoutesResponse' {} a -> s {httpStatus = a} :: AddIpRoutesResponse)

instance Core.NFData AddIpRoutesResponse
