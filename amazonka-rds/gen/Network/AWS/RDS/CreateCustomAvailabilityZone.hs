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
-- Module      : Network.AWS.RDS.CreateCustomAvailabilityZone
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a custom Availability Zone (AZ).
--
-- A custom AZ is an on-premises AZ that is integrated with a VMware
-- vSphere cluster.
--
-- For more information about RDS on VMware, see the
-- <https://docs.aws.amazon.com/AmazonRDS/latest/RDSonVMwareUserGuide/rds-on-vmware.html RDS on VMware User Guide.>
module Network.AWS.RDS.CreateCustomAvailabilityZone
  ( -- * Creating a Request
    CreateCustomAvailabilityZone (..),
    newCreateCustomAvailabilityZone,

    -- * Request Lenses
    createCustomAvailabilityZone_vpnTunnelOriginatorIP,
    createCustomAvailabilityZone_existingVpnId,
    createCustomAvailabilityZone_newVpnTunnelName,
    createCustomAvailabilityZone_customAvailabilityZoneName,

    -- * Destructuring the Response
    CreateCustomAvailabilityZoneResponse (..),
    newCreateCustomAvailabilityZoneResponse,

    -- * Response Lenses
    createCustomAvailabilityZoneResponse_customAvailabilityZone,
    createCustomAvailabilityZoneResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newCreateCustomAvailabilityZone' smart constructor.
data CreateCustomAvailabilityZone = CreateCustomAvailabilityZone'
  { -- | The IP address of network traffic from your on-premises data center. A
    -- custom AZ receives the network traffic.
    --
    -- Specify this parameter only if @ExistingVpnId@ isn\'t specified.
    vpnTunnelOriginatorIP :: Core.Maybe Core.Text,
    -- | The ID of an existing virtual private network (VPN) between the Amazon
    -- RDS website and the VMware vSphere cluster.
    existingVpnId :: Core.Maybe Core.Text,
    -- | The name of a new VPN tunnel between the Amazon RDS website and the
    -- VMware vSphere cluster.
    --
    -- Specify this parameter only if @ExistingVpnId@ isn\'t specified.
    newVpnTunnelName' :: Core.Maybe Core.Text,
    -- | The name of the custom Availability Zone (AZ).
    customAvailabilityZoneName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateCustomAvailabilityZone' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpnTunnelOriginatorIP', 'createCustomAvailabilityZone_vpnTunnelOriginatorIP' - The IP address of network traffic from your on-premises data center. A
-- custom AZ receives the network traffic.
--
-- Specify this parameter only if @ExistingVpnId@ isn\'t specified.
--
-- 'existingVpnId', 'createCustomAvailabilityZone_existingVpnId' - The ID of an existing virtual private network (VPN) between the Amazon
-- RDS website and the VMware vSphere cluster.
--
-- 'newVpnTunnelName'', 'createCustomAvailabilityZone_newVpnTunnelName' - The name of a new VPN tunnel between the Amazon RDS website and the
-- VMware vSphere cluster.
--
-- Specify this parameter only if @ExistingVpnId@ isn\'t specified.
--
-- 'customAvailabilityZoneName', 'createCustomAvailabilityZone_customAvailabilityZoneName' - The name of the custom Availability Zone (AZ).
newCreateCustomAvailabilityZone ::
  -- | 'customAvailabilityZoneName'
  Core.Text ->
  CreateCustomAvailabilityZone
newCreateCustomAvailabilityZone
  pCustomAvailabilityZoneName_ =
    CreateCustomAvailabilityZone'
      { vpnTunnelOriginatorIP =
          Core.Nothing,
        existingVpnId = Core.Nothing,
        newVpnTunnelName' = Core.Nothing,
        customAvailabilityZoneName =
          pCustomAvailabilityZoneName_
      }

-- | The IP address of network traffic from your on-premises data center. A
-- custom AZ receives the network traffic.
--
-- Specify this parameter only if @ExistingVpnId@ isn\'t specified.
createCustomAvailabilityZone_vpnTunnelOriginatorIP :: Lens.Lens' CreateCustomAvailabilityZone (Core.Maybe Core.Text)
createCustomAvailabilityZone_vpnTunnelOriginatorIP = Lens.lens (\CreateCustomAvailabilityZone' {vpnTunnelOriginatorIP} -> vpnTunnelOriginatorIP) (\s@CreateCustomAvailabilityZone' {} a -> s {vpnTunnelOriginatorIP = a} :: CreateCustomAvailabilityZone)

-- | The ID of an existing virtual private network (VPN) between the Amazon
-- RDS website and the VMware vSphere cluster.
createCustomAvailabilityZone_existingVpnId :: Lens.Lens' CreateCustomAvailabilityZone (Core.Maybe Core.Text)
createCustomAvailabilityZone_existingVpnId = Lens.lens (\CreateCustomAvailabilityZone' {existingVpnId} -> existingVpnId) (\s@CreateCustomAvailabilityZone' {} a -> s {existingVpnId = a} :: CreateCustomAvailabilityZone)

-- | The name of a new VPN tunnel between the Amazon RDS website and the
-- VMware vSphere cluster.
--
-- Specify this parameter only if @ExistingVpnId@ isn\'t specified.
createCustomAvailabilityZone_newVpnTunnelName :: Lens.Lens' CreateCustomAvailabilityZone (Core.Maybe Core.Text)
createCustomAvailabilityZone_newVpnTunnelName = Lens.lens (\CreateCustomAvailabilityZone' {newVpnTunnelName'} -> newVpnTunnelName') (\s@CreateCustomAvailabilityZone' {} a -> s {newVpnTunnelName' = a} :: CreateCustomAvailabilityZone)

-- | The name of the custom Availability Zone (AZ).
createCustomAvailabilityZone_customAvailabilityZoneName :: Lens.Lens' CreateCustomAvailabilityZone Core.Text
createCustomAvailabilityZone_customAvailabilityZoneName = Lens.lens (\CreateCustomAvailabilityZone' {customAvailabilityZoneName} -> customAvailabilityZoneName) (\s@CreateCustomAvailabilityZone' {} a -> s {customAvailabilityZoneName = a} :: CreateCustomAvailabilityZone)

instance Core.AWSRequest CreateCustomAvailabilityZone where
  type
    AWSResponse CreateCustomAvailabilityZone =
      CreateCustomAvailabilityZoneResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateCustomAvailabilityZoneResult"
      ( \s h x ->
          CreateCustomAvailabilityZoneResponse'
            Core.<$> (x Core..@? "CustomAvailabilityZone")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateCustomAvailabilityZone

instance Core.NFData CreateCustomAvailabilityZone

instance Core.ToHeaders CreateCustomAvailabilityZone where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateCustomAvailabilityZone where
  toPath = Core.const "/"

instance Core.ToQuery CreateCustomAvailabilityZone where
  toQuery CreateCustomAvailabilityZone' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CreateCustomAvailabilityZone" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "VpnTunnelOriginatorIP"
          Core.=: vpnTunnelOriginatorIP,
        "ExistingVpnId" Core.=: existingVpnId,
        "NewVpnTunnelName" Core.=: newVpnTunnelName',
        "CustomAvailabilityZoneName"
          Core.=: customAvailabilityZoneName
      ]

-- | /See:/ 'newCreateCustomAvailabilityZoneResponse' smart constructor.
data CreateCustomAvailabilityZoneResponse = CreateCustomAvailabilityZoneResponse'
  { customAvailabilityZone :: Core.Maybe CustomAvailabilityZone,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateCustomAvailabilityZoneResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customAvailabilityZone', 'createCustomAvailabilityZoneResponse_customAvailabilityZone' - Undocumented member.
--
-- 'httpStatus', 'createCustomAvailabilityZoneResponse_httpStatus' - The response's http status code.
newCreateCustomAvailabilityZoneResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateCustomAvailabilityZoneResponse
newCreateCustomAvailabilityZoneResponse pHttpStatus_ =
  CreateCustomAvailabilityZoneResponse'
    { customAvailabilityZone =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createCustomAvailabilityZoneResponse_customAvailabilityZone :: Lens.Lens' CreateCustomAvailabilityZoneResponse (Core.Maybe CustomAvailabilityZone)
createCustomAvailabilityZoneResponse_customAvailabilityZone = Lens.lens (\CreateCustomAvailabilityZoneResponse' {customAvailabilityZone} -> customAvailabilityZone) (\s@CreateCustomAvailabilityZoneResponse' {} a -> s {customAvailabilityZone = a} :: CreateCustomAvailabilityZoneResponse)

-- | The response's http status code.
createCustomAvailabilityZoneResponse_httpStatus :: Lens.Lens' CreateCustomAvailabilityZoneResponse Core.Int
createCustomAvailabilityZoneResponse_httpStatus = Lens.lens (\CreateCustomAvailabilityZoneResponse' {httpStatus} -> httpStatus) (\s@CreateCustomAvailabilityZoneResponse' {} a -> s {httpStatus = a} :: CreateCustomAvailabilityZoneResponse)

instance
  Core.NFData
    CreateCustomAvailabilityZoneResponse
