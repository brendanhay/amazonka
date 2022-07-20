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
-- Module      : Amazonka.RDS.CreateCustomAvailabilityZone
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
module Amazonka.RDS.CreateCustomAvailabilityZone
  ( -- * Creating a Request
    CreateCustomAvailabilityZone (..),
    newCreateCustomAvailabilityZone,

    -- * Request Lenses
    createCustomAvailabilityZone_existingVpnId,
    createCustomAvailabilityZone_newVpnTunnelName,
    createCustomAvailabilityZone_vpnTunnelOriginatorIP,
    createCustomAvailabilityZone_customAvailabilityZoneName,

    -- * Destructuring the Response
    CreateCustomAvailabilityZoneResponse (..),
    newCreateCustomAvailabilityZoneResponse,

    -- * Response Lenses
    createCustomAvailabilityZoneResponse_customAvailabilityZone,
    createCustomAvailabilityZoneResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newCreateCustomAvailabilityZone' smart constructor.
data CreateCustomAvailabilityZone = CreateCustomAvailabilityZone'
  { -- | The ID of an existing virtual private network (VPN) between the Amazon
    -- RDS website and the VMware vSphere cluster.
    existingVpnId :: Prelude.Maybe Prelude.Text,
    -- | The name of a new VPN tunnel between the Amazon RDS website and the
    -- VMware vSphere cluster.
    --
    -- Specify this parameter only if @ExistingVpnId@ isn\'t specified.
    newVpnTunnelName' :: Prelude.Maybe Prelude.Text,
    -- | The IP address of network traffic from your on-premises data center. A
    -- custom AZ receives the network traffic.
    --
    -- Specify this parameter only if @ExistingVpnId@ isn\'t specified.
    vpnTunnelOriginatorIP :: Prelude.Maybe Prelude.Text,
    -- | The name of the custom Availability Zone (AZ).
    customAvailabilityZoneName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCustomAvailabilityZone' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'existingVpnId', 'createCustomAvailabilityZone_existingVpnId' - The ID of an existing virtual private network (VPN) between the Amazon
-- RDS website and the VMware vSphere cluster.
--
-- 'newVpnTunnelName'', 'createCustomAvailabilityZone_newVpnTunnelName' - The name of a new VPN tunnel between the Amazon RDS website and the
-- VMware vSphere cluster.
--
-- Specify this parameter only if @ExistingVpnId@ isn\'t specified.
--
-- 'vpnTunnelOriginatorIP', 'createCustomAvailabilityZone_vpnTunnelOriginatorIP' - The IP address of network traffic from your on-premises data center. A
-- custom AZ receives the network traffic.
--
-- Specify this parameter only if @ExistingVpnId@ isn\'t specified.
--
-- 'customAvailabilityZoneName', 'createCustomAvailabilityZone_customAvailabilityZoneName' - The name of the custom Availability Zone (AZ).
newCreateCustomAvailabilityZone ::
  -- | 'customAvailabilityZoneName'
  Prelude.Text ->
  CreateCustomAvailabilityZone
newCreateCustomAvailabilityZone
  pCustomAvailabilityZoneName_ =
    CreateCustomAvailabilityZone'
      { existingVpnId =
          Prelude.Nothing,
        newVpnTunnelName' = Prelude.Nothing,
        vpnTunnelOriginatorIP = Prelude.Nothing,
        customAvailabilityZoneName =
          pCustomAvailabilityZoneName_
      }

-- | The ID of an existing virtual private network (VPN) between the Amazon
-- RDS website and the VMware vSphere cluster.
createCustomAvailabilityZone_existingVpnId :: Lens.Lens' CreateCustomAvailabilityZone (Prelude.Maybe Prelude.Text)
createCustomAvailabilityZone_existingVpnId = Lens.lens (\CreateCustomAvailabilityZone' {existingVpnId} -> existingVpnId) (\s@CreateCustomAvailabilityZone' {} a -> s {existingVpnId = a} :: CreateCustomAvailabilityZone)

-- | The name of a new VPN tunnel between the Amazon RDS website and the
-- VMware vSphere cluster.
--
-- Specify this parameter only if @ExistingVpnId@ isn\'t specified.
createCustomAvailabilityZone_newVpnTunnelName :: Lens.Lens' CreateCustomAvailabilityZone (Prelude.Maybe Prelude.Text)
createCustomAvailabilityZone_newVpnTunnelName = Lens.lens (\CreateCustomAvailabilityZone' {newVpnTunnelName'} -> newVpnTunnelName') (\s@CreateCustomAvailabilityZone' {} a -> s {newVpnTunnelName' = a} :: CreateCustomAvailabilityZone)

-- | The IP address of network traffic from your on-premises data center. A
-- custom AZ receives the network traffic.
--
-- Specify this parameter only if @ExistingVpnId@ isn\'t specified.
createCustomAvailabilityZone_vpnTunnelOriginatorIP :: Lens.Lens' CreateCustomAvailabilityZone (Prelude.Maybe Prelude.Text)
createCustomAvailabilityZone_vpnTunnelOriginatorIP = Lens.lens (\CreateCustomAvailabilityZone' {vpnTunnelOriginatorIP} -> vpnTunnelOriginatorIP) (\s@CreateCustomAvailabilityZone' {} a -> s {vpnTunnelOriginatorIP = a} :: CreateCustomAvailabilityZone)

-- | The name of the custom Availability Zone (AZ).
createCustomAvailabilityZone_customAvailabilityZoneName :: Lens.Lens' CreateCustomAvailabilityZone Prelude.Text
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
            Prelude.<$> (x Core..@? "CustomAvailabilityZone")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateCustomAvailabilityZone
  where
  hashWithSalt _salt CreateCustomAvailabilityZone' {..} =
    _salt `Prelude.hashWithSalt` existingVpnId
      `Prelude.hashWithSalt` newVpnTunnelName'
      `Prelude.hashWithSalt` vpnTunnelOriginatorIP
      `Prelude.hashWithSalt` customAvailabilityZoneName

instance Prelude.NFData CreateCustomAvailabilityZone where
  rnf CreateCustomAvailabilityZone' {..} =
    Prelude.rnf existingVpnId
      `Prelude.seq` Prelude.rnf newVpnTunnelName'
      `Prelude.seq` Prelude.rnf vpnTunnelOriginatorIP
      `Prelude.seq` Prelude.rnf customAvailabilityZoneName

instance Core.ToHeaders CreateCustomAvailabilityZone where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateCustomAvailabilityZone where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateCustomAvailabilityZone where
  toQuery CreateCustomAvailabilityZone' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "CreateCustomAvailabilityZone" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "ExistingVpnId" Core.=: existingVpnId,
        "NewVpnTunnelName" Core.=: newVpnTunnelName',
        "VpnTunnelOriginatorIP"
          Core.=: vpnTunnelOriginatorIP,
        "CustomAvailabilityZoneName"
          Core.=: customAvailabilityZoneName
      ]

-- | /See:/ 'newCreateCustomAvailabilityZoneResponse' smart constructor.
data CreateCustomAvailabilityZoneResponse = CreateCustomAvailabilityZoneResponse'
  { customAvailabilityZone :: Prelude.Maybe CustomAvailabilityZone,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateCustomAvailabilityZoneResponse
newCreateCustomAvailabilityZoneResponse pHttpStatus_ =
  CreateCustomAvailabilityZoneResponse'
    { customAvailabilityZone =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createCustomAvailabilityZoneResponse_customAvailabilityZone :: Lens.Lens' CreateCustomAvailabilityZoneResponse (Prelude.Maybe CustomAvailabilityZone)
createCustomAvailabilityZoneResponse_customAvailabilityZone = Lens.lens (\CreateCustomAvailabilityZoneResponse' {customAvailabilityZone} -> customAvailabilityZone) (\s@CreateCustomAvailabilityZoneResponse' {} a -> s {customAvailabilityZone = a} :: CreateCustomAvailabilityZoneResponse)

-- | The response's http status code.
createCustomAvailabilityZoneResponse_httpStatus :: Lens.Lens' CreateCustomAvailabilityZoneResponse Prelude.Int
createCustomAvailabilityZoneResponse_httpStatus = Lens.lens (\CreateCustomAvailabilityZoneResponse' {httpStatus} -> httpStatus) (\s@CreateCustomAvailabilityZoneResponse' {} a -> s {httpStatus = a} :: CreateCustomAvailabilityZoneResponse)

instance
  Prelude.NFData
    CreateCustomAvailabilityZoneResponse
  where
  rnf CreateCustomAvailabilityZoneResponse' {..} =
    Prelude.rnf customAvailabilityZone
      `Prelude.seq` Prelude.rnf httpStatus
