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
-- Module      : Amazonka.GameLift.RegisterCompute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers your compute resources in a fleet you previously created.
-- After you register a compute to your fleet, you can monitor and manage
-- your compute using GameLift. The operation returns the compute resource
-- containing SDK endpoint you can use to connect your game server to
-- GameLift.
--
-- __Learn more__
--
-- -   <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-creating-anywhere.html Create an Anywhere fleet>
--
-- -   <https://docs.aws.amazon.com/gamelift/latest/developerguide/integration-testing.html Test your integration>
module Amazonka.GameLift.RegisterCompute
  ( -- * Creating a Request
    RegisterCompute (..),
    newRegisterCompute,

    -- * Request Lenses
    registerCompute_certificatePath,
    registerCompute_dnsName,
    registerCompute_ipAddress,
    registerCompute_location,
    registerCompute_fleetId,
    registerCompute_computeName,

    -- * Destructuring the Response
    RegisterComputeResponse (..),
    newRegisterComputeResponse,

    -- * Response Lenses
    registerComputeResponse_compute,
    registerComputeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRegisterCompute' smart constructor.
data RegisterCompute = RegisterCompute'
  { -- | The path to the TLS certificate on your compute resource. The path and
    -- certificate are not validated by GameLift.
    certificatePath :: Prelude.Maybe Prelude.Text,
    -- | The DNS name of the compute resource. GameLift requires the DNS name or
    -- IP address to manage your compute resource.
    dnsName :: Prelude.Maybe Prelude.Text,
    -- | The IP address of the compute resource. GameLift requires the DNS name
    -- or IP address to manage your compute resource.
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | The name of the custom location you added to the fleet you are
    -- registering this compute resource to.
    location :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the fleet to register the compute to. You can
    -- use either the fleet ID or ARN value.
    fleetId :: Prelude.Text,
    -- | A descriptive label that is associated with the compute resource
    -- registered to your fleet.
    computeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterCompute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificatePath', 'registerCompute_certificatePath' - The path to the TLS certificate on your compute resource. The path and
-- certificate are not validated by GameLift.
--
-- 'dnsName', 'registerCompute_dnsName' - The DNS name of the compute resource. GameLift requires the DNS name or
-- IP address to manage your compute resource.
--
-- 'ipAddress', 'registerCompute_ipAddress' - The IP address of the compute resource. GameLift requires the DNS name
-- or IP address to manage your compute resource.
--
-- 'location', 'registerCompute_location' - The name of the custom location you added to the fleet you are
-- registering this compute resource to.
--
-- 'fleetId', 'registerCompute_fleetId' - A unique identifier for the fleet to register the compute to. You can
-- use either the fleet ID or ARN value.
--
-- 'computeName', 'registerCompute_computeName' - A descriptive label that is associated with the compute resource
-- registered to your fleet.
newRegisterCompute ::
  -- | 'fleetId'
  Prelude.Text ->
  -- | 'computeName'
  Prelude.Text ->
  RegisterCompute
newRegisterCompute pFleetId_ pComputeName_ =
  RegisterCompute'
    { certificatePath = Prelude.Nothing,
      dnsName = Prelude.Nothing,
      ipAddress = Prelude.Nothing,
      location = Prelude.Nothing,
      fleetId = pFleetId_,
      computeName = pComputeName_
    }

-- | The path to the TLS certificate on your compute resource. The path and
-- certificate are not validated by GameLift.
registerCompute_certificatePath :: Lens.Lens' RegisterCompute (Prelude.Maybe Prelude.Text)
registerCompute_certificatePath = Lens.lens (\RegisterCompute' {certificatePath} -> certificatePath) (\s@RegisterCompute' {} a -> s {certificatePath = a} :: RegisterCompute)

-- | The DNS name of the compute resource. GameLift requires the DNS name or
-- IP address to manage your compute resource.
registerCompute_dnsName :: Lens.Lens' RegisterCompute (Prelude.Maybe Prelude.Text)
registerCompute_dnsName = Lens.lens (\RegisterCompute' {dnsName} -> dnsName) (\s@RegisterCompute' {} a -> s {dnsName = a} :: RegisterCompute)

-- | The IP address of the compute resource. GameLift requires the DNS name
-- or IP address to manage your compute resource.
registerCompute_ipAddress :: Lens.Lens' RegisterCompute (Prelude.Maybe Prelude.Text)
registerCompute_ipAddress = Lens.lens (\RegisterCompute' {ipAddress} -> ipAddress) (\s@RegisterCompute' {} a -> s {ipAddress = a} :: RegisterCompute)

-- | The name of the custom location you added to the fleet you are
-- registering this compute resource to.
registerCompute_location :: Lens.Lens' RegisterCompute (Prelude.Maybe Prelude.Text)
registerCompute_location = Lens.lens (\RegisterCompute' {location} -> location) (\s@RegisterCompute' {} a -> s {location = a} :: RegisterCompute)

-- | A unique identifier for the fleet to register the compute to. You can
-- use either the fleet ID or ARN value.
registerCompute_fleetId :: Lens.Lens' RegisterCompute Prelude.Text
registerCompute_fleetId = Lens.lens (\RegisterCompute' {fleetId} -> fleetId) (\s@RegisterCompute' {} a -> s {fleetId = a} :: RegisterCompute)

-- | A descriptive label that is associated with the compute resource
-- registered to your fleet.
registerCompute_computeName :: Lens.Lens' RegisterCompute Prelude.Text
registerCompute_computeName = Lens.lens (\RegisterCompute' {computeName} -> computeName) (\s@RegisterCompute' {} a -> s {computeName = a} :: RegisterCompute)

instance Core.AWSRequest RegisterCompute where
  type
    AWSResponse RegisterCompute =
      RegisterComputeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterComputeResponse'
            Prelude.<$> (x Data..?> "Compute")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterCompute where
  hashWithSalt _salt RegisterCompute' {..} =
    _salt
      `Prelude.hashWithSalt` certificatePath
      `Prelude.hashWithSalt` dnsName
      `Prelude.hashWithSalt` ipAddress
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` fleetId
      `Prelude.hashWithSalt` computeName

instance Prelude.NFData RegisterCompute where
  rnf RegisterCompute' {..} =
    Prelude.rnf certificatePath
      `Prelude.seq` Prelude.rnf dnsName
      `Prelude.seq` Prelude.rnf ipAddress
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf fleetId
      `Prelude.seq` Prelude.rnf computeName

instance Data.ToHeaders RegisterCompute where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("GameLift.RegisterCompute" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RegisterCompute where
  toJSON RegisterCompute' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CertificatePath" Data..=)
              Prelude.<$> certificatePath,
            ("DnsName" Data..=) Prelude.<$> dnsName,
            ("IpAddress" Data..=) Prelude.<$> ipAddress,
            ("Location" Data..=) Prelude.<$> location,
            Prelude.Just ("FleetId" Data..= fleetId),
            Prelude.Just ("ComputeName" Data..= computeName)
          ]
      )

instance Data.ToPath RegisterCompute where
  toPath = Prelude.const "/"

instance Data.ToQuery RegisterCompute where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterComputeResponse' smart constructor.
data RegisterComputeResponse = RegisterComputeResponse'
  { -- | The details of the compute resource you registered to the specified
    -- fleet.
    compute :: Prelude.Maybe Compute,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterComputeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compute', 'registerComputeResponse_compute' - The details of the compute resource you registered to the specified
-- fleet.
--
-- 'httpStatus', 'registerComputeResponse_httpStatus' - The response's http status code.
newRegisterComputeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterComputeResponse
newRegisterComputeResponse pHttpStatus_ =
  RegisterComputeResponse'
    { compute = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the compute resource you registered to the specified
-- fleet.
registerComputeResponse_compute :: Lens.Lens' RegisterComputeResponse (Prelude.Maybe Compute)
registerComputeResponse_compute = Lens.lens (\RegisterComputeResponse' {compute} -> compute) (\s@RegisterComputeResponse' {} a -> s {compute = a} :: RegisterComputeResponse)

-- | The response's http status code.
registerComputeResponse_httpStatus :: Lens.Lens' RegisterComputeResponse Prelude.Int
registerComputeResponse_httpStatus = Lens.lens (\RegisterComputeResponse' {httpStatus} -> httpStatus) (\s@RegisterComputeResponse' {} a -> s {httpStatus = a} :: RegisterComputeResponse)

instance Prelude.NFData RegisterComputeResponse where
  rnf RegisterComputeResponse' {..} =
    Prelude.rnf compute
      `Prelude.seq` Prelude.rnf httpStatus
