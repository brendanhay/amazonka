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
-- Module      : Amazonka.IoTWireless.GetPositionConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get position configuration for a given resource.
module Amazonka.IoTWireless.GetPositionConfiguration
  ( -- * Creating a Request
    GetPositionConfiguration (..),
    newGetPositionConfiguration,

    -- * Request Lenses
    getPositionConfiguration_resourceIdentifier,
    getPositionConfiguration_resourceType,

    -- * Destructuring the Response
    GetPositionConfigurationResponse (..),
    newGetPositionConfigurationResponse,

    -- * Response Lenses
    getPositionConfigurationResponse_destination,
    getPositionConfigurationResponse_solvers,
    getPositionConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPositionConfiguration' smart constructor.
data GetPositionConfiguration = GetPositionConfiguration'
  { -- | Resource identifier used in a position configuration.
    resourceIdentifier :: Prelude.Text,
    -- | Resource type of the resource for which position configuration is
    -- retrieved.
    resourceType :: PositionResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPositionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceIdentifier', 'getPositionConfiguration_resourceIdentifier' - Resource identifier used in a position configuration.
--
-- 'resourceType', 'getPositionConfiguration_resourceType' - Resource type of the resource for which position configuration is
-- retrieved.
newGetPositionConfiguration ::
  -- | 'resourceIdentifier'
  Prelude.Text ->
  -- | 'resourceType'
  PositionResourceType ->
  GetPositionConfiguration
newGetPositionConfiguration
  pResourceIdentifier_
  pResourceType_ =
    GetPositionConfiguration'
      { resourceIdentifier =
          pResourceIdentifier_,
        resourceType = pResourceType_
      }

-- | Resource identifier used in a position configuration.
getPositionConfiguration_resourceIdentifier :: Lens.Lens' GetPositionConfiguration Prelude.Text
getPositionConfiguration_resourceIdentifier = Lens.lens (\GetPositionConfiguration' {resourceIdentifier} -> resourceIdentifier) (\s@GetPositionConfiguration' {} a -> s {resourceIdentifier = a} :: GetPositionConfiguration)

-- | Resource type of the resource for which position configuration is
-- retrieved.
getPositionConfiguration_resourceType :: Lens.Lens' GetPositionConfiguration PositionResourceType
getPositionConfiguration_resourceType = Lens.lens (\GetPositionConfiguration' {resourceType} -> resourceType) (\s@GetPositionConfiguration' {} a -> s {resourceType = a} :: GetPositionConfiguration)

instance Core.AWSRequest GetPositionConfiguration where
  type
    AWSResponse GetPositionConfiguration =
      GetPositionConfigurationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPositionConfigurationResponse'
            Prelude.<$> (x Core..?> "Destination")
            Prelude.<*> (x Core..?> "Solvers")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPositionConfiguration where
  hashWithSalt _salt GetPositionConfiguration' {..} =
    _salt `Prelude.hashWithSalt` resourceIdentifier
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData GetPositionConfiguration where
  rnf GetPositionConfiguration' {..} =
    Prelude.rnf resourceIdentifier
      `Prelude.seq` Prelude.rnf resourceType

instance Core.ToHeaders GetPositionConfiguration where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetPositionConfiguration where
  toPath GetPositionConfiguration' {..} =
    Prelude.mconcat
      [ "/position-configurations/",
        Core.toBS resourceIdentifier
      ]

instance Core.ToQuery GetPositionConfiguration where
  toQuery GetPositionConfiguration' {..} =
    Prelude.mconcat
      ["resourceType" Core.=: resourceType]

-- | /See:/ 'newGetPositionConfigurationResponse' smart constructor.
data GetPositionConfigurationResponse = GetPositionConfigurationResponse'
  { -- | The position data destination that describes the AWS IoT rule that
    -- processes the device\'s position data for use by AWS IoT Core for
    -- LoRaWAN.
    destination :: Prelude.Maybe Prelude.Text,
    -- | The wrapper for the solver configuration details object.
    solvers :: Prelude.Maybe PositionSolverDetails,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPositionConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'getPositionConfigurationResponse_destination' - The position data destination that describes the AWS IoT rule that
-- processes the device\'s position data for use by AWS IoT Core for
-- LoRaWAN.
--
-- 'solvers', 'getPositionConfigurationResponse_solvers' - The wrapper for the solver configuration details object.
--
-- 'httpStatus', 'getPositionConfigurationResponse_httpStatus' - The response's http status code.
newGetPositionConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPositionConfigurationResponse
newGetPositionConfigurationResponse pHttpStatus_ =
  GetPositionConfigurationResponse'
    { destination =
        Prelude.Nothing,
      solvers = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The position data destination that describes the AWS IoT rule that
-- processes the device\'s position data for use by AWS IoT Core for
-- LoRaWAN.
getPositionConfigurationResponse_destination :: Lens.Lens' GetPositionConfigurationResponse (Prelude.Maybe Prelude.Text)
getPositionConfigurationResponse_destination = Lens.lens (\GetPositionConfigurationResponse' {destination} -> destination) (\s@GetPositionConfigurationResponse' {} a -> s {destination = a} :: GetPositionConfigurationResponse)

-- | The wrapper for the solver configuration details object.
getPositionConfigurationResponse_solvers :: Lens.Lens' GetPositionConfigurationResponse (Prelude.Maybe PositionSolverDetails)
getPositionConfigurationResponse_solvers = Lens.lens (\GetPositionConfigurationResponse' {solvers} -> solvers) (\s@GetPositionConfigurationResponse' {} a -> s {solvers = a} :: GetPositionConfigurationResponse)

-- | The response's http status code.
getPositionConfigurationResponse_httpStatus :: Lens.Lens' GetPositionConfigurationResponse Prelude.Int
getPositionConfigurationResponse_httpStatus = Lens.lens (\GetPositionConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetPositionConfigurationResponse' {} a -> s {httpStatus = a} :: GetPositionConfigurationResponse)

instance
  Prelude.NFData
    GetPositionConfigurationResponse
  where
  rnf GetPositionConfigurationResponse' {..} =
    Prelude.rnf destination
      `Prelude.seq` Prelude.rnf solvers
      `Prelude.seq` Prelude.rnf httpStatus
