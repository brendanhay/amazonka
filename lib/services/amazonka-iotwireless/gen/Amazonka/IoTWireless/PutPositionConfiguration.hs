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
-- Module      : Amazonka.IoTWireless.PutPositionConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Put position configuration for a given resource.
module Amazonka.IoTWireless.PutPositionConfiguration
  ( -- * Creating a Request
    PutPositionConfiguration (..),
    newPutPositionConfiguration,

    -- * Request Lenses
    putPositionConfiguration_destination,
    putPositionConfiguration_solvers,
    putPositionConfiguration_resourceIdentifier,
    putPositionConfiguration_resourceType,

    -- * Destructuring the Response
    PutPositionConfigurationResponse (..),
    newPutPositionConfigurationResponse,

    -- * Response Lenses
    putPositionConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutPositionConfiguration' smart constructor.
data PutPositionConfiguration = PutPositionConfiguration'
  { -- | The position data destination that describes the AWS IoT rule that
    -- processes the device\'s position data for use by AWS IoT Core for
    -- LoRaWAN.
    destination :: Prelude.Maybe Prelude.Text,
    -- | The positioning solvers used to update the position configuration of the
    -- resource.
    solvers :: Prelude.Maybe PositionSolverConfigurations,
    -- | Resource identifier used to update the position configuration.
    resourceIdentifier :: Prelude.Text,
    -- | Resource type of the resource for which you want to update the position
    -- configuration.
    resourceType :: PositionResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutPositionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'putPositionConfiguration_destination' - The position data destination that describes the AWS IoT rule that
-- processes the device\'s position data for use by AWS IoT Core for
-- LoRaWAN.
--
-- 'solvers', 'putPositionConfiguration_solvers' - The positioning solvers used to update the position configuration of the
-- resource.
--
-- 'resourceIdentifier', 'putPositionConfiguration_resourceIdentifier' - Resource identifier used to update the position configuration.
--
-- 'resourceType', 'putPositionConfiguration_resourceType' - Resource type of the resource for which you want to update the position
-- configuration.
newPutPositionConfiguration ::
  -- | 'resourceIdentifier'
  Prelude.Text ->
  -- | 'resourceType'
  PositionResourceType ->
  PutPositionConfiguration
newPutPositionConfiguration
  pResourceIdentifier_
  pResourceType_ =
    PutPositionConfiguration'
      { destination =
          Prelude.Nothing,
        solvers = Prelude.Nothing,
        resourceIdentifier = pResourceIdentifier_,
        resourceType = pResourceType_
      }

-- | The position data destination that describes the AWS IoT rule that
-- processes the device\'s position data for use by AWS IoT Core for
-- LoRaWAN.
putPositionConfiguration_destination :: Lens.Lens' PutPositionConfiguration (Prelude.Maybe Prelude.Text)
putPositionConfiguration_destination = Lens.lens (\PutPositionConfiguration' {destination} -> destination) (\s@PutPositionConfiguration' {} a -> s {destination = a} :: PutPositionConfiguration)

-- | The positioning solvers used to update the position configuration of the
-- resource.
putPositionConfiguration_solvers :: Lens.Lens' PutPositionConfiguration (Prelude.Maybe PositionSolverConfigurations)
putPositionConfiguration_solvers = Lens.lens (\PutPositionConfiguration' {solvers} -> solvers) (\s@PutPositionConfiguration' {} a -> s {solvers = a} :: PutPositionConfiguration)

-- | Resource identifier used to update the position configuration.
putPositionConfiguration_resourceIdentifier :: Lens.Lens' PutPositionConfiguration Prelude.Text
putPositionConfiguration_resourceIdentifier = Lens.lens (\PutPositionConfiguration' {resourceIdentifier} -> resourceIdentifier) (\s@PutPositionConfiguration' {} a -> s {resourceIdentifier = a} :: PutPositionConfiguration)

-- | Resource type of the resource for which you want to update the position
-- configuration.
putPositionConfiguration_resourceType :: Lens.Lens' PutPositionConfiguration PositionResourceType
putPositionConfiguration_resourceType = Lens.lens (\PutPositionConfiguration' {resourceType} -> resourceType) (\s@PutPositionConfiguration' {} a -> s {resourceType = a} :: PutPositionConfiguration)

instance Core.AWSRequest PutPositionConfiguration where
  type
    AWSResponse PutPositionConfiguration =
      PutPositionConfigurationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutPositionConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutPositionConfiguration where
  hashWithSalt _salt PutPositionConfiguration' {..} =
    _salt `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` solvers
      `Prelude.hashWithSalt` resourceIdentifier
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData PutPositionConfiguration where
  rnf PutPositionConfiguration' {..} =
    Prelude.rnf destination
      `Prelude.seq` Prelude.rnf solvers
      `Prelude.seq` Prelude.rnf resourceIdentifier
      `Prelude.seq` Prelude.rnf resourceType

instance Data.ToHeaders PutPositionConfiguration where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON PutPositionConfiguration where
  toJSON PutPositionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Destination" Data..=) Prelude.<$> destination,
            ("Solvers" Data..=) Prelude.<$> solvers
          ]
      )

instance Data.ToPath PutPositionConfiguration where
  toPath PutPositionConfiguration' {..} =
    Prelude.mconcat
      [ "/position-configurations/",
        Data.toBS resourceIdentifier
      ]

instance Data.ToQuery PutPositionConfiguration where
  toQuery PutPositionConfiguration' {..} =
    Prelude.mconcat
      ["resourceType" Data.=: resourceType]

-- | /See:/ 'newPutPositionConfigurationResponse' smart constructor.
data PutPositionConfigurationResponse = PutPositionConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutPositionConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putPositionConfigurationResponse_httpStatus' - The response's http status code.
newPutPositionConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutPositionConfigurationResponse
newPutPositionConfigurationResponse pHttpStatus_ =
  PutPositionConfigurationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putPositionConfigurationResponse_httpStatus :: Lens.Lens' PutPositionConfigurationResponse Prelude.Int
putPositionConfigurationResponse_httpStatus = Lens.lens (\PutPositionConfigurationResponse' {httpStatus} -> httpStatus) (\s@PutPositionConfigurationResponse' {} a -> s {httpStatus = a} :: PutPositionConfigurationResponse)

instance
  Prelude.NFData
    PutPositionConfigurationResponse
  where
  rnf PutPositionConfigurationResponse' {..} =
    Prelude.rnf httpStatus
