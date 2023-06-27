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
-- Module      : Amazonka.Athena.GetCapacityAssignmentConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the capacity assignment configuration for a capacity reservation,
-- if one exists.
module Amazonka.Athena.GetCapacityAssignmentConfiguration
  ( -- * Creating a Request
    GetCapacityAssignmentConfiguration (..),
    newGetCapacityAssignmentConfiguration,

    -- * Request Lenses
    getCapacityAssignmentConfiguration_capacityReservationName,

    -- * Destructuring the Response
    GetCapacityAssignmentConfigurationResponse (..),
    newGetCapacityAssignmentConfigurationResponse,

    -- * Response Lenses
    getCapacityAssignmentConfigurationResponse_httpStatus,
    getCapacityAssignmentConfigurationResponse_capacityAssignmentConfiguration,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCapacityAssignmentConfiguration' smart constructor.
data GetCapacityAssignmentConfiguration = GetCapacityAssignmentConfiguration'
  { -- | The name of the capacity reservation to retrieve the capacity assignment
    -- configuration for.
    capacityReservationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCapacityAssignmentConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityReservationName', 'getCapacityAssignmentConfiguration_capacityReservationName' - The name of the capacity reservation to retrieve the capacity assignment
-- configuration for.
newGetCapacityAssignmentConfiguration ::
  -- | 'capacityReservationName'
  Prelude.Text ->
  GetCapacityAssignmentConfiguration
newGetCapacityAssignmentConfiguration
  pCapacityReservationName_ =
    GetCapacityAssignmentConfiguration'
      { capacityReservationName =
          pCapacityReservationName_
      }

-- | The name of the capacity reservation to retrieve the capacity assignment
-- configuration for.
getCapacityAssignmentConfiguration_capacityReservationName :: Lens.Lens' GetCapacityAssignmentConfiguration Prelude.Text
getCapacityAssignmentConfiguration_capacityReservationName = Lens.lens (\GetCapacityAssignmentConfiguration' {capacityReservationName} -> capacityReservationName) (\s@GetCapacityAssignmentConfiguration' {} a -> s {capacityReservationName = a} :: GetCapacityAssignmentConfiguration)

instance
  Core.AWSRequest
    GetCapacityAssignmentConfiguration
  where
  type
    AWSResponse GetCapacityAssignmentConfiguration =
      GetCapacityAssignmentConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCapacityAssignmentConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "CapacityAssignmentConfiguration")
      )

instance
  Prelude.Hashable
    GetCapacityAssignmentConfiguration
  where
  hashWithSalt
    _salt
    GetCapacityAssignmentConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` capacityReservationName

instance
  Prelude.NFData
    GetCapacityAssignmentConfiguration
  where
  rnf GetCapacityAssignmentConfiguration' {..} =
    Prelude.rnf capacityReservationName

instance
  Data.ToHeaders
    GetCapacityAssignmentConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.GetCapacityAssignmentConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    GetCapacityAssignmentConfiguration
  where
  toJSON GetCapacityAssignmentConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "CapacityReservationName"
                  Data..= capacityReservationName
              )
          ]
      )

instance
  Data.ToPath
    GetCapacityAssignmentConfiguration
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    GetCapacityAssignmentConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCapacityAssignmentConfigurationResponse' smart constructor.
data GetCapacityAssignmentConfigurationResponse = GetCapacityAssignmentConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The requested capacity assignment configuration for the specified
    -- capacity reservation.
    capacityAssignmentConfiguration :: CapacityAssignmentConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCapacityAssignmentConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getCapacityAssignmentConfigurationResponse_httpStatus' - The response's http status code.
--
-- 'capacityAssignmentConfiguration', 'getCapacityAssignmentConfigurationResponse_capacityAssignmentConfiguration' - The requested capacity assignment configuration for the specified
-- capacity reservation.
newGetCapacityAssignmentConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'capacityAssignmentConfiguration'
  CapacityAssignmentConfiguration ->
  GetCapacityAssignmentConfigurationResponse
newGetCapacityAssignmentConfigurationResponse
  pHttpStatus_
  pCapacityAssignmentConfiguration_ =
    GetCapacityAssignmentConfigurationResponse'
      { httpStatus =
          pHttpStatus_,
        capacityAssignmentConfiguration =
          pCapacityAssignmentConfiguration_
      }

-- | The response's http status code.
getCapacityAssignmentConfigurationResponse_httpStatus :: Lens.Lens' GetCapacityAssignmentConfigurationResponse Prelude.Int
getCapacityAssignmentConfigurationResponse_httpStatus = Lens.lens (\GetCapacityAssignmentConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetCapacityAssignmentConfigurationResponse' {} a -> s {httpStatus = a} :: GetCapacityAssignmentConfigurationResponse)

-- | The requested capacity assignment configuration for the specified
-- capacity reservation.
getCapacityAssignmentConfigurationResponse_capacityAssignmentConfiguration :: Lens.Lens' GetCapacityAssignmentConfigurationResponse CapacityAssignmentConfiguration
getCapacityAssignmentConfigurationResponse_capacityAssignmentConfiguration = Lens.lens (\GetCapacityAssignmentConfigurationResponse' {capacityAssignmentConfiguration} -> capacityAssignmentConfiguration) (\s@GetCapacityAssignmentConfigurationResponse' {} a -> s {capacityAssignmentConfiguration = a} :: GetCapacityAssignmentConfigurationResponse)

instance
  Prelude.NFData
    GetCapacityAssignmentConfigurationResponse
  where
  rnf GetCapacityAssignmentConfigurationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf capacityAssignmentConfiguration
