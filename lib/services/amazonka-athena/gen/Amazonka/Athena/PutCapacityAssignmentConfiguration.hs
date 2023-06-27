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
-- Module      : Amazonka.Athena.PutCapacityAssignmentConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Puts a new capacity assignment configuration for a specified capacity
-- reservation. If a capacity assignment configuration already exists for
-- the capacity reservation, replaces the existing capacity assignment
-- configuration.
module Amazonka.Athena.PutCapacityAssignmentConfiguration
  ( -- * Creating a Request
    PutCapacityAssignmentConfiguration (..),
    newPutCapacityAssignmentConfiguration,

    -- * Request Lenses
    putCapacityAssignmentConfiguration_capacityReservationName,
    putCapacityAssignmentConfiguration_capacityAssignments,

    -- * Destructuring the Response
    PutCapacityAssignmentConfigurationResponse (..),
    newPutCapacityAssignmentConfigurationResponse,

    -- * Response Lenses
    putCapacityAssignmentConfigurationResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutCapacityAssignmentConfiguration' smart constructor.
data PutCapacityAssignmentConfiguration = PutCapacityAssignmentConfiguration'
  { -- | The name of the capacity reservation to put a capacity assignment
    -- configuration for.
    capacityReservationName :: Prelude.Text,
    -- | The list of assignments for the capacity assignment configuration.
    capacityAssignments :: [CapacityAssignment]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutCapacityAssignmentConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityReservationName', 'putCapacityAssignmentConfiguration_capacityReservationName' - The name of the capacity reservation to put a capacity assignment
-- configuration for.
--
-- 'capacityAssignments', 'putCapacityAssignmentConfiguration_capacityAssignments' - The list of assignments for the capacity assignment configuration.
newPutCapacityAssignmentConfiguration ::
  -- | 'capacityReservationName'
  Prelude.Text ->
  PutCapacityAssignmentConfiguration
newPutCapacityAssignmentConfiguration
  pCapacityReservationName_ =
    PutCapacityAssignmentConfiguration'
      { capacityReservationName =
          pCapacityReservationName_,
        capacityAssignments = Prelude.mempty
      }

-- | The name of the capacity reservation to put a capacity assignment
-- configuration for.
putCapacityAssignmentConfiguration_capacityReservationName :: Lens.Lens' PutCapacityAssignmentConfiguration Prelude.Text
putCapacityAssignmentConfiguration_capacityReservationName = Lens.lens (\PutCapacityAssignmentConfiguration' {capacityReservationName} -> capacityReservationName) (\s@PutCapacityAssignmentConfiguration' {} a -> s {capacityReservationName = a} :: PutCapacityAssignmentConfiguration)

-- | The list of assignments for the capacity assignment configuration.
putCapacityAssignmentConfiguration_capacityAssignments :: Lens.Lens' PutCapacityAssignmentConfiguration [CapacityAssignment]
putCapacityAssignmentConfiguration_capacityAssignments = Lens.lens (\PutCapacityAssignmentConfiguration' {capacityAssignments} -> capacityAssignments) (\s@PutCapacityAssignmentConfiguration' {} a -> s {capacityAssignments = a} :: PutCapacityAssignmentConfiguration) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    PutCapacityAssignmentConfiguration
  where
  type
    AWSResponse PutCapacityAssignmentConfiguration =
      PutCapacityAssignmentConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutCapacityAssignmentConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutCapacityAssignmentConfiguration
  where
  hashWithSalt
    _salt
    PutCapacityAssignmentConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` capacityReservationName
        `Prelude.hashWithSalt` capacityAssignments

instance
  Prelude.NFData
    PutCapacityAssignmentConfiguration
  where
  rnf PutCapacityAssignmentConfiguration' {..} =
    Prelude.rnf capacityReservationName
      `Prelude.seq` Prelude.rnf capacityAssignments

instance
  Data.ToHeaders
    PutCapacityAssignmentConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.PutCapacityAssignmentConfiguration" ::
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
    PutCapacityAssignmentConfiguration
  where
  toJSON PutCapacityAssignmentConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "CapacityReservationName"
                  Data..= capacityReservationName
              ),
            Prelude.Just
              ("CapacityAssignments" Data..= capacityAssignments)
          ]
      )

instance
  Data.ToPath
    PutCapacityAssignmentConfiguration
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    PutCapacityAssignmentConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutCapacityAssignmentConfigurationResponse' smart constructor.
data PutCapacityAssignmentConfigurationResponse = PutCapacityAssignmentConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutCapacityAssignmentConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putCapacityAssignmentConfigurationResponse_httpStatus' - The response's http status code.
newPutCapacityAssignmentConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutCapacityAssignmentConfigurationResponse
newPutCapacityAssignmentConfigurationResponse
  pHttpStatus_ =
    PutCapacityAssignmentConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
putCapacityAssignmentConfigurationResponse_httpStatus :: Lens.Lens' PutCapacityAssignmentConfigurationResponse Prelude.Int
putCapacityAssignmentConfigurationResponse_httpStatus = Lens.lens (\PutCapacityAssignmentConfigurationResponse' {httpStatus} -> httpStatus) (\s@PutCapacityAssignmentConfigurationResponse' {} a -> s {httpStatus = a} :: PutCapacityAssignmentConfigurationResponse)

instance
  Prelude.NFData
    PutCapacityAssignmentConfigurationResponse
  where
  rnf PutCapacityAssignmentConfigurationResponse' {..} =
    Prelude.rnf httpStatus
