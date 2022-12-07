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
-- Module      : Amazonka.IoTWireless.GetPosition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the position information for a given resource.
module Amazonka.IoTWireless.GetPosition
  ( -- * Creating a Request
    GetPosition (..),
    newGetPosition,

    -- * Request Lenses
    getPosition_resourceIdentifier,
    getPosition_resourceType,

    -- * Destructuring the Response
    GetPositionResponse (..),
    newGetPositionResponse,

    -- * Response Lenses
    getPositionResponse_solverVersion,
    getPositionResponse_accuracy,
    getPositionResponse_solverType,
    getPositionResponse_timestamp,
    getPositionResponse_solverProvider,
    getPositionResponse_position,
    getPositionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPosition' smart constructor.
data GetPosition = GetPosition'
  { -- | Resource identifier used to retrieve the position information.
    resourceIdentifier :: Prelude.Text,
    -- | Resource type of the resource for which position information is
    -- retrieved.
    resourceType :: PositionResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPosition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceIdentifier', 'getPosition_resourceIdentifier' - Resource identifier used to retrieve the position information.
--
-- 'resourceType', 'getPosition_resourceType' - Resource type of the resource for which position information is
-- retrieved.
newGetPosition ::
  -- | 'resourceIdentifier'
  Prelude.Text ->
  -- | 'resourceType'
  PositionResourceType ->
  GetPosition
newGetPosition pResourceIdentifier_ pResourceType_ =
  GetPosition'
    { resourceIdentifier =
        pResourceIdentifier_,
      resourceType = pResourceType_
    }

-- | Resource identifier used to retrieve the position information.
getPosition_resourceIdentifier :: Lens.Lens' GetPosition Prelude.Text
getPosition_resourceIdentifier = Lens.lens (\GetPosition' {resourceIdentifier} -> resourceIdentifier) (\s@GetPosition' {} a -> s {resourceIdentifier = a} :: GetPosition)

-- | Resource type of the resource for which position information is
-- retrieved.
getPosition_resourceType :: Lens.Lens' GetPosition PositionResourceType
getPosition_resourceType = Lens.lens (\GetPosition' {resourceType} -> resourceType) (\s@GetPosition' {} a -> s {resourceType = a} :: GetPosition)

instance Core.AWSRequest GetPosition where
  type AWSResponse GetPosition = GetPositionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPositionResponse'
            Prelude.<$> (x Data..?> "SolverVersion")
            Prelude.<*> (x Data..?> "Accuracy")
            Prelude.<*> (x Data..?> "SolverType")
            Prelude.<*> (x Data..?> "Timestamp")
            Prelude.<*> (x Data..?> "SolverProvider")
            Prelude.<*> (x Data..?> "Position" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPosition where
  hashWithSalt _salt GetPosition' {..} =
    _salt `Prelude.hashWithSalt` resourceIdentifier
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData GetPosition where
  rnf GetPosition' {..} =
    Prelude.rnf resourceIdentifier
      `Prelude.seq` Prelude.rnf resourceType

instance Data.ToHeaders GetPosition where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetPosition where
  toPath GetPosition' {..} =
    Prelude.mconcat
      ["/positions/", Data.toBS resourceIdentifier]

instance Data.ToQuery GetPosition where
  toQuery GetPosition' {..} =
    Prelude.mconcat
      ["resourceType" Data.=: resourceType]

-- | /See:/ 'newGetPositionResponse' smart constructor.
data GetPositionResponse = GetPositionResponse'
  { -- | The version of the positioning solver.
    solverVersion :: Prelude.Maybe Prelude.Text,
    -- | The accuracy of the estimated position in meters. An empty value
    -- indicates that no position data is available. A value of ‘0.0’ value
    -- indicates that position data is available. This data corresponds to the
    -- position information that you specified instead of the position computed
    -- by solver.
    accuracy :: Prelude.Maybe Accuracy,
    -- | The type of solver used to identify the position of the resource.
    solverType :: Prelude.Maybe PositionSolverType,
    -- | The timestamp at which the device\'s position was determined.
    timestamp :: Prelude.Maybe Prelude.Text,
    -- | The vendor of the positioning solver.
    solverProvider :: Prelude.Maybe PositionSolverProvider,
    -- | The position information of the resource.
    position :: Prelude.Maybe [Prelude.Double],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPositionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'solverVersion', 'getPositionResponse_solverVersion' - The version of the positioning solver.
--
-- 'accuracy', 'getPositionResponse_accuracy' - The accuracy of the estimated position in meters. An empty value
-- indicates that no position data is available. A value of ‘0.0’ value
-- indicates that position data is available. This data corresponds to the
-- position information that you specified instead of the position computed
-- by solver.
--
-- 'solverType', 'getPositionResponse_solverType' - The type of solver used to identify the position of the resource.
--
-- 'timestamp', 'getPositionResponse_timestamp' - The timestamp at which the device\'s position was determined.
--
-- 'solverProvider', 'getPositionResponse_solverProvider' - The vendor of the positioning solver.
--
-- 'position', 'getPositionResponse_position' - The position information of the resource.
--
-- 'httpStatus', 'getPositionResponse_httpStatus' - The response's http status code.
newGetPositionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPositionResponse
newGetPositionResponse pHttpStatus_ =
  GetPositionResponse'
    { solverVersion =
        Prelude.Nothing,
      accuracy = Prelude.Nothing,
      solverType = Prelude.Nothing,
      timestamp = Prelude.Nothing,
      solverProvider = Prelude.Nothing,
      position = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The version of the positioning solver.
getPositionResponse_solverVersion :: Lens.Lens' GetPositionResponse (Prelude.Maybe Prelude.Text)
getPositionResponse_solverVersion = Lens.lens (\GetPositionResponse' {solverVersion} -> solverVersion) (\s@GetPositionResponse' {} a -> s {solverVersion = a} :: GetPositionResponse)

-- | The accuracy of the estimated position in meters. An empty value
-- indicates that no position data is available. A value of ‘0.0’ value
-- indicates that position data is available. This data corresponds to the
-- position information that you specified instead of the position computed
-- by solver.
getPositionResponse_accuracy :: Lens.Lens' GetPositionResponse (Prelude.Maybe Accuracy)
getPositionResponse_accuracy = Lens.lens (\GetPositionResponse' {accuracy} -> accuracy) (\s@GetPositionResponse' {} a -> s {accuracy = a} :: GetPositionResponse)

-- | The type of solver used to identify the position of the resource.
getPositionResponse_solverType :: Lens.Lens' GetPositionResponse (Prelude.Maybe PositionSolverType)
getPositionResponse_solverType = Lens.lens (\GetPositionResponse' {solverType} -> solverType) (\s@GetPositionResponse' {} a -> s {solverType = a} :: GetPositionResponse)

-- | The timestamp at which the device\'s position was determined.
getPositionResponse_timestamp :: Lens.Lens' GetPositionResponse (Prelude.Maybe Prelude.Text)
getPositionResponse_timestamp = Lens.lens (\GetPositionResponse' {timestamp} -> timestamp) (\s@GetPositionResponse' {} a -> s {timestamp = a} :: GetPositionResponse)

-- | The vendor of the positioning solver.
getPositionResponse_solverProvider :: Lens.Lens' GetPositionResponse (Prelude.Maybe PositionSolverProvider)
getPositionResponse_solverProvider = Lens.lens (\GetPositionResponse' {solverProvider} -> solverProvider) (\s@GetPositionResponse' {} a -> s {solverProvider = a} :: GetPositionResponse)

-- | The position information of the resource.
getPositionResponse_position :: Lens.Lens' GetPositionResponse (Prelude.Maybe [Prelude.Double])
getPositionResponse_position = Lens.lens (\GetPositionResponse' {position} -> position) (\s@GetPositionResponse' {} a -> s {position = a} :: GetPositionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getPositionResponse_httpStatus :: Lens.Lens' GetPositionResponse Prelude.Int
getPositionResponse_httpStatus = Lens.lens (\GetPositionResponse' {httpStatus} -> httpStatus) (\s@GetPositionResponse' {} a -> s {httpStatus = a} :: GetPositionResponse)

instance Prelude.NFData GetPositionResponse where
  rnf GetPositionResponse' {..} =
    Prelude.rnf solverVersion
      `Prelude.seq` Prelude.rnf accuracy
      `Prelude.seq` Prelude.rnf solverType
      `Prelude.seq` Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf solverProvider
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf httpStatus
