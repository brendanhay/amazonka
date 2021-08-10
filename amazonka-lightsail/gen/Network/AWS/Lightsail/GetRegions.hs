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
-- Module      : Network.AWS.Lightsail.GetRegions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all valid regions for Amazon Lightsail. Use the
-- @include availability zones@ parameter to also return the Availability
-- Zones in a region.
module Network.AWS.Lightsail.GetRegions
  ( -- * Creating a Request
    GetRegions (..),
    newGetRegions,

    -- * Request Lenses
    getRegions_includeRelationalDatabaseAvailabilityZones,
    getRegions_includeAvailabilityZones,

    -- * Destructuring the Response
    GetRegionsResponse (..),
    newGetRegionsResponse,

    -- * Response Lenses
    getRegionsResponse_regions,
    getRegionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetRegions' smart constructor.
data GetRegions = GetRegions'
  { -- | A Boolean value indicating whether to also include Availability Zones
    -- for databases in your get regions request. Availability Zones are
    -- indicated with a letter (e.g., @us-east-2a@).
    includeRelationalDatabaseAvailabilityZones :: Prelude.Maybe Prelude.Bool,
    -- | A Boolean value indicating whether to also include Availability Zones in
    -- your get regions request. Availability Zones are indicated with a
    -- letter: e.g., @us-east-2a@.
    includeAvailabilityZones :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRegions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeRelationalDatabaseAvailabilityZones', 'getRegions_includeRelationalDatabaseAvailabilityZones' - A Boolean value indicating whether to also include Availability Zones
-- for databases in your get regions request. Availability Zones are
-- indicated with a letter (e.g., @us-east-2a@).
--
-- 'includeAvailabilityZones', 'getRegions_includeAvailabilityZones' - A Boolean value indicating whether to also include Availability Zones in
-- your get regions request. Availability Zones are indicated with a
-- letter: e.g., @us-east-2a@.
newGetRegions ::
  GetRegions
newGetRegions =
  GetRegions'
    { includeRelationalDatabaseAvailabilityZones =
        Prelude.Nothing,
      includeAvailabilityZones = Prelude.Nothing
    }

-- | A Boolean value indicating whether to also include Availability Zones
-- for databases in your get regions request. Availability Zones are
-- indicated with a letter (e.g., @us-east-2a@).
getRegions_includeRelationalDatabaseAvailabilityZones :: Lens.Lens' GetRegions (Prelude.Maybe Prelude.Bool)
getRegions_includeRelationalDatabaseAvailabilityZones = Lens.lens (\GetRegions' {includeRelationalDatabaseAvailabilityZones} -> includeRelationalDatabaseAvailabilityZones) (\s@GetRegions' {} a -> s {includeRelationalDatabaseAvailabilityZones = a} :: GetRegions)

-- | A Boolean value indicating whether to also include Availability Zones in
-- your get regions request. Availability Zones are indicated with a
-- letter: e.g., @us-east-2a@.
getRegions_includeAvailabilityZones :: Lens.Lens' GetRegions (Prelude.Maybe Prelude.Bool)
getRegions_includeAvailabilityZones = Lens.lens (\GetRegions' {includeAvailabilityZones} -> includeAvailabilityZones) (\s@GetRegions' {} a -> s {includeAvailabilityZones = a} :: GetRegions)

instance Core.AWSRequest GetRegions where
  type AWSResponse GetRegions = GetRegionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRegionsResponse'
            Prelude.<$> (x Core..?> "regions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRegions

instance Prelude.NFData GetRegions

instance Core.ToHeaders GetRegions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetRegions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetRegions where
  toJSON GetRegions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ( "includeRelationalDatabaseAvailabilityZones"
                Core..=
            )
              Prelude.<$> includeRelationalDatabaseAvailabilityZones,
            ("includeAvailabilityZones" Core..=)
              Prelude.<$> includeAvailabilityZones
          ]
      )

instance Core.ToPath GetRegions where
  toPath = Prelude.const "/"

instance Core.ToQuery GetRegions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRegionsResponse' smart constructor.
data GetRegionsResponse = GetRegionsResponse'
  { -- | An array of key-value pairs containing information about your get
    -- regions request.
    regions :: Prelude.Maybe [RegionInfo],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRegionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regions', 'getRegionsResponse_regions' - An array of key-value pairs containing information about your get
-- regions request.
--
-- 'httpStatus', 'getRegionsResponse_httpStatus' - The response's http status code.
newGetRegionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRegionsResponse
newGetRegionsResponse pHttpStatus_ =
  GetRegionsResponse'
    { regions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of key-value pairs containing information about your get
-- regions request.
getRegionsResponse_regions :: Lens.Lens' GetRegionsResponse (Prelude.Maybe [RegionInfo])
getRegionsResponse_regions = Lens.lens (\GetRegionsResponse' {regions} -> regions) (\s@GetRegionsResponse' {} a -> s {regions = a} :: GetRegionsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getRegionsResponse_httpStatus :: Lens.Lens' GetRegionsResponse Prelude.Int
getRegionsResponse_httpStatus = Lens.lens (\GetRegionsResponse' {httpStatus} -> httpStatus) (\s@GetRegionsResponse' {} a -> s {httpStatus = a} :: GetRegionsResponse)

instance Prelude.NFData GetRegionsResponse
