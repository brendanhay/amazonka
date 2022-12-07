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
-- Module      : Amazonka.IoTFleetWise.ListVehicles
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of summaries of created vehicles.
--
-- This API operation uses pagination. Specify the @nextToken@ parameter in
-- the request to return more results.
--
-- This operation returns paginated results.
module Amazonka.IoTFleetWise.ListVehicles
  ( -- * Creating a Request
    ListVehicles (..),
    newListVehicles,

    -- * Request Lenses
    listVehicles_nextToken,
    listVehicles_modelManifestArn,
    listVehicles_maxResults,

    -- * Destructuring the Response
    ListVehiclesResponse (..),
    newListVehiclesResponse,

    -- * Response Lenses
    listVehiclesResponse_nextToken,
    listVehiclesResponse_vehicleSummaries,
    listVehiclesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListVehicles' smart constructor.
data ListVehicles = ListVehicles'
  { -- | A pagination token for the next set of results.
    --
    -- If the results of a search are large, only a portion of the results are
    -- returned, and a @nextToken@ pagination token is returned in the
    -- response. To retrieve the next set of results, reissue the search
    -- request and include the returned token. When all results have been
    -- returned, the response does not contain a pagination token value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of a vehicle model (model manifest). You
    -- can use this optional parameter to list only the vehicles created from a
    -- certain vehicle model.
    modelManifestArn :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return, between 1 and 100, inclusive.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVehicles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listVehicles_nextToken' - A pagination token for the next set of results.
--
-- If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next set of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
--
-- 'modelManifestArn', 'listVehicles_modelManifestArn' - The Amazon Resource Name (ARN) of a vehicle model (model manifest). You
-- can use this optional parameter to list only the vehicles created from a
-- certain vehicle model.
--
-- 'maxResults', 'listVehicles_maxResults' - The maximum number of items to return, between 1 and 100, inclusive.
newListVehicles ::
  ListVehicles
newListVehicles =
  ListVehicles'
    { nextToken = Prelude.Nothing,
      modelManifestArn = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A pagination token for the next set of results.
--
-- If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next set of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
listVehicles_nextToken :: Lens.Lens' ListVehicles (Prelude.Maybe Prelude.Text)
listVehicles_nextToken = Lens.lens (\ListVehicles' {nextToken} -> nextToken) (\s@ListVehicles' {} a -> s {nextToken = a} :: ListVehicles)

-- | The Amazon Resource Name (ARN) of a vehicle model (model manifest). You
-- can use this optional parameter to list only the vehicles created from a
-- certain vehicle model.
listVehicles_modelManifestArn :: Lens.Lens' ListVehicles (Prelude.Maybe Prelude.Text)
listVehicles_modelManifestArn = Lens.lens (\ListVehicles' {modelManifestArn} -> modelManifestArn) (\s@ListVehicles' {} a -> s {modelManifestArn = a} :: ListVehicles)

-- | The maximum number of items to return, between 1 and 100, inclusive.
listVehicles_maxResults :: Lens.Lens' ListVehicles (Prelude.Maybe Prelude.Natural)
listVehicles_maxResults = Lens.lens (\ListVehicles' {maxResults} -> maxResults) (\s@ListVehicles' {} a -> s {maxResults = a} :: ListVehicles)

instance Core.AWSPager ListVehicles where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listVehiclesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listVehiclesResponse_vehicleSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listVehicles_nextToken
          Lens..~ rs
          Lens.^? listVehiclesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListVehicles where
  type AWSResponse ListVehicles = ListVehiclesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVehiclesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x Data..?> "vehicleSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListVehicles where
  hashWithSalt _salt ListVehicles' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` modelManifestArn
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListVehicles where
  rnf ListVehicles' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf modelManifestArn
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListVehicles where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "IoTAutobahnControlPlane.ListVehicles" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListVehicles where
  toJSON ListVehicles' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            ("modelManifestArn" Data..=)
              Prelude.<$> modelManifestArn,
            ("maxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListVehicles where
  toPath = Prelude.const "/"

instance Data.ToQuery ListVehicles where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListVehiclesResponse' smart constructor.
data ListVehiclesResponse = ListVehiclesResponse'
  { -- | The token to retrieve the next set of results, or @null@ if there are no
    -- more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of vehicles and information about them.
    vehicleSummaries :: Prelude.Maybe [VehicleSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVehiclesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listVehiclesResponse_nextToken' - The token to retrieve the next set of results, or @null@ if there are no
-- more results.
--
-- 'vehicleSummaries', 'listVehiclesResponse_vehicleSummaries' - A list of vehicles and information about them.
--
-- 'httpStatus', 'listVehiclesResponse_httpStatus' - The response's http status code.
newListVehiclesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListVehiclesResponse
newListVehiclesResponse pHttpStatus_ =
  ListVehiclesResponse'
    { nextToken = Prelude.Nothing,
      vehicleSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to retrieve the next set of results, or @null@ if there are no
-- more results.
listVehiclesResponse_nextToken :: Lens.Lens' ListVehiclesResponse (Prelude.Maybe Prelude.Text)
listVehiclesResponse_nextToken = Lens.lens (\ListVehiclesResponse' {nextToken} -> nextToken) (\s@ListVehiclesResponse' {} a -> s {nextToken = a} :: ListVehiclesResponse)

-- | A list of vehicles and information about them.
listVehiclesResponse_vehicleSummaries :: Lens.Lens' ListVehiclesResponse (Prelude.Maybe [VehicleSummary])
listVehiclesResponse_vehicleSummaries = Lens.lens (\ListVehiclesResponse' {vehicleSummaries} -> vehicleSummaries) (\s@ListVehiclesResponse' {} a -> s {vehicleSummaries = a} :: ListVehiclesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listVehiclesResponse_httpStatus :: Lens.Lens' ListVehiclesResponse Prelude.Int
listVehiclesResponse_httpStatus = Lens.lens (\ListVehiclesResponse' {httpStatus} -> httpStatus) (\s@ListVehiclesResponse' {} a -> s {httpStatus = a} :: ListVehiclesResponse)

instance Prelude.NFData ListVehiclesResponse where
  rnf ListVehiclesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf vehicleSummaries
      `Prelude.seq` Prelude.rnf httpStatus
