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
-- Module      : Amazonka.EMR.ListInstanceFleets
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all available details about the instance fleets in a cluster.
--
-- The instance fleet configuration is available only in Amazon EMR
-- versions 4.8.0 and later, excluding 5.0.x versions.
--
-- This operation returns paginated results.
module Amazonka.EMR.ListInstanceFleets
  ( -- * Creating a Request
    ListInstanceFleets (..),
    newListInstanceFleets,

    -- * Request Lenses
    listInstanceFleets_marker,
    listInstanceFleets_clusterId,

    -- * Destructuring the Response
    ListInstanceFleetsResponse (..),
    newListInstanceFleetsResponse,

    -- * Response Lenses
    listInstanceFleetsResponse_marker,
    listInstanceFleetsResponse_instanceFleets,
    listInstanceFleetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListInstanceFleets' smart constructor.
data ListInstanceFleets = ListInstanceFleets'
  { -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the cluster.
    clusterId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInstanceFleets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listInstanceFleets_marker' - The pagination token that indicates the next set of results to retrieve.
--
-- 'clusterId', 'listInstanceFleets_clusterId' - The unique identifier of the cluster.
newListInstanceFleets ::
  -- | 'clusterId'
  Prelude.Text ->
  ListInstanceFleets
newListInstanceFleets pClusterId_ =
  ListInstanceFleets'
    { marker = Prelude.Nothing,
      clusterId = pClusterId_
    }

-- | The pagination token that indicates the next set of results to retrieve.
listInstanceFleets_marker :: Lens.Lens' ListInstanceFleets (Prelude.Maybe Prelude.Text)
listInstanceFleets_marker = Lens.lens (\ListInstanceFleets' {marker} -> marker) (\s@ListInstanceFleets' {} a -> s {marker = a} :: ListInstanceFleets)

-- | The unique identifier of the cluster.
listInstanceFleets_clusterId :: Lens.Lens' ListInstanceFleets Prelude.Text
listInstanceFleets_clusterId = Lens.lens (\ListInstanceFleets' {clusterId} -> clusterId) (\s@ListInstanceFleets' {} a -> s {clusterId = a} :: ListInstanceFleets)

instance Core.AWSPager ListInstanceFleets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listInstanceFleetsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listInstanceFleetsResponse_instanceFleets
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listInstanceFleets_marker
          Lens..~ rs
          Lens.^? listInstanceFleetsResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest ListInstanceFleets where
  type
    AWSResponse ListInstanceFleets =
      ListInstanceFleetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInstanceFleetsResponse'
            Prelude.<$> (x Data..?> "Marker")
            Prelude.<*> (x Data..?> "InstanceFleets" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListInstanceFleets where
  hashWithSalt _salt ListInstanceFleets' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` clusterId

instance Prelude.NFData ListInstanceFleets where
  rnf ListInstanceFleets' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf clusterId

instance Data.ToHeaders ListInstanceFleets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ElasticMapReduce.ListInstanceFleets" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListInstanceFleets where
  toJSON ListInstanceFleets' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Marker" Data..=) Prelude.<$> marker,
            Prelude.Just ("ClusterId" Data..= clusterId)
          ]
      )

instance Data.ToPath ListInstanceFleets where
  toPath = Prelude.const "/"

instance Data.ToQuery ListInstanceFleets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListInstanceFleetsResponse' smart constructor.
data ListInstanceFleetsResponse = ListInstanceFleetsResponse'
  { -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The list of instance fleets for the cluster and given filters.
    instanceFleets :: Prelude.Maybe [InstanceFleet],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInstanceFleetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listInstanceFleetsResponse_marker' - The pagination token that indicates the next set of results to retrieve.
--
-- 'instanceFleets', 'listInstanceFleetsResponse_instanceFleets' - The list of instance fleets for the cluster and given filters.
--
-- 'httpStatus', 'listInstanceFleetsResponse_httpStatus' - The response's http status code.
newListInstanceFleetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListInstanceFleetsResponse
newListInstanceFleetsResponse pHttpStatus_ =
  ListInstanceFleetsResponse'
    { marker =
        Prelude.Nothing,
      instanceFleets = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token that indicates the next set of results to retrieve.
listInstanceFleetsResponse_marker :: Lens.Lens' ListInstanceFleetsResponse (Prelude.Maybe Prelude.Text)
listInstanceFleetsResponse_marker = Lens.lens (\ListInstanceFleetsResponse' {marker} -> marker) (\s@ListInstanceFleetsResponse' {} a -> s {marker = a} :: ListInstanceFleetsResponse)

-- | The list of instance fleets for the cluster and given filters.
listInstanceFleetsResponse_instanceFleets :: Lens.Lens' ListInstanceFleetsResponse (Prelude.Maybe [InstanceFleet])
listInstanceFleetsResponse_instanceFleets = Lens.lens (\ListInstanceFleetsResponse' {instanceFleets} -> instanceFleets) (\s@ListInstanceFleetsResponse' {} a -> s {instanceFleets = a} :: ListInstanceFleetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listInstanceFleetsResponse_httpStatus :: Lens.Lens' ListInstanceFleetsResponse Prelude.Int
listInstanceFleetsResponse_httpStatus = Lens.lens (\ListInstanceFleetsResponse' {httpStatus} -> httpStatus) (\s@ListInstanceFleetsResponse' {} a -> s {httpStatus = a} :: ListInstanceFleetsResponse)

instance Prelude.NFData ListInstanceFleetsResponse where
  rnf ListInstanceFleetsResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf instanceFleets
      `Prelude.seq` Prelude.rnf httpStatus
