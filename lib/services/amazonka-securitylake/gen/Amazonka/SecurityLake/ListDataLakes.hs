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
-- Module      : Amazonka.SecurityLake.ListDataLakes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the Amazon Security Lake configuration object for the
-- specified Amazon Web Services account ID. You can use the
-- @ListDataLakes@ API to know whether Security Lake is enabled for any
-- region.
module Amazonka.SecurityLake.ListDataLakes
  ( -- * Creating a Request
    ListDataLakes (..),
    newListDataLakes,

    -- * Request Lenses
    listDataLakes_regions,

    -- * Destructuring the Response
    ListDataLakesResponse (..),
    newListDataLakesResponse,

    -- * Response Lenses
    listDataLakesResponse_dataLakes,
    listDataLakesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newListDataLakes' smart constructor.
data ListDataLakes = ListDataLakes'
  { -- | The list of regions where Security Lake is enabled.
    regions :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDataLakes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regions', 'listDataLakes_regions' - The list of regions where Security Lake is enabled.
newListDataLakes ::
  ListDataLakes
newListDataLakes =
  ListDataLakes' {regions = Prelude.Nothing}

-- | The list of regions where Security Lake is enabled.
listDataLakes_regions :: Lens.Lens' ListDataLakes (Prelude.Maybe [Prelude.Text])
listDataLakes_regions = Lens.lens (\ListDataLakes' {regions} -> regions) (\s@ListDataLakes' {} a -> s {regions = a} :: ListDataLakes) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest ListDataLakes where
  type
    AWSResponse ListDataLakes =
      ListDataLakesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDataLakesResponse'
            Prelude.<$> (x Data..?> "dataLakes" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDataLakes where
  hashWithSalt _salt ListDataLakes' {..} =
    _salt `Prelude.hashWithSalt` regions

instance Prelude.NFData ListDataLakes where
  rnf ListDataLakes' {..} = Prelude.rnf regions

instance Data.ToHeaders ListDataLakes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListDataLakes where
  toPath = Prelude.const "/v1/datalakes"

instance Data.ToQuery ListDataLakes where
  toQuery ListDataLakes' {..} =
    Prelude.mconcat
      [ "regions"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> regions)
      ]

-- | /See:/ 'newListDataLakesResponse' smart constructor.
data ListDataLakesResponse = ListDataLakesResponse'
  { -- | Retrieves the Security Lake configuration object.
    dataLakes :: Prelude.Maybe [DataLakeResource],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDataLakesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataLakes', 'listDataLakesResponse_dataLakes' - Retrieves the Security Lake configuration object.
--
-- 'httpStatus', 'listDataLakesResponse_httpStatus' - The response's http status code.
newListDataLakesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDataLakesResponse
newListDataLakesResponse pHttpStatus_ =
  ListDataLakesResponse'
    { dataLakes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Retrieves the Security Lake configuration object.
listDataLakesResponse_dataLakes :: Lens.Lens' ListDataLakesResponse (Prelude.Maybe [DataLakeResource])
listDataLakesResponse_dataLakes = Lens.lens (\ListDataLakesResponse' {dataLakes} -> dataLakes) (\s@ListDataLakesResponse' {} a -> s {dataLakes = a} :: ListDataLakesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDataLakesResponse_httpStatus :: Lens.Lens' ListDataLakesResponse Prelude.Int
listDataLakesResponse_httpStatus = Lens.lens (\ListDataLakesResponse' {httpStatus} -> httpStatus) (\s@ListDataLakesResponse' {} a -> s {httpStatus = a} :: ListDataLakesResponse)

instance Prelude.NFData ListDataLakesResponse where
  rnf ListDataLakesResponse' {..} =
    Prelude.rnf dataLakes
      `Prelude.seq` Prelude.rnf httpStatus
