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
-- Module      : Amazonka.Discovery.DescribeContinuousExports
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists exports as specified by ID. All continuous exports associated with
-- your user account can be listed if you call @DescribeContinuousExports@
-- as is without passing any parameters.
--
-- This operation returns paginated results.
module Amazonka.Discovery.DescribeContinuousExports
  ( -- * Creating a Request
    DescribeContinuousExports (..),
    newDescribeContinuousExports,

    -- * Request Lenses
    describeContinuousExports_exportIds,
    describeContinuousExports_nextToken,
    describeContinuousExports_maxResults,

    -- * Destructuring the Response
    DescribeContinuousExportsResponse (..),
    newDescribeContinuousExportsResponse,

    -- * Response Lenses
    describeContinuousExportsResponse_nextToken,
    describeContinuousExportsResponse_descriptions,
    describeContinuousExportsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Discovery.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeContinuousExports' smart constructor.
data DescribeContinuousExports = DescribeContinuousExports'
  { -- | The unique IDs assigned to the exports.
    exportIds :: Prelude.Maybe [Prelude.Text],
    -- | The token from the previous call to @DescribeExportTasks@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A number between 1 and 100 specifying the maximum number of continuous
    -- export descriptions returned.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeContinuousExports' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportIds', 'describeContinuousExports_exportIds' - The unique IDs assigned to the exports.
--
-- 'nextToken', 'describeContinuousExports_nextToken' - The token from the previous call to @DescribeExportTasks@.
--
-- 'maxResults', 'describeContinuousExports_maxResults' - A number between 1 and 100 specifying the maximum number of continuous
-- export descriptions returned.
newDescribeContinuousExports ::
  DescribeContinuousExports
newDescribeContinuousExports =
  DescribeContinuousExports'
    { exportIds =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The unique IDs assigned to the exports.
describeContinuousExports_exportIds :: Lens.Lens' DescribeContinuousExports (Prelude.Maybe [Prelude.Text])
describeContinuousExports_exportIds = Lens.lens (\DescribeContinuousExports' {exportIds} -> exportIds) (\s@DescribeContinuousExports' {} a -> s {exportIds = a} :: DescribeContinuousExports) Prelude.. Lens.mapping Lens.coerced

-- | The token from the previous call to @DescribeExportTasks@.
describeContinuousExports_nextToken :: Lens.Lens' DescribeContinuousExports (Prelude.Maybe Prelude.Text)
describeContinuousExports_nextToken = Lens.lens (\DescribeContinuousExports' {nextToken} -> nextToken) (\s@DescribeContinuousExports' {} a -> s {nextToken = a} :: DescribeContinuousExports)

-- | A number between 1 and 100 specifying the maximum number of continuous
-- export descriptions returned.
describeContinuousExports_maxResults :: Lens.Lens' DescribeContinuousExports (Prelude.Maybe Prelude.Natural)
describeContinuousExports_maxResults = Lens.lens (\DescribeContinuousExports' {maxResults} -> maxResults) (\s@DescribeContinuousExports' {} a -> s {maxResults = a} :: DescribeContinuousExports)

instance Core.AWSPager DescribeContinuousExports where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeContinuousExportsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeContinuousExportsResponse_descriptions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeContinuousExports_nextToken
          Lens..~ rs
          Lens.^? describeContinuousExportsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeContinuousExports where
  type
    AWSResponse DescribeContinuousExports =
      DescribeContinuousExportsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeContinuousExportsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "descriptions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeContinuousExports where
  hashWithSalt _salt DescribeContinuousExports' {..} =
    _salt `Prelude.hashWithSalt` exportIds
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeContinuousExports where
  rnf DescribeContinuousExports' {..} =
    Prelude.rnf exportIds
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders DescribeContinuousExports where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSPoseidonService_V2015_11_01.DescribeContinuousExports" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeContinuousExports where
  toJSON DescribeContinuousExports' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("exportIds" Core..=) Prelude.<$> exportIds,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath DescribeContinuousExports where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeContinuousExports where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeContinuousExportsResponse' smart constructor.
data DescribeContinuousExportsResponse = DescribeContinuousExportsResponse'
  { -- | The token from the previous call to @DescribeExportTasks@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of continuous export descriptions.
    descriptions :: Prelude.Maybe [ContinuousExportDescription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeContinuousExportsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeContinuousExportsResponse_nextToken' - The token from the previous call to @DescribeExportTasks@.
--
-- 'descriptions', 'describeContinuousExportsResponse_descriptions' - A list of continuous export descriptions.
--
-- 'httpStatus', 'describeContinuousExportsResponse_httpStatus' - The response's http status code.
newDescribeContinuousExportsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeContinuousExportsResponse
newDescribeContinuousExportsResponse pHttpStatus_ =
  DescribeContinuousExportsResponse'
    { nextToken =
        Prelude.Nothing,
      descriptions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token from the previous call to @DescribeExportTasks@.
describeContinuousExportsResponse_nextToken :: Lens.Lens' DescribeContinuousExportsResponse (Prelude.Maybe Prelude.Text)
describeContinuousExportsResponse_nextToken = Lens.lens (\DescribeContinuousExportsResponse' {nextToken} -> nextToken) (\s@DescribeContinuousExportsResponse' {} a -> s {nextToken = a} :: DescribeContinuousExportsResponse)

-- | A list of continuous export descriptions.
describeContinuousExportsResponse_descriptions :: Lens.Lens' DescribeContinuousExportsResponse (Prelude.Maybe [ContinuousExportDescription])
describeContinuousExportsResponse_descriptions = Lens.lens (\DescribeContinuousExportsResponse' {descriptions} -> descriptions) (\s@DescribeContinuousExportsResponse' {} a -> s {descriptions = a} :: DescribeContinuousExportsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeContinuousExportsResponse_httpStatus :: Lens.Lens' DescribeContinuousExportsResponse Prelude.Int
describeContinuousExportsResponse_httpStatus = Lens.lens (\DescribeContinuousExportsResponse' {httpStatus} -> httpStatus) (\s@DescribeContinuousExportsResponse' {} a -> s {httpStatus = a} :: DescribeContinuousExportsResponse)

instance
  Prelude.NFData
    DescribeContinuousExportsResponse
  where
  rnf DescribeContinuousExportsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf descriptions
      `Prelude.seq` Prelude.rnf httpStatus
