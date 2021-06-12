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
-- Module      : Network.AWS.Discovery.DescribeContinuousExports
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.Discovery.DescribeContinuousExports
  ( -- * Creating a Request
    DescribeContinuousExports (..),
    newDescribeContinuousExports,

    -- * Request Lenses
    describeContinuousExports_nextToken,
    describeContinuousExports_maxResults,
    describeContinuousExports_exportIds,

    -- * Destructuring the Response
    DescribeContinuousExportsResponse (..),
    newDescribeContinuousExportsResponse,

    -- * Response Lenses
    describeContinuousExportsResponse_nextToken,
    describeContinuousExportsResponse_descriptions,
    describeContinuousExportsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeContinuousExports' smart constructor.
data DescribeContinuousExports = DescribeContinuousExports'
  { -- | The token from the previous call to @DescribeExportTasks@.
    nextToken :: Core.Maybe Core.Text,
    -- | A number between 1 and 100 specifying the maximum number of continuous
    -- export descriptions returned.
    maxResults :: Core.Maybe Core.Natural,
    -- | The unique IDs assigned to the exports.
    exportIds :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeContinuousExports' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeContinuousExports_nextToken' - The token from the previous call to @DescribeExportTasks@.
--
-- 'maxResults', 'describeContinuousExports_maxResults' - A number between 1 and 100 specifying the maximum number of continuous
-- export descriptions returned.
--
-- 'exportIds', 'describeContinuousExports_exportIds' - The unique IDs assigned to the exports.
newDescribeContinuousExports ::
  DescribeContinuousExports
newDescribeContinuousExports =
  DescribeContinuousExports'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      exportIds = Core.Nothing
    }

-- | The token from the previous call to @DescribeExportTasks@.
describeContinuousExports_nextToken :: Lens.Lens' DescribeContinuousExports (Core.Maybe Core.Text)
describeContinuousExports_nextToken = Lens.lens (\DescribeContinuousExports' {nextToken} -> nextToken) (\s@DescribeContinuousExports' {} a -> s {nextToken = a} :: DescribeContinuousExports)

-- | A number between 1 and 100 specifying the maximum number of continuous
-- export descriptions returned.
describeContinuousExports_maxResults :: Lens.Lens' DescribeContinuousExports (Core.Maybe Core.Natural)
describeContinuousExports_maxResults = Lens.lens (\DescribeContinuousExports' {maxResults} -> maxResults) (\s@DescribeContinuousExports' {} a -> s {maxResults = a} :: DescribeContinuousExports)

-- | The unique IDs assigned to the exports.
describeContinuousExports_exportIds :: Lens.Lens' DescribeContinuousExports (Core.Maybe [Core.Text])
describeContinuousExports_exportIds = Lens.lens (\DescribeContinuousExports' {exportIds} -> exportIds) (\s@DescribeContinuousExports' {} a -> s {exportIds = a} :: DescribeContinuousExports) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeContinuousExports where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeContinuousExportsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeContinuousExportsResponse_descriptions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeContinuousExports_nextToken
          Lens..~ rs
          Lens.^? describeContinuousExportsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeContinuousExports where
  type
    AWSResponse DescribeContinuousExports =
      DescribeContinuousExportsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeContinuousExportsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "descriptions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeContinuousExports

instance Core.NFData DescribeContinuousExports

instance Core.ToHeaders DescribeContinuousExports where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSPoseidonService_V2015_11_01.DescribeContinuousExports" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeContinuousExports where
  toJSON DescribeContinuousExports' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("exportIds" Core..=) Core.<$> exportIds
          ]
      )

instance Core.ToPath DescribeContinuousExports where
  toPath = Core.const "/"

instance Core.ToQuery DescribeContinuousExports where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeContinuousExportsResponse' smart constructor.
data DescribeContinuousExportsResponse = DescribeContinuousExportsResponse'
  { -- | The token from the previous call to @DescribeExportTasks@.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of continuous export descriptions.
    descriptions :: Core.Maybe [ContinuousExportDescription],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeContinuousExportsResponse
newDescribeContinuousExportsResponse pHttpStatus_ =
  DescribeContinuousExportsResponse'
    { nextToken =
        Core.Nothing,
      descriptions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token from the previous call to @DescribeExportTasks@.
describeContinuousExportsResponse_nextToken :: Lens.Lens' DescribeContinuousExportsResponse (Core.Maybe Core.Text)
describeContinuousExportsResponse_nextToken = Lens.lens (\DescribeContinuousExportsResponse' {nextToken} -> nextToken) (\s@DescribeContinuousExportsResponse' {} a -> s {nextToken = a} :: DescribeContinuousExportsResponse)

-- | A list of continuous export descriptions.
describeContinuousExportsResponse_descriptions :: Lens.Lens' DescribeContinuousExportsResponse (Core.Maybe [ContinuousExportDescription])
describeContinuousExportsResponse_descriptions = Lens.lens (\DescribeContinuousExportsResponse' {descriptions} -> descriptions) (\s@DescribeContinuousExportsResponse' {} a -> s {descriptions = a} :: DescribeContinuousExportsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeContinuousExportsResponse_httpStatus :: Lens.Lens' DescribeContinuousExportsResponse Core.Int
describeContinuousExportsResponse_httpStatus = Lens.lens (\DescribeContinuousExportsResponse' {httpStatus} -> httpStatus) (\s@DescribeContinuousExportsResponse' {} a -> s {httpStatus = a} :: DescribeContinuousExportsResponse)

instance
  Core.NFData
    DescribeContinuousExportsResponse
