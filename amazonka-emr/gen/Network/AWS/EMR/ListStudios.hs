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
-- Module      : Network.AWS.EMR.ListStudios
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all Amazon EMR Studios associated with the AWS
-- account. The list includes details such as ID, Studio Access URL, and
-- creation time for each Studio.
--
-- This operation returns paginated results.
module Network.AWS.EMR.ListStudios
  ( -- * Creating a Request
    ListStudios (..),
    newListStudios,

    -- * Request Lenses
    listStudios_marker,

    -- * Destructuring the Response
    ListStudiosResponse (..),
    newListStudiosResponse,

    -- * Response Lenses
    listStudiosResponse_studios,
    listStudiosResponse_marker,
    listStudiosResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListStudios' smart constructor.
data ListStudios = ListStudios'
  { -- | The pagination token that indicates the set of results to retrieve.
    marker :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListStudios' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listStudios_marker' - The pagination token that indicates the set of results to retrieve.
newListStudios ::
  ListStudios
newListStudios = ListStudios' {marker = Core.Nothing}

-- | The pagination token that indicates the set of results to retrieve.
listStudios_marker :: Lens.Lens' ListStudios (Core.Maybe Core.Text)
listStudios_marker = Lens.lens (\ListStudios' {marker} -> marker) (\s@ListStudios' {} a -> s {marker = a} :: ListStudios)

instance Core.AWSPager ListStudios where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listStudiosResponse_marker Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listStudiosResponse_studios Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listStudios_marker
          Lens..~ rs
          Lens.^? listStudiosResponse_marker Core.. Lens._Just

instance Core.AWSRequest ListStudios where
  type AWSResponse ListStudios = ListStudiosResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStudiosResponse'
            Core.<$> (x Core..?> "Studios" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListStudios

instance Core.NFData ListStudios

instance Core.ToHeaders ListStudios where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("ElasticMapReduce.ListStudios" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListStudios where
  toJSON ListStudios' {..} =
    Core.object
      (Core.catMaybes [("Marker" Core..=) Core.<$> marker])

instance Core.ToPath ListStudios where
  toPath = Core.const "/"

instance Core.ToQuery ListStudios where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListStudiosResponse' smart constructor.
data ListStudiosResponse = ListStudiosResponse'
  { -- | The list of Studio summary objects.
    studios :: Core.Maybe [StudioSummary],
    -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListStudiosResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'studios', 'listStudiosResponse_studios' - The list of Studio summary objects.
--
-- 'marker', 'listStudiosResponse_marker' - The pagination token that indicates the next set of results to retrieve.
--
-- 'httpStatus', 'listStudiosResponse_httpStatus' - The response's http status code.
newListStudiosResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListStudiosResponse
newListStudiosResponse pHttpStatus_ =
  ListStudiosResponse'
    { studios = Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of Studio summary objects.
listStudiosResponse_studios :: Lens.Lens' ListStudiosResponse (Core.Maybe [StudioSummary])
listStudiosResponse_studios = Lens.lens (\ListStudiosResponse' {studios} -> studios) (\s@ListStudiosResponse' {} a -> s {studios = a} :: ListStudiosResponse) Core.. Lens.mapping Lens._Coerce

-- | The pagination token that indicates the next set of results to retrieve.
listStudiosResponse_marker :: Lens.Lens' ListStudiosResponse (Core.Maybe Core.Text)
listStudiosResponse_marker = Lens.lens (\ListStudiosResponse' {marker} -> marker) (\s@ListStudiosResponse' {} a -> s {marker = a} :: ListStudiosResponse)

-- | The response's http status code.
listStudiosResponse_httpStatus :: Lens.Lens' ListStudiosResponse Core.Int
listStudiosResponse_httpStatus = Lens.lens (\ListStudiosResponse' {httpStatus} -> httpStatus) (\s@ListStudiosResponse' {} a -> s {httpStatus = a} :: ListStudiosResponse)

instance Core.NFData ListStudiosResponse
