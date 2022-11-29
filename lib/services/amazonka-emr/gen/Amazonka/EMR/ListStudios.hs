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
-- Module      : Amazonka.EMR.ListStudios
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all Amazon EMR Studios associated with the Amazon Web
-- Services account. The list includes details such as ID, Studio Access
-- URL, and creation time for each Studio.
--
-- This operation returns paginated results.
module Amazonka.EMR.ListStudios
  ( -- * Creating a Request
    ListStudios (..),
    newListStudios,

    -- * Request Lenses
    listStudios_marker,

    -- * Destructuring the Response
    ListStudiosResponse (..),
    newListStudiosResponse,

    -- * Response Lenses
    listStudiosResponse_marker,
    listStudiosResponse_studios,
    listStudiosResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListStudios' smart constructor.
data ListStudios = ListStudios'
  { -- | The pagination token that indicates the set of results to retrieve.
    marker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
newListStudios =
  ListStudios' {marker = Prelude.Nothing}

-- | The pagination token that indicates the set of results to retrieve.
listStudios_marker :: Lens.Lens' ListStudios (Prelude.Maybe Prelude.Text)
listStudios_marker = Lens.lens (\ListStudios' {marker} -> marker) (\s@ListStudios' {} a -> s {marker = a} :: ListStudios)

instance Core.AWSPager ListStudios where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listStudiosResponse_marker Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listStudiosResponse_studios Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listStudios_marker
          Lens..~ rs
          Lens.^? listStudiosResponse_marker Prelude.. Lens._Just

instance Core.AWSRequest ListStudios where
  type AWSResponse ListStudios = ListStudiosResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStudiosResponse'
            Prelude.<$> (x Core..?> "Marker")
            Prelude.<*> (x Core..?> "Studios" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListStudios where
  hashWithSalt _salt ListStudios' {..} =
    _salt `Prelude.hashWithSalt` marker

instance Prelude.NFData ListStudios where
  rnf ListStudios' {..} = Prelude.rnf marker

instance Core.ToHeaders ListStudios where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ElasticMapReduce.ListStudios" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListStudios where
  toJSON ListStudios' {..} =
    Core.object
      ( Prelude.catMaybes
          [("Marker" Core..=) Prelude.<$> marker]
      )

instance Core.ToPath ListStudios where
  toPath = Prelude.const "/"

instance Core.ToQuery ListStudios where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListStudiosResponse' smart constructor.
data ListStudiosResponse = ListStudiosResponse'
  { -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The list of Studio summary objects.
    studios :: Prelude.Maybe [StudioSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStudiosResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listStudiosResponse_marker' - The pagination token that indicates the next set of results to retrieve.
--
-- 'studios', 'listStudiosResponse_studios' - The list of Studio summary objects.
--
-- 'httpStatus', 'listStudiosResponse_httpStatus' - The response's http status code.
newListStudiosResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListStudiosResponse
newListStudiosResponse pHttpStatus_ =
  ListStudiosResponse'
    { marker = Prelude.Nothing,
      studios = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token that indicates the next set of results to retrieve.
listStudiosResponse_marker :: Lens.Lens' ListStudiosResponse (Prelude.Maybe Prelude.Text)
listStudiosResponse_marker = Lens.lens (\ListStudiosResponse' {marker} -> marker) (\s@ListStudiosResponse' {} a -> s {marker = a} :: ListStudiosResponse)

-- | The list of Studio summary objects.
listStudiosResponse_studios :: Lens.Lens' ListStudiosResponse (Prelude.Maybe [StudioSummary])
listStudiosResponse_studios = Lens.lens (\ListStudiosResponse' {studios} -> studios) (\s@ListStudiosResponse' {} a -> s {studios = a} :: ListStudiosResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listStudiosResponse_httpStatus :: Lens.Lens' ListStudiosResponse Prelude.Int
listStudiosResponse_httpStatus = Lens.lens (\ListStudiosResponse' {httpStatus} -> httpStatus) (\s@ListStudiosResponse' {} a -> s {httpStatus = a} :: ListStudiosResponse)

instance Prelude.NFData ListStudiosResponse where
  rnf ListStudiosResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf studios
      `Prelude.seq` Prelude.rnf httpStatus
