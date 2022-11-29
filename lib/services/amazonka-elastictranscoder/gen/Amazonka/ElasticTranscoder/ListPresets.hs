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
-- Module      : Amazonka.ElasticTranscoder.ListPresets
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The ListPresets operation gets a list of the default presets included
-- with Elastic Transcoder and the presets that you\'ve added in an AWS
-- region.
--
-- This operation returns paginated results.
module Amazonka.ElasticTranscoder.ListPresets
  ( -- * Creating a Request
    ListPresets (..),
    newListPresets,

    -- * Request Lenses
    listPresets_ascending,
    listPresets_pageToken,

    -- * Destructuring the Response
    ListPresetsResponse (..),
    newListPresetsResponse,

    -- * Response Lenses
    listPresetsResponse_presets,
    listPresetsResponse_nextPageToken,
    listPresetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ElasticTranscoder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The @ListPresetsRequest@ structure.
--
-- /See:/ 'newListPresets' smart constructor.
data ListPresets = ListPresets'
  { -- | To list presets in chronological order by the date and time that they
    -- were created, enter @true@. To list presets in reverse chronological
    -- order, enter @false@.
    ascending :: Prelude.Maybe Prelude.Text,
    -- | When Elastic Transcoder returns more than one page of results, use
    -- @pageToken@ in subsequent @GET@ requests to get each successive page of
    -- results.
    pageToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPresets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ascending', 'listPresets_ascending' - To list presets in chronological order by the date and time that they
-- were created, enter @true@. To list presets in reverse chronological
-- order, enter @false@.
--
-- 'pageToken', 'listPresets_pageToken' - When Elastic Transcoder returns more than one page of results, use
-- @pageToken@ in subsequent @GET@ requests to get each successive page of
-- results.
newListPresets ::
  ListPresets
newListPresets =
  ListPresets'
    { ascending = Prelude.Nothing,
      pageToken = Prelude.Nothing
    }

-- | To list presets in chronological order by the date and time that they
-- were created, enter @true@. To list presets in reverse chronological
-- order, enter @false@.
listPresets_ascending :: Lens.Lens' ListPresets (Prelude.Maybe Prelude.Text)
listPresets_ascending = Lens.lens (\ListPresets' {ascending} -> ascending) (\s@ListPresets' {} a -> s {ascending = a} :: ListPresets)

-- | When Elastic Transcoder returns more than one page of results, use
-- @pageToken@ in subsequent @GET@ requests to get each successive page of
-- results.
listPresets_pageToken :: Lens.Lens' ListPresets (Prelude.Maybe Prelude.Text)
listPresets_pageToken = Lens.lens (\ListPresets' {pageToken} -> pageToken) (\s@ListPresets' {} a -> s {pageToken = a} :: ListPresets)

instance Core.AWSPager ListPresets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPresetsResponse_nextPageToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPresetsResponse_presets Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listPresets_pageToken
          Lens..~ rs
          Lens.^? listPresetsResponse_nextPageToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListPresets where
  type AWSResponse ListPresets = ListPresetsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPresetsResponse'
            Prelude.<$> (x Core..?> "Presets" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextPageToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPresets where
  hashWithSalt _salt ListPresets' {..} =
    _salt `Prelude.hashWithSalt` ascending
      `Prelude.hashWithSalt` pageToken

instance Prelude.NFData ListPresets where
  rnf ListPresets' {..} =
    Prelude.rnf ascending
      `Prelude.seq` Prelude.rnf pageToken

instance Core.ToHeaders ListPresets where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListPresets where
  toPath = Prelude.const "/2012-09-25/presets"

instance Core.ToQuery ListPresets where
  toQuery ListPresets' {..} =
    Prelude.mconcat
      [ "Ascending" Core.=: ascending,
        "PageToken" Core.=: pageToken
      ]

-- | The @ListPresetsResponse@ structure.
--
-- /See:/ 'newListPresetsResponse' smart constructor.
data ListPresetsResponse = ListPresetsResponse'
  { -- | An array of @Preset@ objects.
    presets :: Prelude.Maybe [Preset],
    -- | A value that you use to access the second and subsequent pages of
    -- results, if any. When the presets fit on one page or when you\'ve
    -- reached the last page of results, the value of @NextPageToken@ is
    -- @null@.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPresetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'presets', 'listPresetsResponse_presets' - An array of @Preset@ objects.
--
-- 'nextPageToken', 'listPresetsResponse_nextPageToken' - A value that you use to access the second and subsequent pages of
-- results, if any. When the presets fit on one page or when you\'ve
-- reached the last page of results, the value of @NextPageToken@ is
-- @null@.
--
-- 'httpStatus', 'listPresetsResponse_httpStatus' - The response's http status code.
newListPresetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPresetsResponse
newListPresetsResponse pHttpStatus_ =
  ListPresetsResponse'
    { presets = Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of @Preset@ objects.
listPresetsResponse_presets :: Lens.Lens' ListPresetsResponse (Prelude.Maybe [Preset])
listPresetsResponse_presets = Lens.lens (\ListPresetsResponse' {presets} -> presets) (\s@ListPresetsResponse' {} a -> s {presets = a} :: ListPresetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A value that you use to access the second and subsequent pages of
-- results, if any. When the presets fit on one page or when you\'ve
-- reached the last page of results, the value of @NextPageToken@ is
-- @null@.
listPresetsResponse_nextPageToken :: Lens.Lens' ListPresetsResponse (Prelude.Maybe Prelude.Text)
listPresetsResponse_nextPageToken = Lens.lens (\ListPresetsResponse' {nextPageToken} -> nextPageToken) (\s@ListPresetsResponse' {} a -> s {nextPageToken = a} :: ListPresetsResponse)

-- | The response's http status code.
listPresetsResponse_httpStatus :: Lens.Lens' ListPresetsResponse Prelude.Int
listPresetsResponse_httpStatus = Lens.lens (\ListPresetsResponse' {httpStatus} -> httpStatus) (\s@ListPresetsResponse' {} a -> s {httpStatus = a} :: ListPresetsResponse)

instance Prelude.NFData ListPresetsResponse where
  rnf ListPresetsResponse' {..} =
    Prelude.rnf presets
      `Prelude.seq` Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf httpStatus
