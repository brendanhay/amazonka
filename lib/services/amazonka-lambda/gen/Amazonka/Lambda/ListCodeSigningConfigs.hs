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
-- Module      : Amazonka.Lambda.ListCodeSigningConfigs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuring-codesigning.html code signing configurations>.
-- A request returns up to 10,000 configurations per call. You can use the
-- @MaxItems@ parameter to return fewer configurations per call.
--
-- This operation returns paginated results.
module Amazonka.Lambda.ListCodeSigningConfigs
  ( -- * Creating a Request
    ListCodeSigningConfigs (..),
    newListCodeSigningConfigs,

    -- * Request Lenses
    listCodeSigningConfigs_marker,
    listCodeSigningConfigs_maxItems,

    -- * Destructuring the Response
    ListCodeSigningConfigsResponse (..),
    newListCodeSigningConfigsResponse,

    -- * Response Lenses
    listCodeSigningConfigsResponse_codeSigningConfigs,
    listCodeSigningConfigsResponse_nextMarker,
    listCodeSigningConfigsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCodeSigningConfigs' smart constructor.
data ListCodeSigningConfigs = ListCodeSigningConfigs'
  { -- | Specify the pagination token that\'s returned by a previous request to
    -- retrieve the next page of results.
    marker :: Prelude.Maybe Prelude.Text,
    -- | Maximum number of items to return.
    maxItems :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCodeSigningConfigs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listCodeSigningConfigs_marker' - Specify the pagination token that\'s returned by a previous request to
-- retrieve the next page of results.
--
-- 'maxItems', 'listCodeSigningConfigs_maxItems' - Maximum number of items to return.
newListCodeSigningConfigs ::
  ListCodeSigningConfigs
newListCodeSigningConfigs =
  ListCodeSigningConfigs'
    { marker = Prelude.Nothing,
      maxItems = Prelude.Nothing
    }

-- | Specify the pagination token that\'s returned by a previous request to
-- retrieve the next page of results.
listCodeSigningConfigs_marker :: Lens.Lens' ListCodeSigningConfigs (Prelude.Maybe Prelude.Text)
listCodeSigningConfigs_marker = Lens.lens (\ListCodeSigningConfigs' {marker} -> marker) (\s@ListCodeSigningConfigs' {} a -> s {marker = a} :: ListCodeSigningConfigs)

-- | Maximum number of items to return.
listCodeSigningConfigs_maxItems :: Lens.Lens' ListCodeSigningConfigs (Prelude.Maybe Prelude.Natural)
listCodeSigningConfigs_maxItems = Lens.lens (\ListCodeSigningConfigs' {maxItems} -> maxItems) (\s@ListCodeSigningConfigs' {} a -> s {maxItems = a} :: ListCodeSigningConfigs)

instance Core.AWSPager ListCodeSigningConfigs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCodeSigningConfigsResponse_nextMarker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCodeSigningConfigsResponse_codeSigningConfigs
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listCodeSigningConfigs_marker
          Lens..~ rs
          Lens.^? listCodeSigningConfigsResponse_nextMarker
          Prelude.. Lens._Just

instance Core.AWSRequest ListCodeSigningConfigs where
  type
    AWSResponse ListCodeSigningConfigs =
      ListCodeSigningConfigsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCodeSigningConfigsResponse'
            Prelude.<$> ( x
                            Data..?> "CodeSigningConfigs"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCodeSigningConfigs where
  hashWithSalt _salt ListCodeSigningConfigs' {..} =
    _salt
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems

instance Prelude.NFData ListCodeSigningConfigs where
  rnf ListCodeSigningConfigs' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems

instance Data.ToHeaders ListCodeSigningConfigs where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListCodeSigningConfigs where
  toPath =
    Prelude.const "/2020-04-22/code-signing-configs/"

instance Data.ToQuery ListCodeSigningConfigs where
  toQuery ListCodeSigningConfigs' {..} =
    Prelude.mconcat
      [ "Marker" Data.=: marker,
        "MaxItems" Data.=: maxItems
      ]

-- | /See:/ 'newListCodeSigningConfigsResponse' smart constructor.
data ListCodeSigningConfigsResponse = ListCodeSigningConfigsResponse'
  { -- | The code signing configurations
    codeSigningConfigs :: Prelude.Maybe [CodeSigningConfig],
    -- | The pagination token that\'s included if more results are available.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCodeSigningConfigsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codeSigningConfigs', 'listCodeSigningConfigsResponse_codeSigningConfigs' - The code signing configurations
--
-- 'nextMarker', 'listCodeSigningConfigsResponse_nextMarker' - The pagination token that\'s included if more results are available.
--
-- 'httpStatus', 'listCodeSigningConfigsResponse_httpStatus' - The response's http status code.
newListCodeSigningConfigsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCodeSigningConfigsResponse
newListCodeSigningConfigsResponse pHttpStatus_ =
  ListCodeSigningConfigsResponse'
    { codeSigningConfigs =
        Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The code signing configurations
listCodeSigningConfigsResponse_codeSigningConfigs :: Lens.Lens' ListCodeSigningConfigsResponse (Prelude.Maybe [CodeSigningConfig])
listCodeSigningConfigsResponse_codeSigningConfigs = Lens.lens (\ListCodeSigningConfigsResponse' {codeSigningConfigs} -> codeSigningConfigs) (\s@ListCodeSigningConfigsResponse' {} a -> s {codeSigningConfigs = a} :: ListCodeSigningConfigsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token that\'s included if more results are available.
listCodeSigningConfigsResponse_nextMarker :: Lens.Lens' ListCodeSigningConfigsResponse (Prelude.Maybe Prelude.Text)
listCodeSigningConfigsResponse_nextMarker = Lens.lens (\ListCodeSigningConfigsResponse' {nextMarker} -> nextMarker) (\s@ListCodeSigningConfigsResponse' {} a -> s {nextMarker = a} :: ListCodeSigningConfigsResponse)

-- | The response's http status code.
listCodeSigningConfigsResponse_httpStatus :: Lens.Lens' ListCodeSigningConfigsResponse Prelude.Int
listCodeSigningConfigsResponse_httpStatus = Lens.lens (\ListCodeSigningConfigsResponse' {httpStatus} -> httpStatus) (\s@ListCodeSigningConfigsResponse' {} a -> s {httpStatus = a} :: ListCodeSigningConfigsResponse)

instance
  Prelude.NFData
    ListCodeSigningConfigsResponse
  where
  rnf ListCodeSigningConfigsResponse' {..} =
    Prelude.rnf codeSigningConfigs
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf httpStatus
