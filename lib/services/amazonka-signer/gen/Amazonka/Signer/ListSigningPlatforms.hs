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
-- Module      : Amazonka.Signer.ListSigningPlatforms
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all signing platforms available in code signing that match the
-- request parameters. If additional jobs remain to be listed, code signing
-- returns a @nextToken@ value. Use this value in subsequent calls to
-- @ListSigningJobs@ to fetch the remaining values. You can continue
-- calling @ListSigningJobs@ with your @maxResults@ parameter and with new
-- values that code signing returns in the @nextToken@ parameter until all
-- of your signing jobs have been returned.
--
-- This operation returns paginated results.
module Amazonka.Signer.ListSigningPlatforms
  ( -- * Creating a Request
    ListSigningPlatforms (..),
    newListSigningPlatforms,

    -- * Request Lenses
    listSigningPlatforms_partner,
    listSigningPlatforms_nextToken,
    listSigningPlatforms_target,
    listSigningPlatforms_maxResults,
    listSigningPlatforms_category,

    -- * Destructuring the Response
    ListSigningPlatformsResponse (..),
    newListSigningPlatformsResponse,

    -- * Response Lenses
    listSigningPlatformsResponse_nextToken,
    listSigningPlatformsResponse_platforms,
    listSigningPlatformsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Signer.Types

-- | /See:/ 'newListSigningPlatforms' smart constructor.
data ListSigningPlatforms = ListSigningPlatforms'
  { -- | Any partner entities connected to a signing platform.
    partner :: Prelude.Maybe Prelude.Text,
    -- | Value for specifying the next set of paginated results to return. After
    -- you receive a response with truncated results, use this parameter in a
    -- subsequent request. Set it to the value of @nextToken@ from the response
    -- that you just received.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The validation template that is used by the target signing platform.
    target :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned by this operation.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The category type of a signing platform.
    category :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSigningPlatforms' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'partner', 'listSigningPlatforms_partner' - Any partner entities connected to a signing platform.
--
-- 'nextToken', 'listSigningPlatforms_nextToken' - Value for specifying the next set of paginated results to return. After
-- you receive a response with truncated results, use this parameter in a
-- subsequent request. Set it to the value of @nextToken@ from the response
-- that you just received.
--
-- 'target', 'listSigningPlatforms_target' - The validation template that is used by the target signing platform.
--
-- 'maxResults', 'listSigningPlatforms_maxResults' - The maximum number of results to be returned by this operation.
--
-- 'category', 'listSigningPlatforms_category' - The category type of a signing platform.
newListSigningPlatforms ::
  ListSigningPlatforms
newListSigningPlatforms =
  ListSigningPlatforms'
    { partner = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      target = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      category = Prelude.Nothing
    }

-- | Any partner entities connected to a signing platform.
listSigningPlatforms_partner :: Lens.Lens' ListSigningPlatforms (Prelude.Maybe Prelude.Text)
listSigningPlatforms_partner = Lens.lens (\ListSigningPlatforms' {partner} -> partner) (\s@ListSigningPlatforms' {} a -> s {partner = a} :: ListSigningPlatforms)

-- | Value for specifying the next set of paginated results to return. After
-- you receive a response with truncated results, use this parameter in a
-- subsequent request. Set it to the value of @nextToken@ from the response
-- that you just received.
listSigningPlatforms_nextToken :: Lens.Lens' ListSigningPlatforms (Prelude.Maybe Prelude.Text)
listSigningPlatforms_nextToken = Lens.lens (\ListSigningPlatforms' {nextToken} -> nextToken) (\s@ListSigningPlatforms' {} a -> s {nextToken = a} :: ListSigningPlatforms)

-- | The validation template that is used by the target signing platform.
listSigningPlatforms_target :: Lens.Lens' ListSigningPlatforms (Prelude.Maybe Prelude.Text)
listSigningPlatforms_target = Lens.lens (\ListSigningPlatforms' {target} -> target) (\s@ListSigningPlatforms' {} a -> s {target = a} :: ListSigningPlatforms)

-- | The maximum number of results to be returned by this operation.
listSigningPlatforms_maxResults :: Lens.Lens' ListSigningPlatforms (Prelude.Maybe Prelude.Natural)
listSigningPlatforms_maxResults = Lens.lens (\ListSigningPlatforms' {maxResults} -> maxResults) (\s@ListSigningPlatforms' {} a -> s {maxResults = a} :: ListSigningPlatforms)

-- | The category type of a signing platform.
listSigningPlatforms_category :: Lens.Lens' ListSigningPlatforms (Prelude.Maybe Prelude.Text)
listSigningPlatforms_category = Lens.lens (\ListSigningPlatforms' {category} -> category) (\s@ListSigningPlatforms' {} a -> s {category = a} :: ListSigningPlatforms)

instance Core.AWSPager ListSigningPlatforms where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSigningPlatformsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSigningPlatformsResponse_platforms
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listSigningPlatforms_nextToken
          Lens..~ rs
          Lens.^? listSigningPlatformsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListSigningPlatforms where
  type
    AWSResponse ListSigningPlatforms =
      ListSigningPlatformsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSigningPlatformsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "platforms" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSigningPlatforms where
  hashWithSalt _salt ListSigningPlatforms' {..} =
    _salt `Prelude.hashWithSalt` partner
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` target
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` category

instance Prelude.NFData ListSigningPlatforms where
  rnf ListSigningPlatforms' {..} =
    Prelude.rnf partner
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf target
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf category

instance Data.ToHeaders ListSigningPlatforms where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListSigningPlatforms where
  toPath = Prelude.const "/signing-platforms"

instance Data.ToQuery ListSigningPlatforms where
  toQuery ListSigningPlatforms' {..} =
    Prelude.mconcat
      [ "partner" Data.=: partner,
        "nextToken" Data.=: nextToken,
        "target" Data.=: target,
        "maxResults" Data.=: maxResults,
        "category" Data.=: category
      ]

-- | /See:/ 'newListSigningPlatformsResponse' smart constructor.
data ListSigningPlatformsResponse = ListSigningPlatformsResponse'
  { -- | Value for specifying the next set of paginated results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of all platforms that match the request parameters.
    platforms :: Prelude.Maybe [SigningPlatform],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSigningPlatformsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSigningPlatformsResponse_nextToken' - Value for specifying the next set of paginated results to return.
--
-- 'platforms', 'listSigningPlatformsResponse_platforms' - A list of all platforms that match the request parameters.
--
-- 'httpStatus', 'listSigningPlatformsResponse_httpStatus' - The response's http status code.
newListSigningPlatformsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSigningPlatformsResponse
newListSigningPlatformsResponse pHttpStatus_ =
  ListSigningPlatformsResponse'
    { nextToken =
        Prelude.Nothing,
      platforms = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Value for specifying the next set of paginated results to return.
listSigningPlatformsResponse_nextToken :: Lens.Lens' ListSigningPlatformsResponse (Prelude.Maybe Prelude.Text)
listSigningPlatformsResponse_nextToken = Lens.lens (\ListSigningPlatformsResponse' {nextToken} -> nextToken) (\s@ListSigningPlatformsResponse' {} a -> s {nextToken = a} :: ListSigningPlatformsResponse)

-- | A list of all platforms that match the request parameters.
listSigningPlatformsResponse_platforms :: Lens.Lens' ListSigningPlatformsResponse (Prelude.Maybe [SigningPlatform])
listSigningPlatformsResponse_platforms = Lens.lens (\ListSigningPlatformsResponse' {platforms} -> platforms) (\s@ListSigningPlatformsResponse' {} a -> s {platforms = a} :: ListSigningPlatformsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSigningPlatformsResponse_httpStatus :: Lens.Lens' ListSigningPlatformsResponse Prelude.Int
listSigningPlatformsResponse_httpStatus = Lens.lens (\ListSigningPlatformsResponse' {httpStatus} -> httpStatus) (\s@ListSigningPlatformsResponse' {} a -> s {httpStatus = a} :: ListSigningPlatformsResponse)

instance Prelude.NFData ListSigningPlatformsResponse where
  rnf ListSigningPlatformsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf platforms
      `Prelude.seq` Prelude.rnf httpStatus
