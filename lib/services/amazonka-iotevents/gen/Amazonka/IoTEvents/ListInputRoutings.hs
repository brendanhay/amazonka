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
-- Module      : Amazonka.IoTEvents.ListInputRoutings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists one or more input routings.
module Amazonka.IoTEvents.ListInputRoutings
  ( -- * Creating a Request
    ListInputRoutings (..),
    newListInputRoutings,

    -- * Request Lenses
    listInputRoutings_maxResults,
    listInputRoutings_nextToken,
    listInputRoutings_inputIdentifier,

    -- * Destructuring the Response
    ListInputRoutingsResponse (..),
    newListInputRoutingsResponse,

    -- * Response Lenses
    listInputRoutingsResponse_nextToken,
    listInputRoutingsResponse_routedResources,
    listInputRoutingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEvents.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListInputRoutings' smart constructor.
data ListInputRoutings = ListInputRoutings'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token that you can use to return the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifer of the routed input.
    inputIdentifier :: InputIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInputRoutings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listInputRoutings_maxResults' - The maximum number of results to be returned per request.
--
-- 'nextToken', 'listInputRoutings_nextToken' - The token that you can use to return the next set of results.
--
-- 'inputIdentifier', 'listInputRoutings_inputIdentifier' - The identifer of the routed input.
newListInputRoutings ::
  -- | 'inputIdentifier'
  InputIdentifier ->
  ListInputRoutings
newListInputRoutings pInputIdentifier_ =
  ListInputRoutings'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      inputIdentifier = pInputIdentifier_
    }

-- | The maximum number of results to be returned per request.
listInputRoutings_maxResults :: Lens.Lens' ListInputRoutings (Prelude.Maybe Prelude.Natural)
listInputRoutings_maxResults = Lens.lens (\ListInputRoutings' {maxResults} -> maxResults) (\s@ListInputRoutings' {} a -> s {maxResults = a} :: ListInputRoutings)

-- | The token that you can use to return the next set of results.
listInputRoutings_nextToken :: Lens.Lens' ListInputRoutings (Prelude.Maybe Prelude.Text)
listInputRoutings_nextToken = Lens.lens (\ListInputRoutings' {nextToken} -> nextToken) (\s@ListInputRoutings' {} a -> s {nextToken = a} :: ListInputRoutings)

-- | The identifer of the routed input.
listInputRoutings_inputIdentifier :: Lens.Lens' ListInputRoutings InputIdentifier
listInputRoutings_inputIdentifier = Lens.lens (\ListInputRoutings' {inputIdentifier} -> inputIdentifier) (\s@ListInputRoutings' {} a -> s {inputIdentifier = a} :: ListInputRoutings)

instance Core.AWSRequest ListInputRoutings where
  type
    AWSResponse ListInputRoutings =
      ListInputRoutingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInputRoutingsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x
                            Data..?> "routedResources"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListInputRoutings where
  hashWithSalt _salt ListInputRoutings' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` inputIdentifier

instance Prelude.NFData ListInputRoutings where
  rnf ListInputRoutings' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf inputIdentifier

instance Data.ToHeaders ListInputRoutings where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON ListInputRoutings where
  toJSON ListInputRoutings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("inputIdentifier" Data..= inputIdentifier)
          ]
      )

instance Data.ToPath ListInputRoutings where
  toPath = Prelude.const "/input-routings"

instance Data.ToQuery ListInputRoutings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListInputRoutingsResponse' smart constructor.
data ListInputRoutingsResponse = ListInputRoutingsResponse'
  { -- | The token that you can use to return the next set of results, or @null@
    -- if there are no more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Summary information about the routed resources.
    routedResources :: Prelude.Maybe [RoutedResource],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInputRoutingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listInputRoutingsResponse_nextToken' - The token that you can use to return the next set of results, or @null@
-- if there are no more results.
--
-- 'routedResources', 'listInputRoutingsResponse_routedResources' - Summary information about the routed resources.
--
-- 'httpStatus', 'listInputRoutingsResponse_httpStatus' - The response's http status code.
newListInputRoutingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListInputRoutingsResponse
newListInputRoutingsResponse pHttpStatus_ =
  ListInputRoutingsResponse'
    { nextToken =
        Prelude.Nothing,
      routedResources = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token that you can use to return the next set of results, or @null@
-- if there are no more results.
listInputRoutingsResponse_nextToken :: Lens.Lens' ListInputRoutingsResponse (Prelude.Maybe Prelude.Text)
listInputRoutingsResponse_nextToken = Lens.lens (\ListInputRoutingsResponse' {nextToken} -> nextToken) (\s@ListInputRoutingsResponse' {} a -> s {nextToken = a} :: ListInputRoutingsResponse)

-- | Summary information about the routed resources.
listInputRoutingsResponse_routedResources :: Lens.Lens' ListInputRoutingsResponse (Prelude.Maybe [RoutedResource])
listInputRoutingsResponse_routedResources = Lens.lens (\ListInputRoutingsResponse' {routedResources} -> routedResources) (\s@ListInputRoutingsResponse' {} a -> s {routedResources = a} :: ListInputRoutingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listInputRoutingsResponse_httpStatus :: Lens.Lens' ListInputRoutingsResponse Prelude.Int
listInputRoutingsResponse_httpStatus = Lens.lens (\ListInputRoutingsResponse' {httpStatus} -> httpStatus) (\s@ListInputRoutingsResponse' {} a -> s {httpStatus = a} :: ListInputRoutingsResponse)

instance Prelude.NFData ListInputRoutingsResponse where
  rnf ListInputRoutingsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf routedResources `Prelude.seq`
        Prelude.rnf httpStatus
