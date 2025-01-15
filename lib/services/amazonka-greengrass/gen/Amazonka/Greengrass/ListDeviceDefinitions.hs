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
-- Module      : Amazonka.Greengrass.ListDeviceDefinitions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of device definitions.
--
-- This operation returns paginated results.
module Amazonka.Greengrass.ListDeviceDefinitions
  ( -- * Creating a Request
    ListDeviceDefinitions (..),
    newListDeviceDefinitions,

    -- * Request Lenses
    listDeviceDefinitions_maxResults,
    listDeviceDefinitions_nextToken,

    -- * Destructuring the Response
    ListDeviceDefinitionsResponse (..),
    newListDeviceDefinitionsResponse,

    -- * Response Lenses
    listDeviceDefinitionsResponse_definitions,
    listDeviceDefinitionsResponse_nextToken,
    listDeviceDefinitionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDeviceDefinitions' smart constructor.
data ListDeviceDefinitions = ListDeviceDefinitions'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Text,
    -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDeviceDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listDeviceDefinitions_maxResults' - The maximum number of results to be returned per request.
--
-- 'nextToken', 'listDeviceDefinitions_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
newListDeviceDefinitions ::
  ListDeviceDefinitions
newListDeviceDefinitions =
  ListDeviceDefinitions'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to be returned per request.
listDeviceDefinitions_maxResults :: Lens.Lens' ListDeviceDefinitions (Prelude.Maybe Prelude.Text)
listDeviceDefinitions_maxResults = Lens.lens (\ListDeviceDefinitions' {maxResults} -> maxResults) (\s@ListDeviceDefinitions' {} a -> s {maxResults = a} :: ListDeviceDefinitions)

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listDeviceDefinitions_nextToken :: Lens.Lens' ListDeviceDefinitions (Prelude.Maybe Prelude.Text)
listDeviceDefinitions_nextToken = Lens.lens (\ListDeviceDefinitions' {nextToken} -> nextToken) (\s@ListDeviceDefinitions' {} a -> s {nextToken = a} :: ListDeviceDefinitions)

instance Core.AWSPager ListDeviceDefinitions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDeviceDefinitionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDeviceDefinitionsResponse_definitions
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listDeviceDefinitions_nextToken
              Lens..~ rs
              Lens.^? listDeviceDefinitionsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListDeviceDefinitions where
  type
    AWSResponse ListDeviceDefinitions =
      ListDeviceDefinitionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeviceDefinitionsResponse'
            Prelude.<$> (x Data..?> "Definitions" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDeviceDefinitions where
  hashWithSalt _salt ListDeviceDefinitions' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListDeviceDefinitions where
  rnf ListDeviceDefinitions' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken

instance Data.ToHeaders ListDeviceDefinitions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListDeviceDefinitions where
  toPath =
    Prelude.const "/greengrass/definition/devices"

instance Data.ToQuery ListDeviceDefinitions where
  toQuery ListDeviceDefinitions' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListDeviceDefinitionsResponse' smart constructor.
data ListDeviceDefinitionsResponse = ListDeviceDefinitionsResponse'
  { -- | Information about a definition.
    definitions :: Prelude.Maybe [DefinitionInformation],
    -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDeviceDefinitionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'definitions', 'listDeviceDefinitionsResponse_definitions' - Information about a definition.
--
-- 'nextToken', 'listDeviceDefinitionsResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'httpStatus', 'listDeviceDefinitionsResponse_httpStatus' - The response's http status code.
newListDeviceDefinitionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDeviceDefinitionsResponse
newListDeviceDefinitionsResponse pHttpStatus_ =
  ListDeviceDefinitionsResponse'
    { definitions =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about a definition.
listDeviceDefinitionsResponse_definitions :: Lens.Lens' ListDeviceDefinitionsResponse (Prelude.Maybe [DefinitionInformation])
listDeviceDefinitionsResponse_definitions = Lens.lens (\ListDeviceDefinitionsResponse' {definitions} -> definitions) (\s@ListDeviceDefinitionsResponse' {} a -> s {definitions = a} :: ListDeviceDefinitionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listDeviceDefinitionsResponse_nextToken :: Lens.Lens' ListDeviceDefinitionsResponse (Prelude.Maybe Prelude.Text)
listDeviceDefinitionsResponse_nextToken = Lens.lens (\ListDeviceDefinitionsResponse' {nextToken} -> nextToken) (\s@ListDeviceDefinitionsResponse' {} a -> s {nextToken = a} :: ListDeviceDefinitionsResponse)

-- | The response's http status code.
listDeviceDefinitionsResponse_httpStatus :: Lens.Lens' ListDeviceDefinitionsResponse Prelude.Int
listDeviceDefinitionsResponse_httpStatus = Lens.lens (\ListDeviceDefinitionsResponse' {httpStatus} -> httpStatus) (\s@ListDeviceDefinitionsResponse' {} a -> s {httpStatus = a} :: ListDeviceDefinitionsResponse)

instance Prelude.NFData ListDeviceDefinitionsResponse where
  rnf ListDeviceDefinitionsResponse' {..} =
    Prelude.rnf definitions `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
