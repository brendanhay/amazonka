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
-- Module      : Amazonka.SecurityHub.ListSecurityControlDefinitions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the security controls that apply to a specified standard.
--
-- This operation returns paginated results.
module Amazonka.SecurityHub.ListSecurityControlDefinitions
  ( -- * Creating a Request
    ListSecurityControlDefinitions (..),
    newListSecurityControlDefinitions,

    -- * Request Lenses
    listSecurityControlDefinitions_maxResults,
    listSecurityControlDefinitions_nextToken,
    listSecurityControlDefinitions_standardsArn,

    -- * Destructuring the Response
    ListSecurityControlDefinitionsResponse (..),
    newListSecurityControlDefinitionsResponse,

    -- * Response Lenses
    listSecurityControlDefinitionsResponse_nextToken,
    listSecurityControlDefinitionsResponse_httpStatus,
    listSecurityControlDefinitionsResponse_securityControlDefinitions,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newListSecurityControlDefinitions' smart constructor.
data ListSecurityControlDefinitions = ListSecurityControlDefinitions'
  { -- | An optional parameter that limits the total results of the API response
    -- to the specified number. If this parameter isn\'t provided in the
    -- request, the results include the first 25 security controls that apply
    -- to the specified standard. The results also include a @NextToken@
    -- parameter that you can use in a subsequent API call to get the next 25
    -- controls. This repeats until all controls for the standard are returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Optional pagination parameter.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the standard that you want to view
    -- controls for.
    standardsArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSecurityControlDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listSecurityControlDefinitions_maxResults' - An optional parameter that limits the total results of the API response
-- to the specified number. If this parameter isn\'t provided in the
-- request, the results include the first 25 security controls that apply
-- to the specified standard. The results also include a @NextToken@
-- parameter that you can use in a subsequent API call to get the next 25
-- controls. This repeats until all controls for the standard are returned.
--
-- 'nextToken', 'listSecurityControlDefinitions_nextToken' - Optional pagination parameter.
--
-- 'standardsArn', 'listSecurityControlDefinitions_standardsArn' - The Amazon Resource Name (ARN) of the standard that you want to view
-- controls for.
newListSecurityControlDefinitions ::
  ListSecurityControlDefinitions
newListSecurityControlDefinitions =
  ListSecurityControlDefinitions'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      standardsArn = Prelude.Nothing
    }

-- | An optional parameter that limits the total results of the API response
-- to the specified number. If this parameter isn\'t provided in the
-- request, the results include the first 25 security controls that apply
-- to the specified standard. The results also include a @NextToken@
-- parameter that you can use in a subsequent API call to get the next 25
-- controls. This repeats until all controls for the standard are returned.
listSecurityControlDefinitions_maxResults :: Lens.Lens' ListSecurityControlDefinitions (Prelude.Maybe Prelude.Natural)
listSecurityControlDefinitions_maxResults = Lens.lens (\ListSecurityControlDefinitions' {maxResults} -> maxResults) (\s@ListSecurityControlDefinitions' {} a -> s {maxResults = a} :: ListSecurityControlDefinitions)

-- | Optional pagination parameter.
listSecurityControlDefinitions_nextToken :: Lens.Lens' ListSecurityControlDefinitions (Prelude.Maybe Prelude.Text)
listSecurityControlDefinitions_nextToken = Lens.lens (\ListSecurityControlDefinitions' {nextToken} -> nextToken) (\s@ListSecurityControlDefinitions' {} a -> s {nextToken = a} :: ListSecurityControlDefinitions)

-- | The Amazon Resource Name (ARN) of the standard that you want to view
-- controls for.
listSecurityControlDefinitions_standardsArn :: Lens.Lens' ListSecurityControlDefinitions (Prelude.Maybe Prelude.Text)
listSecurityControlDefinitions_standardsArn = Lens.lens (\ListSecurityControlDefinitions' {standardsArn} -> standardsArn) (\s@ListSecurityControlDefinitions' {} a -> s {standardsArn = a} :: ListSecurityControlDefinitions)

instance Core.AWSPager ListSecurityControlDefinitions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSecurityControlDefinitionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listSecurityControlDefinitionsResponse_securityControlDefinitions
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listSecurityControlDefinitions_nextToken
          Lens..~ rs
          Lens.^? listSecurityControlDefinitionsResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListSecurityControlDefinitions
  where
  type
    AWSResponse ListSecurityControlDefinitions =
      ListSecurityControlDefinitionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSecurityControlDefinitionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "SecurityControlDefinitions"
                            Core..!@ Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    ListSecurityControlDefinitions
  where
  hashWithSalt
    _salt
    ListSecurityControlDefinitions' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` standardsArn

instance
  Prelude.NFData
    ListSecurityControlDefinitions
  where
  rnf ListSecurityControlDefinitions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf standardsArn

instance
  Data.ToHeaders
    ListSecurityControlDefinitions
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListSecurityControlDefinitions where
  toPath =
    Prelude.const "/securityControls/definitions"

instance Data.ToQuery ListSecurityControlDefinitions where
  toQuery ListSecurityControlDefinitions' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        "StandardsArn" Data.=: standardsArn
      ]

-- | /See:/ 'newListSecurityControlDefinitionsResponse' smart constructor.
data ListSecurityControlDefinitionsResponse = ListSecurityControlDefinitionsResponse'
  { -- | A pagination parameter that\'s included in the response only if it was
    -- included in the request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of controls that apply to the specified standard.
    securityControlDefinitions :: [SecurityControlDefinition]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSecurityControlDefinitionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSecurityControlDefinitionsResponse_nextToken' - A pagination parameter that\'s included in the response only if it was
-- included in the request.
--
-- 'httpStatus', 'listSecurityControlDefinitionsResponse_httpStatus' - The response's http status code.
--
-- 'securityControlDefinitions', 'listSecurityControlDefinitionsResponse_securityControlDefinitions' - An array of controls that apply to the specified standard.
newListSecurityControlDefinitionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSecurityControlDefinitionsResponse
newListSecurityControlDefinitionsResponse
  pHttpStatus_ =
    ListSecurityControlDefinitionsResponse'
      { nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        securityControlDefinitions =
          Prelude.mempty
      }

-- | A pagination parameter that\'s included in the response only if it was
-- included in the request.
listSecurityControlDefinitionsResponse_nextToken :: Lens.Lens' ListSecurityControlDefinitionsResponse (Prelude.Maybe Prelude.Text)
listSecurityControlDefinitionsResponse_nextToken = Lens.lens (\ListSecurityControlDefinitionsResponse' {nextToken} -> nextToken) (\s@ListSecurityControlDefinitionsResponse' {} a -> s {nextToken = a} :: ListSecurityControlDefinitionsResponse)

-- | The response's http status code.
listSecurityControlDefinitionsResponse_httpStatus :: Lens.Lens' ListSecurityControlDefinitionsResponse Prelude.Int
listSecurityControlDefinitionsResponse_httpStatus = Lens.lens (\ListSecurityControlDefinitionsResponse' {httpStatus} -> httpStatus) (\s@ListSecurityControlDefinitionsResponse' {} a -> s {httpStatus = a} :: ListSecurityControlDefinitionsResponse)

-- | An array of controls that apply to the specified standard.
listSecurityControlDefinitionsResponse_securityControlDefinitions :: Lens.Lens' ListSecurityControlDefinitionsResponse [SecurityControlDefinition]
listSecurityControlDefinitionsResponse_securityControlDefinitions = Lens.lens (\ListSecurityControlDefinitionsResponse' {securityControlDefinitions} -> securityControlDefinitions) (\s@ListSecurityControlDefinitionsResponse' {} a -> s {securityControlDefinitions = a} :: ListSecurityControlDefinitionsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListSecurityControlDefinitionsResponse
  where
  rnf ListSecurityControlDefinitionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf securityControlDefinitions
