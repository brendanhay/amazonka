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
-- Module      : Amazonka.OpenSearchServerless.ListSecurityConfigs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about configured OpenSearch Serverless security
-- configurations. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/serverless-saml.html SAML authentication for Amazon OpenSearch Serverless>.
module Amazonka.OpenSearchServerless.ListSecurityConfigs
  ( -- * Creating a Request
    ListSecurityConfigs (..),
    newListSecurityConfigs,

    -- * Request Lenses
    listSecurityConfigs_maxResults,
    listSecurityConfigs_nextToken,
    listSecurityConfigs_type,

    -- * Destructuring the Response
    ListSecurityConfigsResponse (..),
    newListSecurityConfigsResponse,

    -- * Response Lenses
    listSecurityConfigsResponse_nextToken,
    listSecurityConfigsResponse_securityConfigSummaries,
    listSecurityConfigsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSecurityConfigs' smart constructor.
data ListSecurityConfigs = ListSecurityConfigs'
  { -- | An optional parameter that specifies the maximum number of results to
    -- return. You can use @nextToken@ to get the next page of results. The
    -- default is 20.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If your initial @ListSecurityConfigs@ operation returns a @nextToken@,
    -- you can include the returned @nextToken@ in subsequent
    -- @ListSecurityConfigs@ operations, which returns results in the next
    -- page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The type of security configuration.
    type' :: SecurityConfigType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSecurityConfigs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listSecurityConfigs_maxResults' - An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to get the next page of results. The
-- default is 20.
--
-- 'nextToken', 'listSecurityConfigs_nextToken' - If your initial @ListSecurityConfigs@ operation returns a @nextToken@,
-- you can include the returned @nextToken@ in subsequent
-- @ListSecurityConfigs@ operations, which returns results in the next
-- page.
--
-- 'type'', 'listSecurityConfigs_type' - The type of security configuration.
newListSecurityConfigs ::
  -- | 'type''
  SecurityConfigType ->
  ListSecurityConfigs
newListSecurityConfigs pType_ =
  ListSecurityConfigs'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      type' = pType_
    }

-- | An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to get the next page of results. The
-- default is 20.
listSecurityConfigs_maxResults :: Lens.Lens' ListSecurityConfigs (Prelude.Maybe Prelude.Natural)
listSecurityConfigs_maxResults = Lens.lens (\ListSecurityConfigs' {maxResults} -> maxResults) (\s@ListSecurityConfigs' {} a -> s {maxResults = a} :: ListSecurityConfigs)

-- | If your initial @ListSecurityConfigs@ operation returns a @nextToken@,
-- you can include the returned @nextToken@ in subsequent
-- @ListSecurityConfigs@ operations, which returns results in the next
-- page.
listSecurityConfigs_nextToken :: Lens.Lens' ListSecurityConfigs (Prelude.Maybe Prelude.Text)
listSecurityConfigs_nextToken = Lens.lens (\ListSecurityConfigs' {nextToken} -> nextToken) (\s@ListSecurityConfigs' {} a -> s {nextToken = a} :: ListSecurityConfigs)

-- | The type of security configuration.
listSecurityConfigs_type :: Lens.Lens' ListSecurityConfigs SecurityConfigType
listSecurityConfigs_type = Lens.lens (\ListSecurityConfigs' {type'} -> type') (\s@ListSecurityConfigs' {} a -> s {type' = a} :: ListSecurityConfigs)

instance Core.AWSRequest ListSecurityConfigs where
  type
    AWSResponse ListSecurityConfigs =
      ListSecurityConfigsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSecurityConfigsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x Data..?> "securityConfigSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSecurityConfigs where
  hashWithSalt _salt ListSecurityConfigs' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ListSecurityConfigs where
  rnf ListSecurityConfigs' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders ListSecurityConfigs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpenSearchServerless.ListSecurityConfigs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListSecurityConfigs where
  toJSON ListSecurityConfigs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("type" Data..= type')
          ]
      )

instance Data.ToPath ListSecurityConfigs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListSecurityConfigs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSecurityConfigsResponse' smart constructor.
data ListSecurityConfigsResponse = ListSecurityConfigsResponse'
  { -- | When @nextToken@ is returned, there are more results available. The
    -- value of @nextToken@ is a unique pagination token for each page. Make
    -- the call again using the returned token to retrieve the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Details about the security configurations in your account.
    securityConfigSummaries :: Prelude.Maybe [SecurityConfigSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSecurityConfigsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSecurityConfigsResponse_nextToken' - When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
--
-- 'securityConfigSummaries', 'listSecurityConfigsResponse_securityConfigSummaries' - Details about the security configurations in your account.
--
-- 'httpStatus', 'listSecurityConfigsResponse_httpStatus' - The response's http status code.
newListSecurityConfigsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSecurityConfigsResponse
newListSecurityConfigsResponse pHttpStatus_ =
  ListSecurityConfigsResponse'
    { nextToken =
        Prelude.Nothing,
      securityConfigSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
listSecurityConfigsResponse_nextToken :: Lens.Lens' ListSecurityConfigsResponse (Prelude.Maybe Prelude.Text)
listSecurityConfigsResponse_nextToken = Lens.lens (\ListSecurityConfigsResponse' {nextToken} -> nextToken) (\s@ListSecurityConfigsResponse' {} a -> s {nextToken = a} :: ListSecurityConfigsResponse)

-- | Details about the security configurations in your account.
listSecurityConfigsResponse_securityConfigSummaries :: Lens.Lens' ListSecurityConfigsResponse (Prelude.Maybe [SecurityConfigSummary])
listSecurityConfigsResponse_securityConfigSummaries = Lens.lens (\ListSecurityConfigsResponse' {securityConfigSummaries} -> securityConfigSummaries) (\s@ListSecurityConfigsResponse' {} a -> s {securityConfigSummaries = a} :: ListSecurityConfigsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSecurityConfigsResponse_httpStatus :: Lens.Lens' ListSecurityConfigsResponse Prelude.Int
listSecurityConfigsResponse_httpStatus = Lens.lens (\ListSecurityConfigsResponse' {httpStatus} -> httpStatus) (\s@ListSecurityConfigsResponse' {} a -> s {httpStatus = a} :: ListSecurityConfigsResponse)

instance Prelude.NFData ListSecurityConfigsResponse where
  rnf ListSecurityConfigsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf securityConfigSummaries
      `Prelude.seq` Prelude.rnf httpStatus
