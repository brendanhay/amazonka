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
-- Module      : Amazonka.Glue.GetSecurityConfigurations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of all security configurations.
--
-- This operation returns paginated results.
module Amazonka.Glue.GetSecurityConfigurations
  ( -- * Creating a Request
    GetSecurityConfigurations (..),
    newGetSecurityConfigurations,

    -- * Request Lenses
    getSecurityConfigurations_maxResults,
    getSecurityConfigurations_nextToken,

    -- * Destructuring the Response
    GetSecurityConfigurationsResponse (..),
    newGetSecurityConfigurationsResponse,

    -- * Response Lenses
    getSecurityConfigurationsResponse_nextToken,
    getSecurityConfigurationsResponse_securityConfigurations,
    getSecurityConfigurationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSecurityConfigurations' smart constructor.
data GetSecurityConfigurations = GetSecurityConfigurations'
  { -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A continuation token, if this is a continuation call.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSecurityConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getSecurityConfigurations_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'getSecurityConfigurations_nextToken' - A continuation token, if this is a continuation call.
newGetSecurityConfigurations ::
  GetSecurityConfigurations
newGetSecurityConfigurations =
  GetSecurityConfigurations'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return.
getSecurityConfigurations_maxResults :: Lens.Lens' GetSecurityConfigurations (Prelude.Maybe Prelude.Natural)
getSecurityConfigurations_maxResults = Lens.lens (\GetSecurityConfigurations' {maxResults} -> maxResults) (\s@GetSecurityConfigurations' {} a -> s {maxResults = a} :: GetSecurityConfigurations)

-- | A continuation token, if this is a continuation call.
getSecurityConfigurations_nextToken :: Lens.Lens' GetSecurityConfigurations (Prelude.Maybe Prelude.Text)
getSecurityConfigurations_nextToken = Lens.lens (\GetSecurityConfigurations' {nextToken} -> nextToken) (\s@GetSecurityConfigurations' {} a -> s {nextToken = a} :: GetSecurityConfigurations)

instance Core.AWSPager GetSecurityConfigurations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getSecurityConfigurationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getSecurityConfigurationsResponse_securityConfigurations
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& getSecurityConfigurations_nextToken
          Lens..~ rs
          Lens.^? getSecurityConfigurationsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest GetSecurityConfigurations where
  type
    AWSResponse GetSecurityConfigurations =
      GetSecurityConfigurationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSecurityConfigurationsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "SecurityConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSecurityConfigurations where
  hashWithSalt _salt GetSecurityConfigurations' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData GetSecurityConfigurations where
  rnf GetSecurityConfigurations' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders GetSecurityConfigurations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.GetSecurityConfigurations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetSecurityConfigurations where
  toJSON GetSecurityConfigurations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath GetSecurityConfigurations where
  toPath = Prelude.const "/"

instance Data.ToQuery GetSecurityConfigurations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSecurityConfigurationsResponse' smart constructor.
data GetSecurityConfigurationsResponse = GetSecurityConfigurationsResponse'
  { -- | A continuation token, if there are more security configurations to
    -- return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of security configurations.
    securityConfigurations :: Prelude.Maybe [SecurityConfiguration],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSecurityConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getSecurityConfigurationsResponse_nextToken' - A continuation token, if there are more security configurations to
-- return.
--
-- 'securityConfigurations', 'getSecurityConfigurationsResponse_securityConfigurations' - A list of security configurations.
--
-- 'httpStatus', 'getSecurityConfigurationsResponse_httpStatus' - The response's http status code.
newGetSecurityConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSecurityConfigurationsResponse
newGetSecurityConfigurationsResponse pHttpStatus_ =
  GetSecurityConfigurationsResponse'
    { nextToken =
        Prelude.Nothing,
      securityConfigurations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token, if there are more security configurations to
-- return.
getSecurityConfigurationsResponse_nextToken :: Lens.Lens' GetSecurityConfigurationsResponse (Prelude.Maybe Prelude.Text)
getSecurityConfigurationsResponse_nextToken = Lens.lens (\GetSecurityConfigurationsResponse' {nextToken} -> nextToken) (\s@GetSecurityConfigurationsResponse' {} a -> s {nextToken = a} :: GetSecurityConfigurationsResponse)

-- | A list of security configurations.
getSecurityConfigurationsResponse_securityConfigurations :: Lens.Lens' GetSecurityConfigurationsResponse (Prelude.Maybe [SecurityConfiguration])
getSecurityConfigurationsResponse_securityConfigurations = Lens.lens (\GetSecurityConfigurationsResponse' {securityConfigurations} -> securityConfigurations) (\s@GetSecurityConfigurationsResponse' {} a -> s {securityConfigurations = a} :: GetSecurityConfigurationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getSecurityConfigurationsResponse_httpStatus :: Lens.Lens' GetSecurityConfigurationsResponse Prelude.Int
getSecurityConfigurationsResponse_httpStatus = Lens.lens (\GetSecurityConfigurationsResponse' {httpStatus} -> httpStatus) (\s@GetSecurityConfigurationsResponse' {} a -> s {httpStatus = a} :: GetSecurityConfigurationsResponse)

instance
  Prelude.NFData
    GetSecurityConfigurationsResponse
  where
  rnf GetSecurityConfigurationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf securityConfigurations
      `Prelude.seq` Prelude.rnf httpStatus
