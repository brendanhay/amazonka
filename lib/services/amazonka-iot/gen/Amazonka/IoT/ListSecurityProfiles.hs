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
-- Module      : Amazonka.IoT.ListSecurityProfiles
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Device Defender security profiles you\'ve created. You can
-- filter security profiles by dimension or custom metric.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListSecurityProfiles>
-- action.
--
-- @dimensionName@ and @metricName@ cannot be used in the same request.
--
-- This operation returns paginated results.
module Amazonka.IoT.ListSecurityProfiles
  ( -- * Creating a Request
    ListSecurityProfiles (..),
    newListSecurityProfiles,

    -- * Request Lenses
    listSecurityProfiles_nextToken,
    listSecurityProfiles_dimensionName,
    listSecurityProfiles_maxResults,
    listSecurityProfiles_metricName,

    -- * Destructuring the Response
    ListSecurityProfilesResponse (..),
    newListSecurityProfilesResponse,

    -- * Response Lenses
    listSecurityProfilesResponse_nextToken,
    listSecurityProfilesResponse_securityProfileIdentifiers,
    listSecurityProfilesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSecurityProfiles' smart constructor.
data ListSecurityProfiles = ListSecurityProfiles'
  { -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A filter to limit results to the security profiles that use the defined
    -- dimension. Cannot be used with @metricName@
    dimensionName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return at one time.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the custom metric. Cannot be used with @dimensionName@.
    metricName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSecurityProfiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSecurityProfiles_nextToken' - The token for the next set of results.
--
-- 'dimensionName', 'listSecurityProfiles_dimensionName' - A filter to limit results to the security profiles that use the defined
-- dimension. Cannot be used with @metricName@
--
-- 'maxResults', 'listSecurityProfiles_maxResults' - The maximum number of results to return at one time.
--
-- 'metricName', 'listSecurityProfiles_metricName' - The name of the custom metric. Cannot be used with @dimensionName@.
newListSecurityProfiles ::
  ListSecurityProfiles
newListSecurityProfiles =
  ListSecurityProfiles'
    { nextToken = Prelude.Nothing,
      dimensionName = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      metricName = Prelude.Nothing
    }

-- | The token for the next set of results.
listSecurityProfiles_nextToken :: Lens.Lens' ListSecurityProfiles (Prelude.Maybe Prelude.Text)
listSecurityProfiles_nextToken = Lens.lens (\ListSecurityProfiles' {nextToken} -> nextToken) (\s@ListSecurityProfiles' {} a -> s {nextToken = a} :: ListSecurityProfiles)

-- | A filter to limit results to the security profiles that use the defined
-- dimension. Cannot be used with @metricName@
listSecurityProfiles_dimensionName :: Lens.Lens' ListSecurityProfiles (Prelude.Maybe Prelude.Text)
listSecurityProfiles_dimensionName = Lens.lens (\ListSecurityProfiles' {dimensionName} -> dimensionName) (\s@ListSecurityProfiles' {} a -> s {dimensionName = a} :: ListSecurityProfiles)

-- | The maximum number of results to return at one time.
listSecurityProfiles_maxResults :: Lens.Lens' ListSecurityProfiles (Prelude.Maybe Prelude.Natural)
listSecurityProfiles_maxResults = Lens.lens (\ListSecurityProfiles' {maxResults} -> maxResults) (\s@ListSecurityProfiles' {} a -> s {maxResults = a} :: ListSecurityProfiles)

-- | The name of the custom metric. Cannot be used with @dimensionName@.
listSecurityProfiles_metricName :: Lens.Lens' ListSecurityProfiles (Prelude.Maybe Prelude.Text)
listSecurityProfiles_metricName = Lens.lens (\ListSecurityProfiles' {metricName} -> metricName) (\s@ListSecurityProfiles' {} a -> s {metricName = a} :: ListSecurityProfiles)

instance Core.AWSPager ListSecurityProfiles where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSecurityProfilesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSecurityProfilesResponse_securityProfileIdentifiers
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listSecurityProfiles_nextToken
          Lens..~ rs
          Lens.^? listSecurityProfilesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListSecurityProfiles where
  type
    AWSResponse ListSecurityProfiles =
      ListSecurityProfilesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSecurityProfilesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "securityProfileIdentifiers"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSecurityProfiles where
  hashWithSalt _salt ListSecurityProfiles' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` dimensionName
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` metricName

instance Prelude.NFData ListSecurityProfiles where
  rnf ListSecurityProfiles' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf dimensionName
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf metricName

instance Core.ToHeaders ListSecurityProfiles where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListSecurityProfiles where
  toPath = Prelude.const "/security-profiles"

instance Core.ToQuery ListSecurityProfiles where
  toQuery ListSecurityProfiles' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "dimensionName" Core.=: dimensionName,
        "maxResults" Core.=: maxResults,
        "metricName" Core.=: metricName
      ]

-- | /See:/ 'newListSecurityProfilesResponse' smart constructor.
data ListSecurityProfilesResponse = ListSecurityProfilesResponse'
  { -- | A token that can be used to retrieve the next set of results, or @null@
    -- if there are no additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of security profile identifiers (names and ARNs).
    securityProfileIdentifiers :: Prelude.Maybe [SecurityProfileIdentifier],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSecurityProfilesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSecurityProfilesResponse_nextToken' - A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
--
-- 'securityProfileIdentifiers', 'listSecurityProfilesResponse_securityProfileIdentifiers' - A list of security profile identifiers (names and ARNs).
--
-- 'httpStatus', 'listSecurityProfilesResponse_httpStatus' - The response's http status code.
newListSecurityProfilesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSecurityProfilesResponse
newListSecurityProfilesResponse pHttpStatus_ =
  ListSecurityProfilesResponse'
    { nextToken =
        Prelude.Nothing,
      securityProfileIdentifiers = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
listSecurityProfilesResponse_nextToken :: Lens.Lens' ListSecurityProfilesResponse (Prelude.Maybe Prelude.Text)
listSecurityProfilesResponse_nextToken = Lens.lens (\ListSecurityProfilesResponse' {nextToken} -> nextToken) (\s@ListSecurityProfilesResponse' {} a -> s {nextToken = a} :: ListSecurityProfilesResponse)

-- | A list of security profile identifiers (names and ARNs).
listSecurityProfilesResponse_securityProfileIdentifiers :: Lens.Lens' ListSecurityProfilesResponse (Prelude.Maybe [SecurityProfileIdentifier])
listSecurityProfilesResponse_securityProfileIdentifiers = Lens.lens (\ListSecurityProfilesResponse' {securityProfileIdentifiers} -> securityProfileIdentifiers) (\s@ListSecurityProfilesResponse' {} a -> s {securityProfileIdentifiers = a} :: ListSecurityProfilesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSecurityProfilesResponse_httpStatus :: Lens.Lens' ListSecurityProfilesResponse Prelude.Int
listSecurityProfilesResponse_httpStatus = Lens.lens (\ListSecurityProfilesResponse' {httpStatus} -> httpStatus) (\s@ListSecurityProfilesResponse' {} a -> s {httpStatus = a} :: ListSecurityProfilesResponse)

instance Prelude.NFData ListSecurityProfilesResponse where
  rnf ListSecurityProfilesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf securityProfileIdentifiers
      `Prelude.seq` Prelude.rnf httpStatus
