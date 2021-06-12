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
-- Module      : Network.AWS.IoT.ListSecurityProfiles
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Device Defender security profiles you\'ve created. You can
-- filter security profiles by dimension or custom metric.
--
-- @dimensionName@ and @metricName@ cannot be used in the same request.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListSecurityProfiles
  ( -- * Creating a Request
    ListSecurityProfiles (..),
    newListSecurityProfiles,

    -- * Request Lenses
    listSecurityProfiles_dimensionName,
    listSecurityProfiles_nextToken,
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

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListSecurityProfiles' smart constructor.
data ListSecurityProfiles = ListSecurityProfiles'
  { -- | A filter to limit results to the security profiles that use the defined
    -- dimension. Cannot be used with @metricName@
    dimensionName :: Core.Maybe Core.Text,
    -- | The token for the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return at one time.
    maxResults :: Core.Maybe Core.Natural,
    -- | The name of the custom metric. Cannot be used with @dimensionName@.
    metricName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListSecurityProfiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimensionName', 'listSecurityProfiles_dimensionName' - A filter to limit results to the security profiles that use the defined
-- dimension. Cannot be used with @metricName@
--
-- 'nextToken', 'listSecurityProfiles_nextToken' - The token for the next set of results.
--
-- 'maxResults', 'listSecurityProfiles_maxResults' - The maximum number of results to return at one time.
--
-- 'metricName', 'listSecurityProfiles_metricName' - The name of the custom metric. Cannot be used with @dimensionName@.
newListSecurityProfiles ::
  ListSecurityProfiles
newListSecurityProfiles =
  ListSecurityProfiles'
    { dimensionName = Core.Nothing,
      nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      metricName = Core.Nothing
    }

-- | A filter to limit results to the security profiles that use the defined
-- dimension. Cannot be used with @metricName@
listSecurityProfiles_dimensionName :: Lens.Lens' ListSecurityProfiles (Core.Maybe Core.Text)
listSecurityProfiles_dimensionName = Lens.lens (\ListSecurityProfiles' {dimensionName} -> dimensionName) (\s@ListSecurityProfiles' {} a -> s {dimensionName = a} :: ListSecurityProfiles)

-- | The token for the next set of results.
listSecurityProfiles_nextToken :: Lens.Lens' ListSecurityProfiles (Core.Maybe Core.Text)
listSecurityProfiles_nextToken = Lens.lens (\ListSecurityProfiles' {nextToken} -> nextToken) (\s@ListSecurityProfiles' {} a -> s {nextToken = a} :: ListSecurityProfiles)

-- | The maximum number of results to return at one time.
listSecurityProfiles_maxResults :: Lens.Lens' ListSecurityProfiles (Core.Maybe Core.Natural)
listSecurityProfiles_maxResults = Lens.lens (\ListSecurityProfiles' {maxResults} -> maxResults) (\s@ListSecurityProfiles' {} a -> s {maxResults = a} :: ListSecurityProfiles)

-- | The name of the custom metric. Cannot be used with @dimensionName@.
listSecurityProfiles_metricName :: Lens.Lens' ListSecurityProfiles (Core.Maybe Core.Text)
listSecurityProfiles_metricName = Lens.lens (\ListSecurityProfiles' {metricName} -> metricName) (\s@ListSecurityProfiles' {} a -> s {metricName = a} :: ListSecurityProfiles)

instance Core.AWSPager ListSecurityProfiles where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSecurityProfilesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listSecurityProfilesResponse_securityProfileIdentifiers
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listSecurityProfiles_nextToken
          Lens..~ rs
          Lens.^? listSecurityProfilesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListSecurityProfiles where
  type
    AWSResponse ListSecurityProfiles =
      ListSecurityProfilesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSecurityProfilesResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> ( x Core..?> "securityProfileIdentifiers"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListSecurityProfiles

instance Core.NFData ListSecurityProfiles

instance Core.ToHeaders ListSecurityProfiles where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListSecurityProfiles where
  toPath = Core.const "/security-profiles"

instance Core.ToQuery ListSecurityProfiles where
  toQuery ListSecurityProfiles' {..} =
    Core.mconcat
      [ "dimensionName" Core.=: dimensionName,
        "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "metricName" Core.=: metricName
      ]

-- | /See:/ 'newListSecurityProfilesResponse' smart constructor.
data ListSecurityProfilesResponse = ListSecurityProfilesResponse'
  { -- | A token that can be used to retrieve the next set of results, or @null@
    -- if there are no additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of security profile identifiers (names and ARNs).
    securityProfileIdentifiers :: Core.Maybe [SecurityProfileIdentifier],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListSecurityProfilesResponse
newListSecurityProfilesResponse pHttpStatus_ =
  ListSecurityProfilesResponse'
    { nextToken =
        Core.Nothing,
      securityProfileIdentifiers = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
listSecurityProfilesResponse_nextToken :: Lens.Lens' ListSecurityProfilesResponse (Core.Maybe Core.Text)
listSecurityProfilesResponse_nextToken = Lens.lens (\ListSecurityProfilesResponse' {nextToken} -> nextToken) (\s@ListSecurityProfilesResponse' {} a -> s {nextToken = a} :: ListSecurityProfilesResponse)

-- | A list of security profile identifiers (names and ARNs).
listSecurityProfilesResponse_securityProfileIdentifiers :: Lens.Lens' ListSecurityProfilesResponse (Core.Maybe [SecurityProfileIdentifier])
listSecurityProfilesResponse_securityProfileIdentifiers = Lens.lens (\ListSecurityProfilesResponse' {securityProfileIdentifiers} -> securityProfileIdentifiers) (\s@ListSecurityProfilesResponse' {} a -> s {securityProfileIdentifiers = a} :: ListSecurityProfilesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listSecurityProfilesResponse_httpStatus :: Lens.Lens' ListSecurityProfilesResponse Core.Int
listSecurityProfilesResponse_httpStatus = Lens.lens (\ListSecurityProfilesResponse' {httpStatus} -> httpStatus) (\s@ListSecurityProfilesResponse' {} a -> s {httpStatus = a} :: ListSecurityProfilesResponse)

instance Core.NFData ListSecurityProfilesResponse
