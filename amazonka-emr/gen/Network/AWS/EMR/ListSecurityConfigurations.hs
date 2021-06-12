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
-- Module      : Network.AWS.EMR.ListSecurityConfigurations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the security configurations visible to this account, providing
-- their creation dates and times, and their names. This call returns a
-- maximum of 50 clusters per call, but returns a marker to track the
-- paging of the cluster list across multiple ListSecurityConfigurations
-- calls.
--
-- This operation returns paginated results.
module Network.AWS.EMR.ListSecurityConfigurations
  ( -- * Creating a Request
    ListSecurityConfigurations (..),
    newListSecurityConfigurations,

    -- * Request Lenses
    listSecurityConfigurations_marker,

    -- * Destructuring the Response
    ListSecurityConfigurationsResponse (..),
    newListSecurityConfigurationsResponse,

    -- * Response Lenses
    listSecurityConfigurationsResponse_securityConfigurations,
    listSecurityConfigurationsResponse_marker,
    listSecurityConfigurationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListSecurityConfigurations' smart constructor.
data ListSecurityConfigurations = ListSecurityConfigurations'
  { -- | The pagination token that indicates the set of results to retrieve.
    marker :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListSecurityConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listSecurityConfigurations_marker' - The pagination token that indicates the set of results to retrieve.
newListSecurityConfigurations ::
  ListSecurityConfigurations
newListSecurityConfigurations =
  ListSecurityConfigurations' {marker = Core.Nothing}

-- | The pagination token that indicates the set of results to retrieve.
listSecurityConfigurations_marker :: Lens.Lens' ListSecurityConfigurations (Core.Maybe Core.Text)
listSecurityConfigurations_marker = Lens.lens (\ListSecurityConfigurations' {marker} -> marker) (\s@ListSecurityConfigurations' {} a -> s {marker = a} :: ListSecurityConfigurations)

instance Core.AWSPager ListSecurityConfigurations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSecurityConfigurationsResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listSecurityConfigurationsResponse_securityConfigurations
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listSecurityConfigurations_marker
          Lens..~ rs
          Lens.^? listSecurityConfigurationsResponse_marker
            Core.. Lens._Just

instance Core.AWSRequest ListSecurityConfigurations where
  type
    AWSResponse ListSecurityConfigurations =
      ListSecurityConfigurationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSecurityConfigurationsResponse'
            Core.<$> ( x Core..?> "SecurityConfigurations"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListSecurityConfigurations

instance Core.NFData ListSecurityConfigurations

instance Core.ToHeaders ListSecurityConfigurations where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ElasticMapReduce.ListSecurityConfigurations" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListSecurityConfigurations where
  toJSON ListSecurityConfigurations' {..} =
    Core.object
      (Core.catMaybes [("Marker" Core..=) Core.<$> marker])

instance Core.ToPath ListSecurityConfigurations where
  toPath = Core.const "/"

instance Core.ToQuery ListSecurityConfigurations where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListSecurityConfigurationsResponse' smart constructor.
data ListSecurityConfigurationsResponse = ListSecurityConfigurationsResponse'
  { -- | The creation date and time, and name, of each security configuration.
    securityConfigurations :: Core.Maybe [SecurityConfigurationSummary],
    -- | A pagination token that indicates the next set of results to retrieve.
    -- Include the marker in the next ListSecurityConfiguration call to
    -- retrieve the next page of results, if required.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListSecurityConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityConfigurations', 'listSecurityConfigurationsResponse_securityConfigurations' - The creation date and time, and name, of each security configuration.
--
-- 'marker', 'listSecurityConfigurationsResponse_marker' - A pagination token that indicates the next set of results to retrieve.
-- Include the marker in the next ListSecurityConfiguration call to
-- retrieve the next page of results, if required.
--
-- 'httpStatus', 'listSecurityConfigurationsResponse_httpStatus' - The response's http status code.
newListSecurityConfigurationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListSecurityConfigurationsResponse
newListSecurityConfigurationsResponse pHttpStatus_ =
  ListSecurityConfigurationsResponse'
    { securityConfigurations =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The creation date and time, and name, of each security configuration.
listSecurityConfigurationsResponse_securityConfigurations :: Lens.Lens' ListSecurityConfigurationsResponse (Core.Maybe [SecurityConfigurationSummary])
listSecurityConfigurationsResponse_securityConfigurations = Lens.lens (\ListSecurityConfigurationsResponse' {securityConfigurations} -> securityConfigurations) (\s@ListSecurityConfigurationsResponse' {} a -> s {securityConfigurations = a} :: ListSecurityConfigurationsResponse) Core.. Lens.mapping Lens._Coerce

-- | A pagination token that indicates the next set of results to retrieve.
-- Include the marker in the next ListSecurityConfiguration call to
-- retrieve the next page of results, if required.
listSecurityConfigurationsResponse_marker :: Lens.Lens' ListSecurityConfigurationsResponse (Core.Maybe Core.Text)
listSecurityConfigurationsResponse_marker = Lens.lens (\ListSecurityConfigurationsResponse' {marker} -> marker) (\s@ListSecurityConfigurationsResponse' {} a -> s {marker = a} :: ListSecurityConfigurationsResponse)

-- | The response's http status code.
listSecurityConfigurationsResponse_httpStatus :: Lens.Lens' ListSecurityConfigurationsResponse Core.Int
listSecurityConfigurationsResponse_httpStatus = Lens.lens (\ListSecurityConfigurationsResponse' {httpStatus} -> httpStatus) (\s@ListSecurityConfigurationsResponse' {} a -> s {httpStatus = a} :: ListSecurityConfigurationsResponse)

instance
  Core.NFData
    ListSecurityConfigurationsResponse
