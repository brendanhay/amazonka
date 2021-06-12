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
-- Module      : Network.AWS.IoT.ListDomainConfigurations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of domain configurations for the user. This list is sorted
-- alphabetically by domain configuration name.
--
-- The domain configuration feature is in public preview and is subject to
-- change.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListDomainConfigurations
  ( -- * Creating a Request
    ListDomainConfigurations (..),
    newListDomainConfigurations,

    -- * Request Lenses
    listDomainConfigurations_pageSize,
    listDomainConfigurations_serviceType,
    listDomainConfigurations_marker,

    -- * Destructuring the Response
    ListDomainConfigurationsResponse (..),
    newListDomainConfigurationsResponse,

    -- * Response Lenses
    listDomainConfigurationsResponse_domainConfigurations,
    listDomainConfigurationsResponse_nextMarker,
    listDomainConfigurationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDomainConfigurations' smart constructor.
data ListDomainConfigurations = ListDomainConfigurations'
  { -- | The result page size.
    pageSize :: Core.Maybe Core.Natural,
    -- | The type of service delivered by the endpoint.
    serviceType :: Core.Maybe ServiceType,
    -- | The marker for the next set of results.
    marker :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDomainConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'listDomainConfigurations_pageSize' - The result page size.
--
-- 'serviceType', 'listDomainConfigurations_serviceType' - The type of service delivered by the endpoint.
--
-- 'marker', 'listDomainConfigurations_marker' - The marker for the next set of results.
newListDomainConfigurations ::
  ListDomainConfigurations
newListDomainConfigurations =
  ListDomainConfigurations'
    { pageSize = Core.Nothing,
      serviceType = Core.Nothing,
      marker = Core.Nothing
    }

-- | The result page size.
listDomainConfigurations_pageSize :: Lens.Lens' ListDomainConfigurations (Core.Maybe Core.Natural)
listDomainConfigurations_pageSize = Lens.lens (\ListDomainConfigurations' {pageSize} -> pageSize) (\s@ListDomainConfigurations' {} a -> s {pageSize = a} :: ListDomainConfigurations)

-- | The type of service delivered by the endpoint.
listDomainConfigurations_serviceType :: Lens.Lens' ListDomainConfigurations (Core.Maybe ServiceType)
listDomainConfigurations_serviceType = Lens.lens (\ListDomainConfigurations' {serviceType} -> serviceType) (\s@ListDomainConfigurations' {} a -> s {serviceType = a} :: ListDomainConfigurations)

-- | The marker for the next set of results.
listDomainConfigurations_marker :: Lens.Lens' ListDomainConfigurations (Core.Maybe Core.Text)
listDomainConfigurations_marker = Lens.lens (\ListDomainConfigurations' {marker} -> marker) (\s@ListDomainConfigurations' {} a -> s {marker = a} :: ListDomainConfigurations)

instance Core.AWSPager ListDomainConfigurations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDomainConfigurationsResponse_nextMarker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listDomainConfigurationsResponse_domainConfigurations
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listDomainConfigurations_marker
          Lens..~ rs
          Lens.^? listDomainConfigurationsResponse_nextMarker
            Core.. Lens._Just

instance Core.AWSRequest ListDomainConfigurations where
  type
    AWSResponse ListDomainConfigurations =
      ListDomainConfigurationsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDomainConfigurationsResponse'
            Core.<$> ( x Core..?> "domainConfigurations"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "nextMarker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListDomainConfigurations

instance Core.NFData ListDomainConfigurations

instance Core.ToHeaders ListDomainConfigurations where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListDomainConfigurations where
  toPath = Core.const "/domainConfigurations"

instance Core.ToQuery ListDomainConfigurations where
  toQuery ListDomainConfigurations' {..} =
    Core.mconcat
      [ "pageSize" Core.=: pageSize,
        "serviceType" Core.=: serviceType,
        "marker" Core.=: marker
      ]

-- | /See:/ 'newListDomainConfigurationsResponse' smart constructor.
data ListDomainConfigurationsResponse = ListDomainConfigurationsResponse'
  { -- | A list of objects that contain summary information about the user\'s
    -- domain configurations.
    domainConfigurations :: Core.Maybe [DomainConfigurationSummary],
    -- | The marker for the next set of results.
    nextMarker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDomainConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainConfigurations', 'listDomainConfigurationsResponse_domainConfigurations' - A list of objects that contain summary information about the user\'s
-- domain configurations.
--
-- 'nextMarker', 'listDomainConfigurationsResponse_nextMarker' - The marker for the next set of results.
--
-- 'httpStatus', 'listDomainConfigurationsResponse_httpStatus' - The response's http status code.
newListDomainConfigurationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListDomainConfigurationsResponse
newListDomainConfigurationsResponse pHttpStatus_ =
  ListDomainConfigurationsResponse'
    { domainConfigurations =
        Core.Nothing,
      nextMarker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of objects that contain summary information about the user\'s
-- domain configurations.
listDomainConfigurationsResponse_domainConfigurations :: Lens.Lens' ListDomainConfigurationsResponse (Core.Maybe [DomainConfigurationSummary])
listDomainConfigurationsResponse_domainConfigurations = Lens.lens (\ListDomainConfigurationsResponse' {domainConfigurations} -> domainConfigurations) (\s@ListDomainConfigurationsResponse' {} a -> s {domainConfigurations = a} :: ListDomainConfigurationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The marker for the next set of results.
listDomainConfigurationsResponse_nextMarker :: Lens.Lens' ListDomainConfigurationsResponse (Core.Maybe Core.Text)
listDomainConfigurationsResponse_nextMarker = Lens.lens (\ListDomainConfigurationsResponse' {nextMarker} -> nextMarker) (\s@ListDomainConfigurationsResponse' {} a -> s {nextMarker = a} :: ListDomainConfigurationsResponse)

-- | The response's http status code.
listDomainConfigurationsResponse_httpStatus :: Lens.Lens' ListDomainConfigurationsResponse Core.Int
listDomainConfigurationsResponse_httpStatus = Lens.lens (\ListDomainConfigurationsResponse' {httpStatus} -> httpStatus) (\s@ListDomainConfigurationsResponse' {} a -> s {httpStatus = a} :: ListDomainConfigurationsResponse)

instance Core.NFData ListDomainConfigurationsResponse
