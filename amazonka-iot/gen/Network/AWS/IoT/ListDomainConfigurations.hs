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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDomainConfigurations' smart constructor.
data ListDomainConfigurations = ListDomainConfigurations'
  { -- | The result page size.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | The type of service delivered by the endpoint.
    serviceType :: Prelude.Maybe ServiceType,
    -- | The marker for the next set of results.
    marker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { pageSize =
        Prelude.Nothing,
      serviceType = Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | The result page size.
listDomainConfigurations_pageSize :: Lens.Lens' ListDomainConfigurations (Prelude.Maybe Prelude.Natural)
listDomainConfigurations_pageSize = Lens.lens (\ListDomainConfigurations' {pageSize} -> pageSize) (\s@ListDomainConfigurations' {} a -> s {pageSize = a} :: ListDomainConfigurations)

-- | The type of service delivered by the endpoint.
listDomainConfigurations_serviceType :: Lens.Lens' ListDomainConfigurations (Prelude.Maybe ServiceType)
listDomainConfigurations_serviceType = Lens.lens (\ListDomainConfigurations' {serviceType} -> serviceType) (\s@ListDomainConfigurations' {} a -> s {serviceType = a} :: ListDomainConfigurations)

-- | The marker for the next set of results.
listDomainConfigurations_marker :: Lens.Lens' ListDomainConfigurations (Prelude.Maybe Prelude.Text)
listDomainConfigurations_marker = Lens.lens (\ListDomainConfigurations' {marker} -> marker) (\s@ListDomainConfigurations' {} a -> s {marker = a} :: ListDomainConfigurations)

instance Core.AWSPager ListDomainConfigurations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDomainConfigurationsResponse_nextMarker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDomainConfigurationsResponse_domainConfigurations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDomainConfigurations_marker
          Lens..~ rs
          Lens.^? listDomainConfigurationsResponse_nextMarker
            Prelude.. Lens._Just

instance Core.AWSRequest ListDomainConfigurations where
  type
    AWSResponse ListDomainConfigurations =
      ListDomainConfigurationsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDomainConfigurationsResponse'
            Prelude.<$> ( x Core..?> "domainConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "nextMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDomainConfigurations

instance Prelude.NFData ListDomainConfigurations

instance Core.ToHeaders ListDomainConfigurations where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListDomainConfigurations where
  toPath = Prelude.const "/domainConfigurations"

instance Core.ToQuery ListDomainConfigurations where
  toQuery ListDomainConfigurations' {..} =
    Prelude.mconcat
      [ "pageSize" Core.=: pageSize,
        "serviceType" Core.=: serviceType,
        "marker" Core.=: marker
      ]

-- | /See:/ 'newListDomainConfigurationsResponse' smart constructor.
data ListDomainConfigurationsResponse = ListDomainConfigurationsResponse'
  { -- | A list of objects that contain summary information about the user\'s
    -- domain configurations.
    domainConfigurations :: Prelude.Maybe [DomainConfigurationSummary],
    -- | The marker for the next set of results.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListDomainConfigurationsResponse
newListDomainConfigurationsResponse pHttpStatus_ =
  ListDomainConfigurationsResponse'
    { domainConfigurations =
        Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of objects that contain summary information about the user\'s
-- domain configurations.
listDomainConfigurationsResponse_domainConfigurations :: Lens.Lens' ListDomainConfigurationsResponse (Prelude.Maybe [DomainConfigurationSummary])
listDomainConfigurationsResponse_domainConfigurations = Lens.lens (\ListDomainConfigurationsResponse' {domainConfigurations} -> domainConfigurations) (\s@ListDomainConfigurationsResponse' {} a -> s {domainConfigurations = a} :: ListDomainConfigurationsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The marker for the next set of results.
listDomainConfigurationsResponse_nextMarker :: Lens.Lens' ListDomainConfigurationsResponse (Prelude.Maybe Prelude.Text)
listDomainConfigurationsResponse_nextMarker = Lens.lens (\ListDomainConfigurationsResponse' {nextMarker} -> nextMarker) (\s@ListDomainConfigurationsResponse' {} a -> s {nextMarker = a} :: ListDomainConfigurationsResponse)

-- | The response's http status code.
listDomainConfigurationsResponse_httpStatus :: Lens.Lens' ListDomainConfigurationsResponse Prelude.Int
listDomainConfigurationsResponse_httpStatus = Lens.lens (\ListDomainConfigurationsResponse' {httpStatus} -> httpStatus) (\s@ListDomainConfigurationsResponse' {} a -> s {httpStatus = a} :: ListDomainConfigurationsResponse)

instance
  Prelude.NFData
    ListDomainConfigurationsResponse
