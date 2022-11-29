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
-- Module      : Amazonka.EMR.ListSecurityConfigurations
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.EMR.ListSecurityConfigurations
  ( -- * Creating a Request
    ListSecurityConfigurations (..),
    newListSecurityConfigurations,

    -- * Request Lenses
    listSecurityConfigurations_marker,

    -- * Destructuring the Response
    ListSecurityConfigurationsResponse (..),
    newListSecurityConfigurationsResponse,

    -- * Response Lenses
    listSecurityConfigurationsResponse_marker,
    listSecurityConfigurationsResponse_securityConfigurations,
    listSecurityConfigurationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSecurityConfigurations' smart constructor.
data ListSecurityConfigurations = ListSecurityConfigurations'
  { -- | The pagination token that indicates the set of results to retrieve.
    marker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  ListSecurityConfigurations'
    { marker =
        Prelude.Nothing
    }

-- | The pagination token that indicates the set of results to retrieve.
listSecurityConfigurations_marker :: Lens.Lens' ListSecurityConfigurations (Prelude.Maybe Prelude.Text)
listSecurityConfigurations_marker = Lens.lens (\ListSecurityConfigurations' {marker} -> marker) (\s@ListSecurityConfigurations' {} a -> s {marker = a} :: ListSecurityConfigurations)

instance Core.AWSPager ListSecurityConfigurations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSecurityConfigurationsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSecurityConfigurationsResponse_securityConfigurations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listSecurityConfigurations_marker
          Lens..~ rs
          Lens.^? listSecurityConfigurationsResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest ListSecurityConfigurations where
  type
    AWSResponse ListSecurityConfigurations =
      ListSecurityConfigurationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSecurityConfigurationsResponse'
            Prelude.<$> (x Core..?> "Marker")
            Prelude.<*> ( x Core..?> "SecurityConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSecurityConfigurations where
  hashWithSalt _salt ListSecurityConfigurations' {..} =
    _salt `Prelude.hashWithSalt` marker

instance Prelude.NFData ListSecurityConfigurations where
  rnf ListSecurityConfigurations' {..} =
    Prelude.rnf marker

instance Core.ToHeaders ListSecurityConfigurations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ElasticMapReduce.ListSecurityConfigurations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListSecurityConfigurations where
  toJSON ListSecurityConfigurations' {..} =
    Core.object
      ( Prelude.catMaybes
          [("Marker" Core..=) Prelude.<$> marker]
      )

instance Core.ToPath ListSecurityConfigurations where
  toPath = Prelude.const "/"

instance Core.ToQuery ListSecurityConfigurations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSecurityConfigurationsResponse' smart constructor.
data ListSecurityConfigurationsResponse = ListSecurityConfigurationsResponse'
  { -- | A pagination token that indicates the next set of results to retrieve.
    -- Include the marker in the next ListSecurityConfiguration call to
    -- retrieve the next page of results, if required.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The creation date and time, and name, of each security configuration.
    securityConfigurations :: Prelude.Maybe [SecurityConfigurationSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSecurityConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listSecurityConfigurationsResponse_marker' - A pagination token that indicates the next set of results to retrieve.
-- Include the marker in the next ListSecurityConfiguration call to
-- retrieve the next page of results, if required.
--
-- 'securityConfigurations', 'listSecurityConfigurationsResponse_securityConfigurations' - The creation date and time, and name, of each security configuration.
--
-- 'httpStatus', 'listSecurityConfigurationsResponse_httpStatus' - The response's http status code.
newListSecurityConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSecurityConfigurationsResponse
newListSecurityConfigurationsResponse pHttpStatus_ =
  ListSecurityConfigurationsResponse'
    { marker =
        Prelude.Nothing,
      securityConfigurations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A pagination token that indicates the next set of results to retrieve.
-- Include the marker in the next ListSecurityConfiguration call to
-- retrieve the next page of results, if required.
listSecurityConfigurationsResponse_marker :: Lens.Lens' ListSecurityConfigurationsResponse (Prelude.Maybe Prelude.Text)
listSecurityConfigurationsResponse_marker = Lens.lens (\ListSecurityConfigurationsResponse' {marker} -> marker) (\s@ListSecurityConfigurationsResponse' {} a -> s {marker = a} :: ListSecurityConfigurationsResponse)

-- | The creation date and time, and name, of each security configuration.
listSecurityConfigurationsResponse_securityConfigurations :: Lens.Lens' ListSecurityConfigurationsResponse (Prelude.Maybe [SecurityConfigurationSummary])
listSecurityConfigurationsResponse_securityConfigurations = Lens.lens (\ListSecurityConfigurationsResponse' {securityConfigurations} -> securityConfigurations) (\s@ListSecurityConfigurationsResponse' {} a -> s {securityConfigurations = a} :: ListSecurityConfigurationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSecurityConfigurationsResponse_httpStatus :: Lens.Lens' ListSecurityConfigurationsResponse Prelude.Int
listSecurityConfigurationsResponse_httpStatus = Lens.lens (\ListSecurityConfigurationsResponse' {httpStatus} -> httpStatus) (\s@ListSecurityConfigurationsResponse' {} a -> s {httpStatus = a} :: ListSecurityConfigurationsResponse)

instance
  Prelude.NFData
    ListSecurityConfigurationsResponse
  where
  rnf ListSecurityConfigurationsResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf securityConfigurations
      `Prelude.seq` Prelude.rnf httpStatus
