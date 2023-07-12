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
-- Module      : Amazonka.Braket.SearchDevices
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches for devices using the specified filters.
--
-- This operation returns paginated results.
module Amazonka.Braket.SearchDevices
  ( -- * Creating a Request
    SearchDevices (..),
    newSearchDevices,

    -- * Request Lenses
    searchDevices_maxResults,
    searchDevices_nextToken,
    searchDevices_filters,

    -- * Destructuring the Response
    SearchDevicesResponse (..),
    newSearchDevicesResponse,

    -- * Response Lenses
    searchDevicesResponse_nextToken,
    searchDevicesResponse_httpStatus,
    searchDevicesResponse_devices,
  )
where

import Amazonka.Braket.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchDevices' smart constructor.
data SearchDevices = SearchDevices'
  { -- | The maximum number of results to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token used for pagination of results returned in the response. Use the
    -- token returned from the previous request continue results where the
    -- previous request ended.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The filter values to use to search for a device.
    filters :: [SearchDevicesFilter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchDevices' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'searchDevices_maxResults' - The maximum number of results to return in the response.
--
-- 'nextToken', 'searchDevices_nextToken' - A token used for pagination of results returned in the response. Use the
-- token returned from the previous request continue results where the
-- previous request ended.
--
-- 'filters', 'searchDevices_filters' - The filter values to use to search for a device.
newSearchDevices ::
  SearchDevices
newSearchDevices =
  SearchDevices'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      filters = Prelude.mempty
    }

-- | The maximum number of results to return in the response.
searchDevices_maxResults :: Lens.Lens' SearchDevices (Prelude.Maybe Prelude.Natural)
searchDevices_maxResults = Lens.lens (\SearchDevices' {maxResults} -> maxResults) (\s@SearchDevices' {} a -> s {maxResults = a} :: SearchDevices)

-- | A token used for pagination of results returned in the response. Use the
-- token returned from the previous request continue results where the
-- previous request ended.
searchDevices_nextToken :: Lens.Lens' SearchDevices (Prelude.Maybe Prelude.Text)
searchDevices_nextToken = Lens.lens (\SearchDevices' {nextToken} -> nextToken) (\s@SearchDevices' {} a -> s {nextToken = a} :: SearchDevices)

-- | The filter values to use to search for a device.
searchDevices_filters :: Lens.Lens' SearchDevices [SearchDevicesFilter]
searchDevices_filters = Lens.lens (\SearchDevices' {filters} -> filters) (\s@SearchDevices' {} a -> s {filters = a} :: SearchDevices) Prelude.. Lens.coerced

instance Core.AWSPager SearchDevices where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchDevicesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. searchDevicesResponse_devices) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& searchDevices_nextToken
          Lens..~ rs
          Lens.^? searchDevicesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest SearchDevices where
  type
    AWSResponse SearchDevices =
      SearchDevicesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchDevicesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "devices" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable SearchDevices where
  hashWithSalt _salt SearchDevices' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters

instance Prelude.NFData SearchDevices where
  rnf SearchDevices' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters

instance Data.ToHeaders SearchDevices where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchDevices where
  toJSON SearchDevices' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("filters" Data..= filters)
          ]
      )

instance Data.ToPath SearchDevices where
  toPath = Prelude.const "/devices"

instance Data.ToQuery SearchDevices where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchDevicesResponse' smart constructor.
data SearchDevicesResponse = SearchDevicesResponse'
  { -- | A token used for pagination of results, or null if there are no
    -- additional results. Use the token value in a subsequent request to
    -- continue results where the previous request ended.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of @DeviceSummary@ objects for devices that match the specified
    -- filter values.
    devices :: [DeviceSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchDevicesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchDevicesResponse_nextToken' - A token used for pagination of results, or null if there are no
-- additional results. Use the token value in a subsequent request to
-- continue results where the previous request ended.
--
-- 'httpStatus', 'searchDevicesResponse_httpStatus' - The response's http status code.
--
-- 'devices', 'searchDevicesResponse_devices' - An array of @DeviceSummary@ objects for devices that match the specified
-- filter values.
newSearchDevicesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchDevicesResponse
newSearchDevicesResponse pHttpStatus_ =
  SearchDevicesResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      devices = Prelude.mempty
    }

-- | A token used for pagination of results, or null if there are no
-- additional results. Use the token value in a subsequent request to
-- continue results where the previous request ended.
searchDevicesResponse_nextToken :: Lens.Lens' SearchDevicesResponse (Prelude.Maybe Prelude.Text)
searchDevicesResponse_nextToken = Lens.lens (\SearchDevicesResponse' {nextToken} -> nextToken) (\s@SearchDevicesResponse' {} a -> s {nextToken = a} :: SearchDevicesResponse)

-- | The response's http status code.
searchDevicesResponse_httpStatus :: Lens.Lens' SearchDevicesResponse Prelude.Int
searchDevicesResponse_httpStatus = Lens.lens (\SearchDevicesResponse' {httpStatus} -> httpStatus) (\s@SearchDevicesResponse' {} a -> s {httpStatus = a} :: SearchDevicesResponse)

-- | An array of @DeviceSummary@ objects for devices that match the specified
-- filter values.
searchDevicesResponse_devices :: Lens.Lens' SearchDevicesResponse [DeviceSummary]
searchDevicesResponse_devices = Lens.lens (\SearchDevicesResponse' {devices} -> devices) (\s@SearchDevicesResponse' {} a -> s {devices = a} :: SearchDevicesResponse) Prelude.. Lens.coerced

instance Prelude.NFData SearchDevicesResponse where
  rnf SearchDevicesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf devices
