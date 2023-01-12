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
-- Module      : Amazonka.PrivateNetworks.ListDeviceIdentifiers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists device identifiers. Add filters to your request to return a more
-- specific list of results. Use filters to match the Amazon Resource Name
-- (ARN) of an order, the status of device identifiers, or the ARN of the
-- traffic group.
--
-- >  <p>If you specify multiple filters, filters are joined with an OR, and the request
--
-- returns results that match all of the specified filters.
--
-- This operation returns paginated results.
module Amazonka.PrivateNetworks.ListDeviceIdentifiers
  ( -- * Creating a Request
    ListDeviceIdentifiers (..),
    newListDeviceIdentifiers,

    -- * Request Lenses
    listDeviceIdentifiers_filters,
    listDeviceIdentifiers_maxResults,
    listDeviceIdentifiers_startToken,
    listDeviceIdentifiers_networkArn,

    -- * Destructuring the Response
    ListDeviceIdentifiersResponse (..),
    newListDeviceIdentifiersResponse,

    -- * Response Lenses
    listDeviceIdentifiersResponse_deviceIdentifiers,
    listDeviceIdentifiersResponse_nextToken,
    listDeviceIdentifiersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.PrivateNetworks.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDeviceIdentifiers' smart constructor.
data ListDeviceIdentifiers = ListDeviceIdentifiers'
  { -- | The filters.
    --
    -- -   @ORDER@ - The Amazon Resource Name (ARN) of the order.
    --
    -- -   @STATUS@ - The status (@ACTIVE@ | @INACTIVE@).
    --
    -- -   @TRAFFIC_GROUP@ - The Amazon Resource Name (ARN) of the traffic
    --     group.
    --
    -- Filter values are case sensitive. If you specify multiple values for a
    -- filter, the values are joined with an @OR@, and the request returns all
    -- results that match any of the specified values.
    filters :: Prelude.Maybe (Prelude.HashMap DeviceIdentifierFilterKeys [Prelude.Text]),
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    startToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the network.
    networkArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDeviceIdentifiers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listDeviceIdentifiers_filters' - The filters.
--
-- -   @ORDER@ - The Amazon Resource Name (ARN) of the order.
--
-- -   @STATUS@ - The status (@ACTIVE@ | @INACTIVE@).
--
-- -   @TRAFFIC_GROUP@ - The Amazon Resource Name (ARN) of the traffic
--     group.
--
-- Filter values are case sensitive. If you specify multiple values for a
-- filter, the values are joined with an @OR@, and the request returns all
-- results that match any of the specified values.
--
-- 'maxResults', 'listDeviceIdentifiers_maxResults' - The maximum number of results to return.
--
-- 'startToken', 'listDeviceIdentifiers_startToken' - The token for the next page of results.
--
-- 'networkArn', 'listDeviceIdentifiers_networkArn' - The Amazon Resource Name (ARN) of the network.
newListDeviceIdentifiers ::
  -- | 'networkArn'
  Prelude.Text ->
  ListDeviceIdentifiers
newListDeviceIdentifiers pNetworkArn_ =
  ListDeviceIdentifiers'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      startToken = Prelude.Nothing,
      networkArn = pNetworkArn_
    }

-- | The filters.
--
-- -   @ORDER@ - The Amazon Resource Name (ARN) of the order.
--
-- -   @STATUS@ - The status (@ACTIVE@ | @INACTIVE@).
--
-- -   @TRAFFIC_GROUP@ - The Amazon Resource Name (ARN) of the traffic
--     group.
--
-- Filter values are case sensitive. If you specify multiple values for a
-- filter, the values are joined with an @OR@, and the request returns all
-- results that match any of the specified values.
listDeviceIdentifiers_filters :: Lens.Lens' ListDeviceIdentifiers (Prelude.Maybe (Prelude.HashMap DeviceIdentifierFilterKeys [Prelude.Text]))
listDeviceIdentifiers_filters = Lens.lens (\ListDeviceIdentifiers' {filters} -> filters) (\s@ListDeviceIdentifiers' {} a -> s {filters = a} :: ListDeviceIdentifiers) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return.
listDeviceIdentifiers_maxResults :: Lens.Lens' ListDeviceIdentifiers (Prelude.Maybe Prelude.Natural)
listDeviceIdentifiers_maxResults = Lens.lens (\ListDeviceIdentifiers' {maxResults} -> maxResults) (\s@ListDeviceIdentifiers' {} a -> s {maxResults = a} :: ListDeviceIdentifiers)

-- | The token for the next page of results.
listDeviceIdentifiers_startToken :: Lens.Lens' ListDeviceIdentifiers (Prelude.Maybe Prelude.Text)
listDeviceIdentifiers_startToken = Lens.lens (\ListDeviceIdentifiers' {startToken} -> startToken) (\s@ListDeviceIdentifiers' {} a -> s {startToken = a} :: ListDeviceIdentifiers)

-- | The Amazon Resource Name (ARN) of the network.
listDeviceIdentifiers_networkArn :: Lens.Lens' ListDeviceIdentifiers Prelude.Text
listDeviceIdentifiers_networkArn = Lens.lens (\ListDeviceIdentifiers' {networkArn} -> networkArn) (\s@ListDeviceIdentifiers' {} a -> s {networkArn = a} :: ListDeviceIdentifiers)

instance Core.AWSPager ListDeviceIdentifiers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDeviceIdentifiersResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDeviceIdentifiersResponse_deviceIdentifiers
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDeviceIdentifiers_startToken
          Lens..~ rs
          Lens.^? listDeviceIdentifiersResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListDeviceIdentifiers where
  type
    AWSResponse ListDeviceIdentifiers =
      ListDeviceIdentifiersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeviceIdentifiersResponse'
            Prelude.<$> ( x Data..?> "deviceIdentifiers"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDeviceIdentifiers where
  hashWithSalt _salt ListDeviceIdentifiers' {..} =
    _salt `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` startToken
      `Prelude.hashWithSalt` networkArn

instance Prelude.NFData ListDeviceIdentifiers where
  rnf ListDeviceIdentifiers' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf startToken
      `Prelude.seq` Prelude.rnf networkArn

instance Data.ToHeaders ListDeviceIdentifiers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDeviceIdentifiers where
  toJSON ListDeviceIdentifiers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filters" Data..=) Prelude.<$> filters,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("startToken" Data..=) Prelude.<$> startToken,
            Prelude.Just ("networkArn" Data..= networkArn)
          ]
      )

instance Data.ToPath ListDeviceIdentifiers where
  toPath = Prelude.const "/v1/device-identifiers/list"

instance Data.ToQuery ListDeviceIdentifiers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDeviceIdentifiersResponse' smart constructor.
data ListDeviceIdentifiersResponse = ListDeviceIdentifiersResponse'
  { -- | Information about the device identifiers.
    deviceIdentifiers :: Prelude.Maybe [DeviceIdentifier],
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDeviceIdentifiersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceIdentifiers', 'listDeviceIdentifiersResponse_deviceIdentifiers' - Information about the device identifiers.
--
-- 'nextToken', 'listDeviceIdentifiersResponse_nextToken' - The token for the next page of results.
--
-- 'httpStatus', 'listDeviceIdentifiersResponse_httpStatus' - The response's http status code.
newListDeviceIdentifiersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDeviceIdentifiersResponse
newListDeviceIdentifiersResponse pHttpStatus_ =
  ListDeviceIdentifiersResponse'
    { deviceIdentifiers =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the device identifiers.
listDeviceIdentifiersResponse_deviceIdentifiers :: Lens.Lens' ListDeviceIdentifiersResponse (Prelude.Maybe [DeviceIdentifier])
listDeviceIdentifiersResponse_deviceIdentifiers = Lens.lens (\ListDeviceIdentifiersResponse' {deviceIdentifiers} -> deviceIdentifiers) (\s@ListDeviceIdentifiersResponse' {} a -> s {deviceIdentifiers = a} :: ListDeviceIdentifiersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next page of results.
listDeviceIdentifiersResponse_nextToken :: Lens.Lens' ListDeviceIdentifiersResponse (Prelude.Maybe Prelude.Text)
listDeviceIdentifiersResponse_nextToken = Lens.lens (\ListDeviceIdentifiersResponse' {nextToken} -> nextToken) (\s@ListDeviceIdentifiersResponse' {} a -> s {nextToken = a} :: ListDeviceIdentifiersResponse)

-- | The response's http status code.
listDeviceIdentifiersResponse_httpStatus :: Lens.Lens' ListDeviceIdentifiersResponse Prelude.Int
listDeviceIdentifiersResponse_httpStatus = Lens.lens (\ListDeviceIdentifiersResponse' {httpStatus} -> httpStatus) (\s@ListDeviceIdentifiersResponse' {} a -> s {httpStatus = a} :: ListDeviceIdentifiersResponse)

instance Prelude.NFData ListDeviceIdentifiersResponse where
  rnf ListDeviceIdentifiersResponse' {..} =
    Prelude.rnf deviceIdentifiers
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
