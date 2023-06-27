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
-- Module      : Amazonka.GreengrassV2.ListClientDevicesAssociatedWithCoreDevice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a paginated list of client devices that are associated with a
-- core device.
--
-- This operation returns paginated results.
module Amazonka.GreengrassV2.ListClientDevicesAssociatedWithCoreDevice
  ( -- * Creating a Request
    ListClientDevicesAssociatedWithCoreDevice (..),
    newListClientDevicesAssociatedWithCoreDevice,

    -- * Request Lenses
    listClientDevicesAssociatedWithCoreDevice_maxResults,
    listClientDevicesAssociatedWithCoreDevice_nextToken,
    listClientDevicesAssociatedWithCoreDevice_coreDeviceThingName,

    -- * Destructuring the Response
    ListClientDevicesAssociatedWithCoreDeviceResponse (..),
    newListClientDevicesAssociatedWithCoreDeviceResponse,

    -- * Response Lenses
    listClientDevicesAssociatedWithCoreDeviceResponse_associatedClientDevices,
    listClientDevicesAssociatedWithCoreDeviceResponse_nextToken,
    listClientDevicesAssociatedWithCoreDeviceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GreengrassV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListClientDevicesAssociatedWithCoreDevice' smart constructor.
data ListClientDevicesAssociatedWithCoreDevice = ListClientDevicesAssociatedWithCoreDevice'
  { -- | The maximum number of results to be returned per paginated request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to be used for the next set of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the core device. This is also the name of the IoT thing.
    coreDeviceThingName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListClientDevicesAssociatedWithCoreDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listClientDevicesAssociatedWithCoreDevice_maxResults' - The maximum number of results to be returned per paginated request.
--
-- 'nextToken', 'listClientDevicesAssociatedWithCoreDevice_nextToken' - The token to be used for the next set of paginated results.
--
-- 'coreDeviceThingName', 'listClientDevicesAssociatedWithCoreDevice_coreDeviceThingName' - The name of the core device. This is also the name of the IoT thing.
newListClientDevicesAssociatedWithCoreDevice ::
  -- | 'coreDeviceThingName'
  Prelude.Text ->
  ListClientDevicesAssociatedWithCoreDevice
newListClientDevicesAssociatedWithCoreDevice
  pCoreDeviceThingName_ =
    ListClientDevicesAssociatedWithCoreDevice'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        coreDeviceThingName =
          pCoreDeviceThingName_
      }

-- | The maximum number of results to be returned per paginated request.
listClientDevicesAssociatedWithCoreDevice_maxResults :: Lens.Lens' ListClientDevicesAssociatedWithCoreDevice (Prelude.Maybe Prelude.Natural)
listClientDevicesAssociatedWithCoreDevice_maxResults = Lens.lens (\ListClientDevicesAssociatedWithCoreDevice' {maxResults} -> maxResults) (\s@ListClientDevicesAssociatedWithCoreDevice' {} a -> s {maxResults = a} :: ListClientDevicesAssociatedWithCoreDevice)

-- | The token to be used for the next set of paginated results.
listClientDevicesAssociatedWithCoreDevice_nextToken :: Lens.Lens' ListClientDevicesAssociatedWithCoreDevice (Prelude.Maybe Prelude.Text)
listClientDevicesAssociatedWithCoreDevice_nextToken = Lens.lens (\ListClientDevicesAssociatedWithCoreDevice' {nextToken} -> nextToken) (\s@ListClientDevicesAssociatedWithCoreDevice' {} a -> s {nextToken = a} :: ListClientDevicesAssociatedWithCoreDevice)

-- | The name of the core device. This is also the name of the IoT thing.
listClientDevicesAssociatedWithCoreDevice_coreDeviceThingName :: Lens.Lens' ListClientDevicesAssociatedWithCoreDevice Prelude.Text
listClientDevicesAssociatedWithCoreDevice_coreDeviceThingName = Lens.lens (\ListClientDevicesAssociatedWithCoreDevice' {coreDeviceThingName} -> coreDeviceThingName) (\s@ListClientDevicesAssociatedWithCoreDevice' {} a -> s {coreDeviceThingName = a} :: ListClientDevicesAssociatedWithCoreDevice)

instance
  Core.AWSPager
    ListClientDevicesAssociatedWithCoreDevice
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listClientDevicesAssociatedWithCoreDeviceResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listClientDevicesAssociatedWithCoreDeviceResponse_associatedClientDevices
            Prelude.. Lens._Just
            Prelude.. Lens.to Prelude.toList
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listClientDevicesAssociatedWithCoreDevice_nextToken
          Lens..~ rs
          Lens.^? listClientDevicesAssociatedWithCoreDeviceResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListClientDevicesAssociatedWithCoreDevice
  where
  type
    AWSResponse
      ListClientDevicesAssociatedWithCoreDevice =
      ListClientDevicesAssociatedWithCoreDeviceResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListClientDevicesAssociatedWithCoreDeviceResponse'
            Prelude.<$> (x Data..?> "associatedClientDevices")
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListClientDevicesAssociatedWithCoreDevice
  where
  hashWithSalt
    _salt
    ListClientDevicesAssociatedWithCoreDevice' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` coreDeviceThingName

instance
  Prelude.NFData
    ListClientDevicesAssociatedWithCoreDevice
  where
  rnf ListClientDevicesAssociatedWithCoreDevice' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf coreDeviceThingName

instance
  Data.ToHeaders
    ListClientDevicesAssociatedWithCoreDevice
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    ListClientDevicesAssociatedWithCoreDevice
  where
  toPath ListClientDevicesAssociatedWithCoreDevice' {..} =
    Prelude.mconcat
      [ "/greengrass/v2/coreDevices/",
        Data.toBS coreDeviceThingName,
        "/associatedClientDevices"
      ]

instance
  Data.ToQuery
    ListClientDevicesAssociatedWithCoreDevice
  where
  toQuery
    ListClientDevicesAssociatedWithCoreDevice' {..} =
      Prelude.mconcat
        [ "maxResults" Data.=: maxResults,
          "nextToken" Data.=: nextToken
        ]

-- | /See:/ 'newListClientDevicesAssociatedWithCoreDeviceResponse' smart constructor.
data ListClientDevicesAssociatedWithCoreDeviceResponse = ListClientDevicesAssociatedWithCoreDeviceResponse'
  { -- | A list that describes the client devices that are associated with the
    -- core device.
    associatedClientDevices :: Prelude.Maybe (Prelude.NonEmpty AssociatedClientDevice),
    -- | The token for the next set of results, or null if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListClientDevicesAssociatedWithCoreDeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associatedClientDevices', 'listClientDevicesAssociatedWithCoreDeviceResponse_associatedClientDevices' - A list that describes the client devices that are associated with the
-- core device.
--
-- 'nextToken', 'listClientDevicesAssociatedWithCoreDeviceResponse_nextToken' - The token for the next set of results, or null if there are no
-- additional results.
--
-- 'httpStatus', 'listClientDevicesAssociatedWithCoreDeviceResponse_httpStatus' - The response's http status code.
newListClientDevicesAssociatedWithCoreDeviceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListClientDevicesAssociatedWithCoreDeviceResponse
newListClientDevicesAssociatedWithCoreDeviceResponse
  pHttpStatus_ =
    ListClientDevicesAssociatedWithCoreDeviceResponse'
      { associatedClientDevices =
          Prelude.Nothing,
        nextToken =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | A list that describes the client devices that are associated with the
-- core device.
listClientDevicesAssociatedWithCoreDeviceResponse_associatedClientDevices :: Lens.Lens' ListClientDevicesAssociatedWithCoreDeviceResponse (Prelude.Maybe (Prelude.NonEmpty AssociatedClientDevice))
listClientDevicesAssociatedWithCoreDeviceResponse_associatedClientDevices = Lens.lens (\ListClientDevicesAssociatedWithCoreDeviceResponse' {associatedClientDevices} -> associatedClientDevices) (\s@ListClientDevicesAssociatedWithCoreDeviceResponse' {} a -> s {associatedClientDevices = a} :: ListClientDevicesAssociatedWithCoreDeviceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or null if there are no
-- additional results.
listClientDevicesAssociatedWithCoreDeviceResponse_nextToken :: Lens.Lens' ListClientDevicesAssociatedWithCoreDeviceResponse (Prelude.Maybe Prelude.Text)
listClientDevicesAssociatedWithCoreDeviceResponse_nextToken = Lens.lens (\ListClientDevicesAssociatedWithCoreDeviceResponse' {nextToken} -> nextToken) (\s@ListClientDevicesAssociatedWithCoreDeviceResponse' {} a -> s {nextToken = a} :: ListClientDevicesAssociatedWithCoreDeviceResponse)

-- | The response's http status code.
listClientDevicesAssociatedWithCoreDeviceResponse_httpStatus :: Lens.Lens' ListClientDevicesAssociatedWithCoreDeviceResponse Prelude.Int
listClientDevicesAssociatedWithCoreDeviceResponse_httpStatus = Lens.lens (\ListClientDevicesAssociatedWithCoreDeviceResponse' {httpStatus} -> httpStatus) (\s@ListClientDevicesAssociatedWithCoreDeviceResponse' {} a -> s {httpStatus = a} :: ListClientDevicesAssociatedWithCoreDeviceResponse)

instance
  Prelude.NFData
    ListClientDevicesAssociatedWithCoreDeviceResponse
  where
  rnf
    ListClientDevicesAssociatedWithCoreDeviceResponse' {..} =
      Prelude.rnf associatedClientDevices
        `Prelude.seq` Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf httpStatus
