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
-- Module      : Amazonka.DeviceFarm.ListDeviceInstances
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the private device instances associated with
-- one or more AWS accounts.
--
-- This operation returns paginated results.
module Amazonka.DeviceFarm.ListDeviceInstances
  ( -- * Creating a Request
    ListDeviceInstances (..),
    newListDeviceInstances,

    -- * Request Lenses
    listDeviceInstances_nextToken,
    listDeviceInstances_maxResults,

    -- * Destructuring the Response
    ListDeviceInstancesResponse (..),
    newListDeviceInstancesResponse,

    -- * Response Lenses
    listDeviceInstancesResponse_nextToken,
    listDeviceInstancesResponse_deviceInstances,
    listDeviceInstancesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDeviceInstances' smart constructor.
data ListDeviceInstances = ListDeviceInstances'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An integer that specifies the maximum number of items you want to return
    -- in the API response.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDeviceInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDeviceInstances_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'maxResults', 'listDeviceInstances_maxResults' - An integer that specifies the maximum number of items you want to return
-- in the API response.
newListDeviceInstances ::
  ListDeviceInstances
newListDeviceInstances =
  ListDeviceInstances'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listDeviceInstances_nextToken :: Lens.Lens' ListDeviceInstances (Prelude.Maybe Prelude.Text)
listDeviceInstances_nextToken = Lens.lens (\ListDeviceInstances' {nextToken} -> nextToken) (\s@ListDeviceInstances' {} a -> s {nextToken = a} :: ListDeviceInstances)

-- | An integer that specifies the maximum number of items you want to return
-- in the API response.
listDeviceInstances_maxResults :: Lens.Lens' ListDeviceInstances (Prelude.Maybe Prelude.Int)
listDeviceInstances_maxResults = Lens.lens (\ListDeviceInstances' {maxResults} -> maxResults) (\s@ListDeviceInstances' {} a -> s {maxResults = a} :: ListDeviceInstances)

instance Core.AWSPager ListDeviceInstances where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDeviceInstancesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDeviceInstancesResponse_deviceInstances
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDeviceInstances_nextToken
          Lens..~ rs
          Lens.^? listDeviceInstancesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListDeviceInstances where
  type
    AWSResponse ListDeviceInstances =
      ListDeviceInstancesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeviceInstancesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "deviceInstances"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDeviceInstances where
  hashWithSalt _salt ListDeviceInstances' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListDeviceInstances where
  rnf ListDeviceInstances' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListDeviceInstances where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.ListDeviceInstances" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListDeviceInstances where
  toJSON ListDeviceInstances' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListDeviceInstances where
  toPath = Prelude.const "/"

instance Core.ToQuery ListDeviceInstances where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDeviceInstancesResponse' smart constructor.
data ListDeviceInstancesResponse = ListDeviceInstancesResponse'
  { -- | An identifier that can be used in the next call to this operation to
    -- return the next set of items in the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An object that contains information about your device instances.
    deviceInstances :: Prelude.Maybe [DeviceInstance],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDeviceInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDeviceInstancesResponse_nextToken' - An identifier that can be used in the next call to this operation to
-- return the next set of items in the list.
--
-- 'deviceInstances', 'listDeviceInstancesResponse_deviceInstances' - An object that contains information about your device instances.
--
-- 'httpStatus', 'listDeviceInstancesResponse_httpStatus' - The response's http status code.
newListDeviceInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDeviceInstancesResponse
newListDeviceInstancesResponse pHttpStatus_ =
  ListDeviceInstancesResponse'
    { nextToken =
        Prelude.Nothing,
      deviceInstances = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An identifier that can be used in the next call to this operation to
-- return the next set of items in the list.
listDeviceInstancesResponse_nextToken :: Lens.Lens' ListDeviceInstancesResponse (Prelude.Maybe Prelude.Text)
listDeviceInstancesResponse_nextToken = Lens.lens (\ListDeviceInstancesResponse' {nextToken} -> nextToken) (\s@ListDeviceInstancesResponse' {} a -> s {nextToken = a} :: ListDeviceInstancesResponse)

-- | An object that contains information about your device instances.
listDeviceInstancesResponse_deviceInstances :: Lens.Lens' ListDeviceInstancesResponse (Prelude.Maybe [DeviceInstance])
listDeviceInstancesResponse_deviceInstances = Lens.lens (\ListDeviceInstancesResponse' {deviceInstances} -> deviceInstances) (\s@ListDeviceInstancesResponse' {} a -> s {deviceInstances = a} :: ListDeviceInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDeviceInstancesResponse_httpStatus :: Lens.Lens' ListDeviceInstancesResponse Prelude.Int
listDeviceInstancesResponse_httpStatus = Lens.lens (\ListDeviceInstancesResponse' {httpStatus} -> httpStatus) (\s@ListDeviceInstancesResponse' {} a -> s {httpStatus = a} :: ListDeviceInstancesResponse)

instance Prelude.NFData ListDeviceInstancesResponse where
  rnf ListDeviceInstancesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf deviceInstances
      `Prelude.seq` Prelude.rnf httpStatus
