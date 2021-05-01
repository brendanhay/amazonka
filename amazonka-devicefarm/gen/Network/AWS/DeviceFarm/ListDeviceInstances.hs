{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.DeviceFarm.ListDeviceInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the private device instances associated with
-- one or more AWS accounts.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListDeviceInstances
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

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Pager.AWSPager ListDeviceInstances where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listDeviceInstancesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listDeviceInstancesResponse_deviceInstances
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listDeviceInstances_nextToken
          Lens..~ rs
          Lens.^? listDeviceInstancesResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListDeviceInstances where
  type
    Rs ListDeviceInstances =
      ListDeviceInstancesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeviceInstancesResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> ( x Prelude..?> "deviceInstances"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDeviceInstances

instance Prelude.NFData ListDeviceInstances

instance Prelude.ToHeaders ListDeviceInstances where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DeviceFarm_20150623.ListDeviceInstances" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListDeviceInstances where
  toJSON ListDeviceInstances' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("nextToken" Prelude..=) Prelude.<$> nextToken,
            ("maxResults" Prelude..=) Prelude.<$> maxResults
          ]
      )

instance Prelude.ToPath ListDeviceInstances where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListDeviceInstances where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
listDeviceInstancesResponse_deviceInstances = Lens.lens (\ListDeviceInstancesResponse' {deviceInstances} -> deviceInstances) (\s@ListDeviceInstancesResponse' {} a -> s {deviceInstances = a} :: ListDeviceInstancesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listDeviceInstancesResponse_httpStatus :: Lens.Lens' ListDeviceInstancesResponse Prelude.Int
listDeviceInstancesResponse_httpStatus = Lens.lens (\ListDeviceInstancesResponse' {httpStatus} -> httpStatus) (\s@ListDeviceInstancesResponse' {} a -> s {httpStatus = a} :: ListDeviceInstancesResponse)

instance Prelude.NFData ListDeviceInstancesResponse
