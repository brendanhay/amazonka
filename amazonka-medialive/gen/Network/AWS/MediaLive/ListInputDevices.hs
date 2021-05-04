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
-- Module      : Network.AWS.MediaLive.ListInputDevices
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List input devices
--
-- This operation returns paginated results.
module Network.AWS.MediaLive.ListInputDevices
  ( -- * Creating a Request
    ListInputDevices (..),
    newListInputDevices,

    -- * Request Lenses
    listInputDevices_nextToken,
    listInputDevices_maxResults,

    -- * Destructuring the Response
    ListInputDevicesResponse (..),
    newListInputDevicesResponse,

    -- * Response Lenses
    listInputDevicesResponse_nextToken,
    listInputDevicesResponse_inputDevices,
    listInputDevicesResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for ListInputDevicesRequest
--
-- /See:/ 'newListInputDevices' smart constructor.
data ListInputDevices = ListInputDevices'
  { nextToken :: Prelude.Maybe Prelude.Text,
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListInputDevices' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listInputDevices_nextToken' - Undocumented member.
--
-- 'maxResults', 'listInputDevices_maxResults' - Undocumented member.
newListInputDevices ::
  ListInputDevices
newListInputDevices =
  ListInputDevices'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Undocumented member.
listInputDevices_nextToken :: Lens.Lens' ListInputDevices (Prelude.Maybe Prelude.Text)
listInputDevices_nextToken = Lens.lens (\ListInputDevices' {nextToken} -> nextToken) (\s@ListInputDevices' {} a -> s {nextToken = a} :: ListInputDevices)

-- | Undocumented member.
listInputDevices_maxResults :: Lens.Lens' ListInputDevices (Prelude.Maybe Prelude.Natural)
listInputDevices_maxResults = Lens.lens (\ListInputDevices' {maxResults} -> maxResults) (\s@ListInputDevices' {} a -> s {maxResults = a} :: ListInputDevices)

instance Pager.AWSPager ListInputDevices where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listInputDevicesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listInputDevicesResponse_inputDevices
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listInputDevices_nextToken
          Lens..~ rs
          Lens.^? listInputDevicesResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListInputDevices where
  type Rs ListInputDevices = ListInputDevicesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInputDevicesResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> ( x Prelude..?> "inputDevices"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListInputDevices

instance Prelude.NFData ListInputDevices

instance Prelude.ToHeaders ListInputDevices where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath ListInputDevices where
  toPath = Prelude.const "/prod/inputDevices"

instance Prelude.ToQuery ListInputDevices where
  toQuery ListInputDevices' {..} =
    Prelude.mconcat
      [ "nextToken" Prelude.=: nextToken,
        "maxResults" Prelude.=: maxResults
      ]

-- | Placeholder documentation for ListInputDevicesResponse
--
-- /See:/ 'newListInputDevicesResponse' smart constructor.
data ListInputDevicesResponse = ListInputDevicesResponse'
  { -- | A token to get additional list results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of input devices.
    inputDevices :: Prelude.Maybe [InputDeviceSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListInputDevicesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listInputDevicesResponse_nextToken' - A token to get additional list results.
--
-- 'inputDevices', 'listInputDevicesResponse_inputDevices' - The list of input devices.
--
-- 'httpStatus', 'listInputDevicesResponse_httpStatus' - The response's http status code.
newListInputDevicesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListInputDevicesResponse
newListInputDevicesResponse pHttpStatus_ =
  ListInputDevicesResponse'
    { nextToken =
        Prelude.Nothing,
      inputDevices = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token to get additional list results.
listInputDevicesResponse_nextToken :: Lens.Lens' ListInputDevicesResponse (Prelude.Maybe Prelude.Text)
listInputDevicesResponse_nextToken = Lens.lens (\ListInputDevicesResponse' {nextToken} -> nextToken) (\s@ListInputDevicesResponse' {} a -> s {nextToken = a} :: ListInputDevicesResponse)

-- | The list of input devices.
listInputDevicesResponse_inputDevices :: Lens.Lens' ListInputDevicesResponse (Prelude.Maybe [InputDeviceSummary])
listInputDevicesResponse_inputDevices = Lens.lens (\ListInputDevicesResponse' {inputDevices} -> inputDevices) (\s@ListInputDevicesResponse' {} a -> s {inputDevices = a} :: ListInputDevicesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listInputDevicesResponse_httpStatus :: Lens.Lens' ListInputDevicesResponse Prelude.Int
listInputDevicesResponse_httpStatus = Lens.lens (\ListInputDevicesResponse' {httpStatus} -> httpStatus) (\s@ListInputDevicesResponse' {} a -> s {httpStatus = a} :: ListInputDevicesResponse)

instance Prelude.NFData ListInputDevicesResponse
