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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for ListInputDevicesRequest
--
-- /See:/ 'newListInputDevices' smart constructor.
data ListInputDevices = ListInputDevices'
  { nextToken :: Core.Maybe Core.Text,
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | Undocumented member.
listInputDevices_nextToken :: Lens.Lens' ListInputDevices (Core.Maybe Core.Text)
listInputDevices_nextToken = Lens.lens (\ListInputDevices' {nextToken} -> nextToken) (\s@ListInputDevices' {} a -> s {nextToken = a} :: ListInputDevices)

-- | Undocumented member.
listInputDevices_maxResults :: Lens.Lens' ListInputDevices (Core.Maybe Core.Natural)
listInputDevices_maxResults = Lens.lens (\ListInputDevices' {maxResults} -> maxResults) (\s@ListInputDevices' {} a -> s {maxResults = a} :: ListInputDevices)

instance Core.AWSPager ListInputDevices where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listInputDevicesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listInputDevicesResponse_inputDevices
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listInputDevices_nextToken
          Lens..~ rs
          Lens.^? listInputDevicesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListInputDevices where
  type
    AWSResponse ListInputDevices =
      ListInputDevicesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInputDevicesResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "inputDevices" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListInputDevices

instance Core.NFData ListInputDevices

instance Core.ToHeaders ListInputDevices where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListInputDevices where
  toPath = Core.const "/prod/inputDevices"

instance Core.ToQuery ListInputDevices where
  toQuery ListInputDevices' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | Placeholder documentation for ListInputDevicesResponse
--
-- /See:/ 'newListInputDevicesResponse' smart constructor.
data ListInputDevicesResponse = ListInputDevicesResponse'
  { -- | A token to get additional list results.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of input devices.
    inputDevices :: Core.Maybe [InputDeviceSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListInputDevicesResponse
newListInputDevicesResponse pHttpStatus_ =
  ListInputDevicesResponse'
    { nextToken = Core.Nothing,
      inputDevices = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token to get additional list results.
listInputDevicesResponse_nextToken :: Lens.Lens' ListInputDevicesResponse (Core.Maybe Core.Text)
listInputDevicesResponse_nextToken = Lens.lens (\ListInputDevicesResponse' {nextToken} -> nextToken) (\s@ListInputDevicesResponse' {} a -> s {nextToken = a} :: ListInputDevicesResponse)

-- | The list of input devices.
listInputDevicesResponse_inputDevices :: Lens.Lens' ListInputDevicesResponse (Core.Maybe [InputDeviceSummary])
listInputDevicesResponse_inputDevices = Lens.lens (\ListInputDevicesResponse' {inputDevices} -> inputDevices) (\s@ListInputDevicesResponse' {} a -> s {inputDevices = a} :: ListInputDevicesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listInputDevicesResponse_httpStatus :: Lens.Lens' ListInputDevicesResponse Core.Int
listInputDevicesResponse_httpStatus = Lens.lens (\ListInputDevicesResponse' {httpStatus} -> httpStatus) (\s@ListInputDevicesResponse' {} a -> s {httpStatus = a} :: ListInputDevicesResponse)

instance Core.NFData ListInputDevicesResponse
