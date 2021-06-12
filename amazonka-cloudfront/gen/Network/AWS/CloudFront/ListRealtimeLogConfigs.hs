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
-- Module      : Network.AWS.CloudFront.ListRealtimeLogConfigs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of real-time log configurations.
--
-- You can optionally specify the maximum number of items to receive in the
-- response. If the total number of items in the list exceeds the maximum
-- that you specify, or the default maximum, the response is paginated. To
-- get the next page of items, send a subsequent request that specifies the
-- @NextMarker@ value from the current response as the @Marker@ value in
-- the subsequent request.
module Network.AWS.CloudFront.ListRealtimeLogConfigs
  ( -- * Creating a Request
    ListRealtimeLogConfigs (..),
    newListRealtimeLogConfigs,

    -- * Request Lenses
    listRealtimeLogConfigs_maxItems,
    listRealtimeLogConfigs_marker,

    -- * Destructuring the Response
    ListRealtimeLogConfigsResponse (..),
    newListRealtimeLogConfigsResponse,

    -- * Response Lenses
    listRealtimeLogConfigsResponse_realtimeLogConfigs,
    listRealtimeLogConfigsResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListRealtimeLogConfigs' smart constructor.
data ListRealtimeLogConfigs = ListRealtimeLogConfigs'
  { -- | The maximum number of real-time log configurations that you want in the
    -- response.
    maxItems :: Core.Maybe Core.Text,
    -- | Use this field when paginating results to indicate where to begin in
    -- your list of real-time log configurations. The response includes
    -- real-time log configurations in the list that occur after the marker. To
    -- get the next page of the list, set this field’s value to the value of
    -- @NextMarker@ from the current page’s response.
    marker :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListRealtimeLogConfigs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxItems', 'listRealtimeLogConfigs_maxItems' - The maximum number of real-time log configurations that you want in the
-- response.
--
-- 'marker', 'listRealtimeLogConfigs_marker' - Use this field when paginating results to indicate where to begin in
-- your list of real-time log configurations. The response includes
-- real-time log configurations in the list that occur after the marker. To
-- get the next page of the list, set this field’s value to the value of
-- @NextMarker@ from the current page’s response.
newListRealtimeLogConfigs ::
  ListRealtimeLogConfigs
newListRealtimeLogConfigs =
  ListRealtimeLogConfigs'
    { maxItems = Core.Nothing,
      marker = Core.Nothing
    }

-- | The maximum number of real-time log configurations that you want in the
-- response.
listRealtimeLogConfigs_maxItems :: Lens.Lens' ListRealtimeLogConfigs (Core.Maybe Core.Text)
listRealtimeLogConfigs_maxItems = Lens.lens (\ListRealtimeLogConfigs' {maxItems} -> maxItems) (\s@ListRealtimeLogConfigs' {} a -> s {maxItems = a} :: ListRealtimeLogConfigs)

-- | Use this field when paginating results to indicate where to begin in
-- your list of real-time log configurations. The response includes
-- real-time log configurations in the list that occur after the marker. To
-- get the next page of the list, set this field’s value to the value of
-- @NextMarker@ from the current page’s response.
listRealtimeLogConfigs_marker :: Lens.Lens' ListRealtimeLogConfigs (Core.Maybe Core.Text)
listRealtimeLogConfigs_marker = Lens.lens (\ListRealtimeLogConfigs' {marker} -> marker) (\s@ListRealtimeLogConfigs' {} a -> s {marker = a} :: ListRealtimeLogConfigs)

instance Core.AWSRequest ListRealtimeLogConfigs where
  type
    AWSResponse ListRealtimeLogConfigs =
      ListRealtimeLogConfigsResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ListRealtimeLogConfigsResponse'
            Core.<$> (Core.parseXML x)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListRealtimeLogConfigs

instance Core.NFData ListRealtimeLogConfigs

instance Core.ToHeaders ListRealtimeLogConfigs where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListRealtimeLogConfigs where
  toPath = Core.const "/2020-05-31/realtime-log-config"

instance Core.ToQuery ListRealtimeLogConfigs where
  toQuery ListRealtimeLogConfigs' {..} =
    Core.mconcat
      [ "MaxItems" Core.=: maxItems,
        "Marker" Core.=: marker
      ]

-- | /See:/ 'newListRealtimeLogConfigsResponse' smart constructor.
data ListRealtimeLogConfigsResponse = ListRealtimeLogConfigsResponse'
  { -- | A list of real-time log configurations.
    realtimeLogConfigs :: Core.Maybe RealtimeLogConfigs,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListRealtimeLogConfigsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'realtimeLogConfigs', 'listRealtimeLogConfigsResponse_realtimeLogConfigs' - A list of real-time log configurations.
--
-- 'httpStatus', 'listRealtimeLogConfigsResponse_httpStatus' - The response's http status code.
newListRealtimeLogConfigsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListRealtimeLogConfigsResponse
newListRealtimeLogConfigsResponse pHttpStatus_ =
  ListRealtimeLogConfigsResponse'
    { realtimeLogConfigs =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of real-time log configurations.
listRealtimeLogConfigsResponse_realtimeLogConfigs :: Lens.Lens' ListRealtimeLogConfigsResponse (Core.Maybe RealtimeLogConfigs)
listRealtimeLogConfigsResponse_realtimeLogConfigs = Lens.lens (\ListRealtimeLogConfigsResponse' {realtimeLogConfigs} -> realtimeLogConfigs) (\s@ListRealtimeLogConfigsResponse' {} a -> s {realtimeLogConfigs = a} :: ListRealtimeLogConfigsResponse)

-- | The response's http status code.
listRealtimeLogConfigsResponse_httpStatus :: Lens.Lens' ListRealtimeLogConfigsResponse Core.Int
listRealtimeLogConfigsResponse_httpStatus = Lens.lens (\ListRealtimeLogConfigsResponse' {httpStatus} -> httpStatus) (\s@ListRealtimeLogConfigsResponse' {} a -> s {httpStatus = a} :: ListRealtimeLogConfigsResponse)

instance Core.NFData ListRealtimeLogConfigsResponse
