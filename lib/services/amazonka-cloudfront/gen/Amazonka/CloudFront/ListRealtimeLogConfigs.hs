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
-- Module      : Amazonka.CloudFront.ListRealtimeLogConfigs
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.CloudFront.ListRealtimeLogConfigs
  ( -- * Creating a Request
    ListRealtimeLogConfigs (..),
    newListRealtimeLogConfigs,

    -- * Request Lenses
    listRealtimeLogConfigs_marker,
    listRealtimeLogConfigs_maxItems,

    -- * Destructuring the Response
    ListRealtimeLogConfigsResponse (..),
    newListRealtimeLogConfigsResponse,

    -- * Response Lenses
    listRealtimeLogConfigsResponse_realtimeLogConfigs,
    listRealtimeLogConfigsResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRealtimeLogConfigs' smart constructor.
data ListRealtimeLogConfigs = ListRealtimeLogConfigs'
  { -- | Use this field when paginating results to indicate where to begin in
    -- your list of real-time log configurations. The response includes
    -- real-time log configurations in the list that occur after the marker. To
    -- get the next page of the list, set this field\'s value to the value of
    -- @NextMarker@ from the current page\'s response.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of real-time log configurations that you want in the
    -- response.
    maxItems :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRealtimeLogConfigs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listRealtimeLogConfigs_marker' - Use this field when paginating results to indicate where to begin in
-- your list of real-time log configurations. The response includes
-- real-time log configurations in the list that occur after the marker. To
-- get the next page of the list, set this field\'s value to the value of
-- @NextMarker@ from the current page\'s response.
--
-- 'maxItems', 'listRealtimeLogConfigs_maxItems' - The maximum number of real-time log configurations that you want in the
-- response.
newListRealtimeLogConfigs ::
  ListRealtimeLogConfigs
newListRealtimeLogConfigs =
  ListRealtimeLogConfigs'
    { marker = Prelude.Nothing,
      maxItems = Prelude.Nothing
    }

-- | Use this field when paginating results to indicate where to begin in
-- your list of real-time log configurations. The response includes
-- real-time log configurations in the list that occur after the marker. To
-- get the next page of the list, set this field\'s value to the value of
-- @NextMarker@ from the current page\'s response.
listRealtimeLogConfigs_marker :: Lens.Lens' ListRealtimeLogConfigs (Prelude.Maybe Prelude.Text)
listRealtimeLogConfigs_marker = Lens.lens (\ListRealtimeLogConfigs' {marker} -> marker) (\s@ListRealtimeLogConfigs' {} a -> s {marker = a} :: ListRealtimeLogConfigs)

-- | The maximum number of real-time log configurations that you want in the
-- response.
listRealtimeLogConfigs_maxItems :: Lens.Lens' ListRealtimeLogConfigs (Prelude.Maybe Prelude.Text)
listRealtimeLogConfigs_maxItems = Lens.lens (\ListRealtimeLogConfigs' {maxItems} -> maxItems) (\s@ListRealtimeLogConfigs' {} a -> s {maxItems = a} :: ListRealtimeLogConfigs)

instance Core.AWSRequest ListRealtimeLogConfigs where
  type
    AWSResponse ListRealtimeLogConfigs =
      ListRealtimeLogConfigsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListRealtimeLogConfigsResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRealtimeLogConfigs where
  hashWithSalt _salt ListRealtimeLogConfigs' {..} =
    _salt
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems

instance Prelude.NFData ListRealtimeLogConfigs where
  rnf ListRealtimeLogConfigs' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems

instance Data.ToHeaders ListRealtimeLogConfigs where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListRealtimeLogConfigs where
  toPath =
    Prelude.const "/2020-05-31/realtime-log-config"

instance Data.ToQuery ListRealtimeLogConfigs where
  toQuery ListRealtimeLogConfigs' {..} =
    Prelude.mconcat
      [ "Marker" Data.=: marker,
        "MaxItems" Data.=: maxItems
      ]

-- | /See:/ 'newListRealtimeLogConfigsResponse' smart constructor.
data ListRealtimeLogConfigsResponse = ListRealtimeLogConfigsResponse'
  { -- | A list of real-time log configurations.
    realtimeLogConfigs :: Prelude.Maybe RealtimeLogConfigs,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListRealtimeLogConfigsResponse
newListRealtimeLogConfigsResponse pHttpStatus_ =
  ListRealtimeLogConfigsResponse'
    { realtimeLogConfigs =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of real-time log configurations.
listRealtimeLogConfigsResponse_realtimeLogConfigs :: Lens.Lens' ListRealtimeLogConfigsResponse (Prelude.Maybe RealtimeLogConfigs)
listRealtimeLogConfigsResponse_realtimeLogConfigs = Lens.lens (\ListRealtimeLogConfigsResponse' {realtimeLogConfigs} -> realtimeLogConfigs) (\s@ListRealtimeLogConfigsResponse' {} a -> s {realtimeLogConfigs = a} :: ListRealtimeLogConfigsResponse)

-- | The response's http status code.
listRealtimeLogConfigsResponse_httpStatus :: Lens.Lens' ListRealtimeLogConfigsResponse Prelude.Int
listRealtimeLogConfigsResponse_httpStatus = Lens.lens (\ListRealtimeLogConfigsResponse' {httpStatus} -> httpStatus) (\s@ListRealtimeLogConfigsResponse' {} a -> s {httpStatus = a} :: ListRealtimeLogConfigsResponse)

instance
  Prelude.NFData
    ListRealtimeLogConfigsResponse
  where
  rnf ListRealtimeLogConfigsResponse' {..} =
    Prelude.rnf realtimeLogConfigs
      `Prelude.seq` Prelude.rnf httpStatus
