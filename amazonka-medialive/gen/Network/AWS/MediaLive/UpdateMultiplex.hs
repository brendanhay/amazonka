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
-- Module      : Network.AWS.MediaLive.UpdateMultiplex
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a multiplex.
module Network.AWS.MediaLive.UpdateMultiplex
  ( -- * Creating a Request
    UpdateMultiplex' (..),
    newUpdateMultiplex',

    -- * Request Lenses
    updateMultiplex'_name,
    updateMultiplex'_multiplexSettings,
    updateMultiplex'_multiplexId,

    -- * Destructuring the Response
    UpdateMultiplexResponse (..),
    newUpdateMultiplexResponse,

    -- * Response Lenses
    updateMultiplexResponse_multiplex,
    updateMultiplexResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to update a multiplex.
--
-- /See:/ 'newUpdateMultiplex'' smart constructor.
data UpdateMultiplex' = UpdateMultiplex''
  { -- | Name of the multiplex.
    name :: Core.Maybe Core.Text,
    -- | The new settings for a multiplex.
    multiplexSettings :: Core.Maybe MultiplexSettings,
    -- | ID of the multiplex to update.
    multiplexId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateMultiplex'' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateMultiplex'_name' - Name of the multiplex.
--
-- 'multiplexSettings', 'updateMultiplex'_multiplexSettings' - The new settings for a multiplex.
--
-- 'multiplexId', 'updateMultiplex'_multiplexId' - ID of the multiplex to update.
newUpdateMultiplex' ::
  -- | 'multiplexId'
  Core.Text ->
  UpdateMultiplex'
newUpdateMultiplex' pMultiplexId_ =
  UpdateMultiplex''
    { name = Core.Nothing,
      multiplexSettings = Core.Nothing,
      multiplexId = pMultiplexId_
    }

-- | Name of the multiplex.
updateMultiplex'_name :: Lens.Lens' UpdateMultiplex' (Core.Maybe Core.Text)
updateMultiplex'_name = Lens.lens (\UpdateMultiplex'' {name} -> name) (\s@UpdateMultiplex'' {} a -> s {name = a} :: UpdateMultiplex')

-- | The new settings for a multiplex.
updateMultiplex'_multiplexSettings :: Lens.Lens' UpdateMultiplex' (Core.Maybe MultiplexSettings)
updateMultiplex'_multiplexSettings = Lens.lens (\UpdateMultiplex'' {multiplexSettings} -> multiplexSettings) (\s@UpdateMultiplex'' {} a -> s {multiplexSettings = a} :: UpdateMultiplex')

-- | ID of the multiplex to update.
updateMultiplex'_multiplexId :: Lens.Lens' UpdateMultiplex' Core.Text
updateMultiplex'_multiplexId = Lens.lens (\UpdateMultiplex'' {multiplexId} -> multiplexId) (\s@UpdateMultiplex'' {} a -> s {multiplexId = a} :: UpdateMultiplex')

instance Core.AWSRequest UpdateMultiplex' where
  type
    AWSResponse UpdateMultiplex' =
      UpdateMultiplexResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMultiplexResponse'
            Core.<$> (x Core..?> "multiplex")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateMultiplex'

instance Core.NFData UpdateMultiplex'

instance Core.ToHeaders UpdateMultiplex' where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateMultiplex' where
  toJSON UpdateMultiplex'' {..} =
    Core.object
      ( Core.catMaybes
          [ ("name" Core..=) Core.<$> name,
            ("multiplexSettings" Core..=)
              Core.<$> multiplexSettings
          ]
      )

instance Core.ToPath UpdateMultiplex' where
  toPath UpdateMultiplex'' {..} =
    Core.mconcat
      ["/prod/multiplexes/", Core.toBS multiplexId]

instance Core.ToQuery UpdateMultiplex' where
  toQuery = Core.const Core.mempty

-- | Placeholder documentation for UpdateMultiplexResponse
--
-- /See:/ 'newUpdateMultiplexResponse' smart constructor.
data UpdateMultiplexResponse = UpdateMultiplexResponse'
  { -- | The updated multiplex.
    multiplex :: Core.Maybe Multiplex,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateMultiplexResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'multiplex', 'updateMultiplexResponse_multiplex' - The updated multiplex.
--
-- 'httpStatus', 'updateMultiplexResponse_httpStatus' - The response's http status code.
newUpdateMultiplexResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateMultiplexResponse
newUpdateMultiplexResponse pHttpStatus_ =
  UpdateMultiplexResponse'
    { multiplex = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated multiplex.
updateMultiplexResponse_multiplex :: Lens.Lens' UpdateMultiplexResponse (Core.Maybe Multiplex)
updateMultiplexResponse_multiplex = Lens.lens (\UpdateMultiplexResponse' {multiplex} -> multiplex) (\s@UpdateMultiplexResponse' {} a -> s {multiplex = a} :: UpdateMultiplexResponse)

-- | The response's http status code.
updateMultiplexResponse_httpStatus :: Lens.Lens' UpdateMultiplexResponse Core.Int
updateMultiplexResponse_httpStatus = Lens.lens (\UpdateMultiplexResponse' {httpStatus} -> httpStatus) (\s@UpdateMultiplexResponse' {} a -> s {httpStatus = a} :: UpdateMultiplexResponse)

instance Core.NFData UpdateMultiplexResponse
