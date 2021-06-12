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
-- Module      : Network.AWS.MediaLive.UpdateChannelClass
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the class of the channel.
module Network.AWS.MediaLive.UpdateChannelClass
  ( -- * Creating a Request
    UpdateChannelClass' (..),
    newUpdateChannelClass',

    -- * Request Lenses
    updateChannelClass'_destinations,
    updateChannelClass'_channelId,
    updateChannelClass'_channelClass,

    -- * Destructuring the Response
    UpdateChannelClassResponse (..),
    newUpdateChannelClassResponse,

    -- * Response Lenses
    updateChannelClassResponse_channel,
    updateChannelClassResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Channel class that the channel should be updated to.
--
-- /See:/ 'newUpdateChannelClass'' smart constructor.
data UpdateChannelClass' = UpdateChannelClass''
  { -- | A list of output destinations for this channel.
    destinations :: Core.Maybe [OutputDestination],
    -- | Channel Id of the channel whose class should be updated.
    channelId :: Core.Text,
    -- | The channel class that you wish to update this channel to use.
    channelClass :: ChannelClass
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateChannelClass'' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinations', 'updateChannelClass'_destinations' - A list of output destinations for this channel.
--
-- 'channelId', 'updateChannelClass'_channelId' - Channel Id of the channel whose class should be updated.
--
-- 'channelClass', 'updateChannelClass'_channelClass' - The channel class that you wish to update this channel to use.
newUpdateChannelClass' ::
  -- | 'channelId'
  Core.Text ->
  -- | 'channelClass'
  ChannelClass ->
  UpdateChannelClass'
newUpdateChannelClass' pChannelId_ pChannelClass_ =
  UpdateChannelClass''
    { destinations = Core.Nothing,
      channelId = pChannelId_,
      channelClass = pChannelClass_
    }

-- | A list of output destinations for this channel.
updateChannelClass'_destinations :: Lens.Lens' UpdateChannelClass' (Core.Maybe [OutputDestination])
updateChannelClass'_destinations = Lens.lens (\UpdateChannelClass'' {destinations} -> destinations) (\s@UpdateChannelClass'' {} a -> s {destinations = a} :: UpdateChannelClass') Core.. Lens.mapping Lens._Coerce

-- | Channel Id of the channel whose class should be updated.
updateChannelClass'_channelId :: Lens.Lens' UpdateChannelClass' Core.Text
updateChannelClass'_channelId = Lens.lens (\UpdateChannelClass'' {channelId} -> channelId) (\s@UpdateChannelClass'' {} a -> s {channelId = a} :: UpdateChannelClass')

-- | The channel class that you wish to update this channel to use.
updateChannelClass'_channelClass :: Lens.Lens' UpdateChannelClass' ChannelClass
updateChannelClass'_channelClass = Lens.lens (\UpdateChannelClass'' {channelClass} -> channelClass) (\s@UpdateChannelClass'' {} a -> s {channelClass = a} :: UpdateChannelClass')

instance Core.AWSRequest UpdateChannelClass' where
  type
    AWSResponse UpdateChannelClass' =
      UpdateChannelClassResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateChannelClassResponse'
            Core.<$> (x Core..?> "channel")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateChannelClass'

instance Core.NFData UpdateChannelClass'

instance Core.ToHeaders UpdateChannelClass' where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateChannelClass' where
  toJSON UpdateChannelClass'' {..} =
    Core.object
      ( Core.catMaybes
          [ ("destinations" Core..=) Core.<$> destinations,
            Core.Just ("channelClass" Core..= channelClass)
          ]
      )

instance Core.ToPath UpdateChannelClass' where
  toPath UpdateChannelClass'' {..} =
    Core.mconcat
      [ "/prod/channels/",
        Core.toBS channelId,
        "/channelClass"
      ]

instance Core.ToQuery UpdateChannelClass' where
  toQuery = Core.const Core.mempty

-- | Placeholder documentation for UpdateChannelClassResponse
--
-- /See:/ 'newUpdateChannelClassResponse' smart constructor.
data UpdateChannelClassResponse = UpdateChannelClassResponse'
  { channel :: Core.Maybe Channel,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateChannelClassResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channel', 'updateChannelClassResponse_channel' - Undocumented member.
--
-- 'httpStatus', 'updateChannelClassResponse_httpStatus' - The response's http status code.
newUpdateChannelClassResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateChannelClassResponse
newUpdateChannelClassResponse pHttpStatus_ =
  UpdateChannelClassResponse'
    { channel = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateChannelClassResponse_channel :: Lens.Lens' UpdateChannelClassResponse (Core.Maybe Channel)
updateChannelClassResponse_channel = Lens.lens (\UpdateChannelClassResponse' {channel} -> channel) (\s@UpdateChannelClassResponse' {} a -> s {channel = a} :: UpdateChannelClassResponse)

-- | The response's http status code.
updateChannelClassResponse_httpStatus :: Lens.Lens' UpdateChannelClassResponse Core.Int
updateChannelClassResponse_httpStatus = Lens.lens (\UpdateChannelClassResponse' {httpStatus} -> httpStatus) (\s@UpdateChannelClassResponse' {} a -> s {httpStatus = a} :: UpdateChannelClassResponse)

instance Core.NFData UpdateChannelClassResponse
