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
-- Module      : Network.AWS.KinesisVideo.DeleteSignalingChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified signaling channel. @DeleteSignalingChannel@ is an
-- asynchronous operation. If you don\'t specify the channel\'s current
-- version, the most recent version is deleted.
module Network.AWS.KinesisVideo.DeleteSignalingChannel
  ( -- * Creating a Request
    DeleteSignalingChannel (..),
    newDeleteSignalingChannel,

    -- * Request Lenses
    deleteSignalingChannel_currentVersion,
    deleteSignalingChannel_channelARN,

    -- * Destructuring the Response
    DeleteSignalingChannelResponse (..),
    newDeleteSignalingChannelResponse,

    -- * Response Lenses
    deleteSignalingChannelResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.KinesisVideo.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteSignalingChannel' smart constructor.
data DeleteSignalingChannel = DeleteSignalingChannel'
  { -- | The current version of the signaling channel that you want to delete.
    -- You can obtain the current version by invoking the
    -- @DescribeSignalingChannel@ or @ListSignalingChannels@ API operations.
    currentVersion :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the signaling channel that you want to
    -- delete.
    channelARN :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteSignalingChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currentVersion', 'deleteSignalingChannel_currentVersion' - The current version of the signaling channel that you want to delete.
-- You can obtain the current version by invoking the
-- @DescribeSignalingChannel@ or @ListSignalingChannels@ API operations.
--
-- 'channelARN', 'deleteSignalingChannel_channelARN' - The Amazon Resource Name (ARN) of the signaling channel that you want to
-- delete.
newDeleteSignalingChannel ::
  -- | 'channelARN'
  Core.Text ->
  DeleteSignalingChannel
newDeleteSignalingChannel pChannelARN_ =
  DeleteSignalingChannel'
    { currentVersion =
        Core.Nothing,
      channelARN = pChannelARN_
    }

-- | The current version of the signaling channel that you want to delete.
-- You can obtain the current version by invoking the
-- @DescribeSignalingChannel@ or @ListSignalingChannels@ API operations.
deleteSignalingChannel_currentVersion :: Lens.Lens' DeleteSignalingChannel (Core.Maybe Core.Text)
deleteSignalingChannel_currentVersion = Lens.lens (\DeleteSignalingChannel' {currentVersion} -> currentVersion) (\s@DeleteSignalingChannel' {} a -> s {currentVersion = a} :: DeleteSignalingChannel)

-- | The Amazon Resource Name (ARN) of the signaling channel that you want to
-- delete.
deleteSignalingChannel_channelARN :: Lens.Lens' DeleteSignalingChannel Core.Text
deleteSignalingChannel_channelARN = Lens.lens (\DeleteSignalingChannel' {channelARN} -> channelARN) (\s@DeleteSignalingChannel' {} a -> s {channelARN = a} :: DeleteSignalingChannel)

instance Core.AWSRequest DeleteSignalingChannel where
  type
    AWSResponse DeleteSignalingChannel =
      DeleteSignalingChannelResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteSignalingChannelResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteSignalingChannel

instance Core.NFData DeleteSignalingChannel

instance Core.ToHeaders DeleteSignalingChannel where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON DeleteSignalingChannel where
  toJSON DeleteSignalingChannel' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CurrentVersion" Core..=) Core.<$> currentVersion,
            Core.Just ("ChannelARN" Core..= channelARN)
          ]
      )

instance Core.ToPath DeleteSignalingChannel where
  toPath = Core.const "/deleteSignalingChannel"

instance Core.ToQuery DeleteSignalingChannel where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteSignalingChannelResponse' smart constructor.
data DeleteSignalingChannelResponse = DeleteSignalingChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteSignalingChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteSignalingChannelResponse_httpStatus' - The response's http status code.
newDeleteSignalingChannelResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteSignalingChannelResponse
newDeleteSignalingChannelResponse pHttpStatus_ =
  DeleteSignalingChannelResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteSignalingChannelResponse_httpStatus :: Lens.Lens' DeleteSignalingChannelResponse Core.Int
deleteSignalingChannelResponse_httpStatus = Lens.lens (\DeleteSignalingChannelResponse' {httpStatus} -> httpStatus) (\s@DeleteSignalingChannelResponse' {} a -> s {httpStatus = a} :: DeleteSignalingChannelResponse)

instance Core.NFData DeleteSignalingChannelResponse
