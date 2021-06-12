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
-- Module      : Network.AWS.CloudWatchEvents.CancelReplay
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the specified replay.
module Network.AWS.CloudWatchEvents.CancelReplay
  ( -- * Creating a Request
    CancelReplay (..),
    newCancelReplay,

    -- * Request Lenses
    cancelReplay_replayName,

    -- * Destructuring the Response
    CancelReplayResponse (..),
    newCancelReplayResponse,

    -- * Response Lenses
    cancelReplayResponse_replayArn,
    cancelReplayResponse_stateReason,
    cancelReplayResponse_state,
    cancelReplayResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCancelReplay' smart constructor.
data CancelReplay = CancelReplay'
  { -- | The name of the replay to cancel.
    replayName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CancelReplay' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replayName', 'cancelReplay_replayName' - The name of the replay to cancel.
newCancelReplay ::
  -- | 'replayName'
  Core.Text ->
  CancelReplay
newCancelReplay pReplayName_ =
  CancelReplay' {replayName = pReplayName_}

-- | The name of the replay to cancel.
cancelReplay_replayName :: Lens.Lens' CancelReplay Core.Text
cancelReplay_replayName = Lens.lens (\CancelReplay' {replayName} -> replayName) (\s@CancelReplay' {} a -> s {replayName = a} :: CancelReplay)

instance Core.AWSRequest CancelReplay where
  type AWSResponse CancelReplay = CancelReplayResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelReplayResponse'
            Core.<$> (x Core..?> "ReplayArn")
            Core.<*> (x Core..?> "StateReason")
            Core.<*> (x Core..?> "State")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CancelReplay

instance Core.NFData CancelReplay

instance Core.ToHeaders CancelReplay where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSEvents.CancelReplay" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CancelReplay where
  toJSON CancelReplay' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ReplayName" Core..= replayName)]
      )

instance Core.ToPath CancelReplay where
  toPath = Core.const "/"

instance Core.ToQuery CancelReplay where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCancelReplayResponse' smart constructor.
data CancelReplayResponse = CancelReplayResponse'
  { -- | The ARN of the replay to cancel.
    replayArn :: Core.Maybe Core.Text,
    -- | The reason that the replay is in the current state.
    stateReason :: Core.Maybe Core.Text,
    -- | The current state of the replay.
    state :: Core.Maybe ReplayState,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CancelReplayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replayArn', 'cancelReplayResponse_replayArn' - The ARN of the replay to cancel.
--
-- 'stateReason', 'cancelReplayResponse_stateReason' - The reason that the replay is in the current state.
--
-- 'state', 'cancelReplayResponse_state' - The current state of the replay.
--
-- 'httpStatus', 'cancelReplayResponse_httpStatus' - The response's http status code.
newCancelReplayResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CancelReplayResponse
newCancelReplayResponse pHttpStatus_ =
  CancelReplayResponse'
    { replayArn = Core.Nothing,
      stateReason = Core.Nothing,
      state = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the replay to cancel.
cancelReplayResponse_replayArn :: Lens.Lens' CancelReplayResponse (Core.Maybe Core.Text)
cancelReplayResponse_replayArn = Lens.lens (\CancelReplayResponse' {replayArn} -> replayArn) (\s@CancelReplayResponse' {} a -> s {replayArn = a} :: CancelReplayResponse)

-- | The reason that the replay is in the current state.
cancelReplayResponse_stateReason :: Lens.Lens' CancelReplayResponse (Core.Maybe Core.Text)
cancelReplayResponse_stateReason = Lens.lens (\CancelReplayResponse' {stateReason} -> stateReason) (\s@CancelReplayResponse' {} a -> s {stateReason = a} :: CancelReplayResponse)

-- | The current state of the replay.
cancelReplayResponse_state :: Lens.Lens' CancelReplayResponse (Core.Maybe ReplayState)
cancelReplayResponse_state = Lens.lens (\CancelReplayResponse' {state} -> state) (\s@CancelReplayResponse' {} a -> s {state = a} :: CancelReplayResponse)

-- | The response's http status code.
cancelReplayResponse_httpStatus :: Lens.Lens' CancelReplayResponse Core.Int
cancelReplayResponse_httpStatus = Lens.lens (\CancelReplayResponse' {httpStatus} -> httpStatus) (\s@CancelReplayResponse' {} a -> s {httpStatus = a} :: CancelReplayResponse)

instance Core.NFData CancelReplayResponse
