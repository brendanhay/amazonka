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
    cancelReplayResponse_state,
    cancelReplayResponse_replayArn,
    cancelReplayResponse_stateReason,
    cancelReplayResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCancelReplay' smart constructor.
data CancelReplay = CancelReplay'
  { -- | The name of the replay to cancel.
    replayName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  CancelReplay
newCancelReplay pReplayName_ =
  CancelReplay' {replayName = pReplayName_}

-- | The name of the replay to cancel.
cancelReplay_replayName :: Lens.Lens' CancelReplay Prelude.Text
cancelReplay_replayName = Lens.lens (\CancelReplay' {replayName} -> replayName) (\s@CancelReplay' {} a -> s {replayName = a} :: CancelReplay)

instance Core.AWSRequest CancelReplay where
  type AWSResponse CancelReplay = CancelReplayResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelReplayResponse'
            Prelude.<$> (x Core..?> "State")
            Prelude.<*> (x Core..?> "ReplayArn")
            Prelude.<*> (x Core..?> "StateReason")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelReplay

instance Prelude.NFData CancelReplay

instance Core.ToHeaders CancelReplay where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSEvents.CancelReplay" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CancelReplay where
  toJSON CancelReplay' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("ReplayName" Core..= replayName)]
      )

instance Core.ToPath CancelReplay where
  toPath = Prelude.const "/"

instance Core.ToQuery CancelReplay where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelReplayResponse' smart constructor.
data CancelReplayResponse = CancelReplayResponse'
  { -- | The current state of the replay.
    state :: Prelude.Maybe ReplayState,
    -- | The ARN of the replay to cancel.
    replayArn :: Prelude.Maybe Prelude.Text,
    -- | The reason that the replay is in the current state.
    stateReason :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelReplayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'cancelReplayResponse_state' - The current state of the replay.
--
-- 'replayArn', 'cancelReplayResponse_replayArn' - The ARN of the replay to cancel.
--
-- 'stateReason', 'cancelReplayResponse_stateReason' - The reason that the replay is in the current state.
--
-- 'httpStatus', 'cancelReplayResponse_httpStatus' - The response's http status code.
newCancelReplayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelReplayResponse
newCancelReplayResponse pHttpStatus_ =
  CancelReplayResponse'
    { state = Prelude.Nothing,
      replayArn = Prelude.Nothing,
      stateReason = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current state of the replay.
cancelReplayResponse_state :: Lens.Lens' CancelReplayResponse (Prelude.Maybe ReplayState)
cancelReplayResponse_state = Lens.lens (\CancelReplayResponse' {state} -> state) (\s@CancelReplayResponse' {} a -> s {state = a} :: CancelReplayResponse)

-- | The ARN of the replay to cancel.
cancelReplayResponse_replayArn :: Lens.Lens' CancelReplayResponse (Prelude.Maybe Prelude.Text)
cancelReplayResponse_replayArn = Lens.lens (\CancelReplayResponse' {replayArn} -> replayArn) (\s@CancelReplayResponse' {} a -> s {replayArn = a} :: CancelReplayResponse)

-- | The reason that the replay is in the current state.
cancelReplayResponse_stateReason :: Lens.Lens' CancelReplayResponse (Prelude.Maybe Prelude.Text)
cancelReplayResponse_stateReason = Lens.lens (\CancelReplayResponse' {stateReason} -> stateReason) (\s@CancelReplayResponse' {} a -> s {stateReason = a} :: CancelReplayResponse)

-- | The response's http status code.
cancelReplayResponse_httpStatus :: Lens.Lens' CancelReplayResponse Prelude.Int
cancelReplayResponse_httpStatus = Lens.lens (\CancelReplayResponse' {httpStatus} -> httpStatus) (\s@CancelReplayResponse' {} a -> s {httpStatus = a} :: CancelReplayResponse)

instance Prelude.NFData CancelReplayResponse
