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
-- Module      : Amazonka.CloudWatchEvents.CancelReplay
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the specified replay.
module Amazonka.CloudWatchEvents.CancelReplay
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
    cancelReplayResponse_state,
    cancelReplayResponse_stateReason,
    cancelReplayResponse_httpStatus,
  )
where

import Amazonka.CloudWatchEvents.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelReplayResponse'
            Prelude.<$> (x Data..?> "ReplayArn")
            Prelude.<*> (x Data..?> "State")
            Prelude.<*> (x Data..?> "StateReason")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelReplay where
  hashWithSalt _salt CancelReplay' {..} =
    _salt `Prelude.hashWithSalt` replayName

instance Prelude.NFData CancelReplay where
  rnf CancelReplay' {..} = Prelude.rnf replayName

instance Data.ToHeaders CancelReplay where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSEvents.CancelReplay" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CancelReplay where
  toJSON CancelReplay' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ReplayName" Data..= replayName)]
      )

instance Data.ToPath CancelReplay where
  toPath = Prelude.const "/"

instance Data.ToQuery CancelReplay where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelReplayResponse' smart constructor.
data CancelReplayResponse = CancelReplayResponse'
  { -- | The ARN of the replay to cancel.
    replayArn :: Prelude.Maybe Prelude.Text,
    -- | The current state of the replay.
    state :: Prelude.Maybe ReplayState,
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
-- 'replayArn', 'cancelReplayResponse_replayArn' - The ARN of the replay to cancel.
--
-- 'state', 'cancelReplayResponse_state' - The current state of the replay.
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
    { replayArn = Prelude.Nothing,
      state = Prelude.Nothing,
      stateReason = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the replay to cancel.
cancelReplayResponse_replayArn :: Lens.Lens' CancelReplayResponse (Prelude.Maybe Prelude.Text)
cancelReplayResponse_replayArn = Lens.lens (\CancelReplayResponse' {replayArn} -> replayArn) (\s@CancelReplayResponse' {} a -> s {replayArn = a} :: CancelReplayResponse)

-- | The current state of the replay.
cancelReplayResponse_state :: Lens.Lens' CancelReplayResponse (Prelude.Maybe ReplayState)
cancelReplayResponse_state = Lens.lens (\CancelReplayResponse' {state} -> state) (\s@CancelReplayResponse' {} a -> s {state = a} :: CancelReplayResponse)

-- | The reason that the replay is in the current state.
cancelReplayResponse_stateReason :: Lens.Lens' CancelReplayResponse (Prelude.Maybe Prelude.Text)
cancelReplayResponse_stateReason = Lens.lens (\CancelReplayResponse' {stateReason} -> stateReason) (\s@CancelReplayResponse' {} a -> s {stateReason = a} :: CancelReplayResponse)

-- | The response's http status code.
cancelReplayResponse_httpStatus :: Lens.Lens' CancelReplayResponse Prelude.Int
cancelReplayResponse_httpStatus = Lens.lens (\CancelReplayResponse' {httpStatus} -> httpStatus) (\s@CancelReplayResponse' {} a -> s {httpStatus = a} :: CancelReplayResponse)

instance Prelude.NFData CancelReplayResponse where
  rnf CancelReplayResponse' {..} =
    Prelude.rnf replayArn `Prelude.seq`
      Prelude.rnf state `Prelude.seq`
        Prelude.rnf stateReason `Prelude.seq`
          Prelude.rnf httpStatus
