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
-- Module      : Amazonka.KinesisVideoWebRtcStorage.JoinStorageSession
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Join the ongoing one way-video and\/or multi-way audio WebRTC session as
-- a video producing device for an input channel. If there’s no existing
-- session for the channel, a new streaming session needs to be created,
-- and the Amazon Resource Name (ARN) of the signaling channel must be
-- provided.
--
-- Currently for the @SINGLE_MASTER@ type, a video producing device is able
-- to ingest both audio and video media into a stream, while viewers can
-- only ingest audio. Both a video producing device and viewers can join
-- the session first, and wait for other participants.
--
-- While participants are having peer to peer conversations through webRTC,
-- the ingested media session will be stored into the Kinesis Video Stream.
-- Multiple viewers are able to playback real-time media.
--
-- Customers can also use existing Kinesis Video Streams features like
-- @HLS@ or @DASH@ playback, Image generation, and more with ingested
-- WebRTC media.
--
-- Assume that only one video producing device client can be associated
-- with a session for the channel. If more than one client joins the
-- session of a specific channel as a video producing device, the most
-- recent client request takes precedence.
module Amazonka.KinesisVideoWebRtcStorage.JoinStorageSession
  ( -- * Creating a Request
    JoinStorageSession (..),
    newJoinStorageSession,

    -- * Request Lenses
    joinStorageSession_channelArn,

    -- * Destructuring the Response
    JoinStorageSessionResponse (..),
    newJoinStorageSessionResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisVideoWebRtcStorage.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newJoinStorageSession' smart constructor.
data JoinStorageSession = JoinStorageSession'
  { -- | The Amazon Resource Name (ARN) of the signaling channel.
    channelArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JoinStorageSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelArn', 'joinStorageSession_channelArn' - The Amazon Resource Name (ARN) of the signaling channel.
newJoinStorageSession ::
  -- | 'channelArn'
  Prelude.Text ->
  JoinStorageSession
newJoinStorageSession pChannelArn_ =
  JoinStorageSession' {channelArn = pChannelArn_}

-- | The Amazon Resource Name (ARN) of the signaling channel.
joinStorageSession_channelArn :: Lens.Lens' JoinStorageSession Prelude.Text
joinStorageSession_channelArn = Lens.lens (\JoinStorageSession' {channelArn} -> channelArn) (\s@JoinStorageSession' {} a -> s {channelArn = a} :: JoinStorageSession)

instance Core.AWSRequest JoinStorageSession where
  type
    AWSResponse JoinStorageSession =
      JoinStorageSessionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull JoinStorageSessionResponse'

instance Prelude.Hashable JoinStorageSession where
  hashWithSalt _salt JoinStorageSession' {..} =
    _salt `Prelude.hashWithSalt` channelArn

instance Prelude.NFData JoinStorageSession where
  rnf JoinStorageSession' {..} = Prelude.rnf channelArn

instance Data.ToHeaders JoinStorageSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON JoinStorageSession where
  toJSON JoinStorageSession' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("channelArn" Data..= channelArn)]
      )

instance Data.ToPath JoinStorageSession where
  toPath = Prelude.const "/joinStorageSession"

instance Data.ToQuery JoinStorageSession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newJoinStorageSessionResponse' smart constructor.
data JoinStorageSessionResponse = JoinStorageSessionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JoinStorageSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newJoinStorageSessionResponse ::
  JoinStorageSessionResponse
newJoinStorageSessionResponse =
  JoinStorageSessionResponse'

instance Prelude.NFData JoinStorageSessionResponse where
  rnf _ = ()
