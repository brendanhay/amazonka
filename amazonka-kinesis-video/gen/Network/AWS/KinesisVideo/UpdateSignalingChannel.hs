{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.KinesisVideo.UpdateSignalingChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the existing signaling channel. This is an asynchronous
-- operation and takes time to complete.
--
-- If the @MessageTtlSeconds@ value is updated (either increased or
-- reduced), it only applies to new messages sent via this channel after
-- it\'s been updated. Existing messages are still expired as per the
-- previous @MessageTtlSeconds@ value.
module Network.AWS.KinesisVideo.UpdateSignalingChannel
  ( -- * Creating a Request
    UpdateSignalingChannel (..),
    newUpdateSignalingChannel,

    -- * Request Lenses
    updateSignalingChannel_singleMasterConfiguration,
    updateSignalingChannel_channelARN,
    updateSignalingChannel_currentVersion,

    -- * Destructuring the Response
    UpdateSignalingChannelResponse (..),
    newUpdateSignalingChannelResponse,

    -- * Response Lenses
    updateSignalingChannelResponse_httpStatus,
  )
where

import Network.AWS.KinesisVideo.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateSignalingChannel' smart constructor.
data UpdateSignalingChannel = UpdateSignalingChannel'
  { -- | The structure containing the configuration for the @SINGLE_MASTER@ type
    -- of the signaling channel that you want to update.
    singleMasterConfiguration :: Prelude.Maybe SingleMasterConfiguration,
    -- | The Amazon Resource Name (ARN) of the signaling channel that you want to
    -- update.
    channelARN :: Prelude.Text,
    -- | The current version of the signaling channel that you want to update.
    currentVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateSignalingChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'singleMasterConfiguration', 'updateSignalingChannel_singleMasterConfiguration' - The structure containing the configuration for the @SINGLE_MASTER@ type
-- of the signaling channel that you want to update.
--
-- 'channelARN', 'updateSignalingChannel_channelARN' - The Amazon Resource Name (ARN) of the signaling channel that you want to
-- update.
--
-- 'currentVersion', 'updateSignalingChannel_currentVersion' - The current version of the signaling channel that you want to update.
newUpdateSignalingChannel ::
  -- | 'channelARN'
  Prelude.Text ->
  -- | 'currentVersion'
  Prelude.Text ->
  UpdateSignalingChannel
newUpdateSignalingChannel
  pChannelARN_
  pCurrentVersion_ =
    UpdateSignalingChannel'
      { singleMasterConfiguration =
          Prelude.Nothing,
        channelARN = pChannelARN_,
        currentVersion = pCurrentVersion_
      }

-- | The structure containing the configuration for the @SINGLE_MASTER@ type
-- of the signaling channel that you want to update.
updateSignalingChannel_singleMasterConfiguration :: Lens.Lens' UpdateSignalingChannel (Prelude.Maybe SingleMasterConfiguration)
updateSignalingChannel_singleMasterConfiguration = Lens.lens (\UpdateSignalingChannel' {singleMasterConfiguration} -> singleMasterConfiguration) (\s@UpdateSignalingChannel' {} a -> s {singleMasterConfiguration = a} :: UpdateSignalingChannel)

-- | The Amazon Resource Name (ARN) of the signaling channel that you want to
-- update.
updateSignalingChannel_channelARN :: Lens.Lens' UpdateSignalingChannel Prelude.Text
updateSignalingChannel_channelARN = Lens.lens (\UpdateSignalingChannel' {channelARN} -> channelARN) (\s@UpdateSignalingChannel' {} a -> s {channelARN = a} :: UpdateSignalingChannel)

-- | The current version of the signaling channel that you want to update.
updateSignalingChannel_currentVersion :: Lens.Lens' UpdateSignalingChannel Prelude.Text
updateSignalingChannel_currentVersion = Lens.lens (\UpdateSignalingChannel' {currentVersion} -> currentVersion) (\s@UpdateSignalingChannel' {} a -> s {currentVersion = a} :: UpdateSignalingChannel)

instance Prelude.AWSRequest UpdateSignalingChannel where
  type
    Rs UpdateSignalingChannel =
      UpdateSignalingChannelResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateSignalingChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSignalingChannel

instance Prelude.NFData UpdateSignalingChannel

instance Prelude.ToHeaders UpdateSignalingChannel where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON UpdateSignalingChannel where
  toJSON UpdateSignalingChannel' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SingleMasterConfiguration" Prelude..=)
              Prelude.<$> singleMasterConfiguration,
            Prelude.Just ("ChannelARN" Prelude..= channelARN),
            Prelude.Just
              ("CurrentVersion" Prelude..= currentVersion)
          ]
      )

instance Prelude.ToPath UpdateSignalingChannel where
  toPath = Prelude.const "/updateSignalingChannel"

instance Prelude.ToQuery UpdateSignalingChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSignalingChannelResponse' smart constructor.
data UpdateSignalingChannelResponse = UpdateSignalingChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateSignalingChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateSignalingChannelResponse_httpStatus' - The response's http status code.
newUpdateSignalingChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSignalingChannelResponse
newUpdateSignalingChannelResponse pHttpStatus_ =
  UpdateSignalingChannelResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateSignalingChannelResponse_httpStatus :: Lens.Lens' UpdateSignalingChannelResponse Prelude.Int
updateSignalingChannelResponse_httpStatus = Lens.lens (\UpdateSignalingChannelResponse' {httpStatus} -> httpStatus) (\s@UpdateSignalingChannelResponse' {} a -> s {httpStatus = a} :: UpdateSignalingChannelResponse)

instance
  Prelude.NFData
    UpdateSignalingChannelResponse
