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
-- Module      : Amazonka.KinesisVideo.UpdateSignalingChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.KinesisVideo.UpdateSignalingChannel
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisVideo.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest UpdateSignalingChannel where
  type
    AWSResponse UpdateSignalingChannel =
      UpdateSignalingChannelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateSignalingChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSignalingChannel where
  hashWithSalt _salt UpdateSignalingChannel' {..} =
    _salt
      `Prelude.hashWithSalt` singleMasterConfiguration
      `Prelude.hashWithSalt` channelARN
      `Prelude.hashWithSalt` currentVersion

instance Prelude.NFData UpdateSignalingChannel where
  rnf UpdateSignalingChannel' {..} =
    Prelude.rnf singleMasterConfiguration
      `Prelude.seq` Prelude.rnf channelARN
      `Prelude.seq` Prelude.rnf currentVersion

instance Data.ToHeaders UpdateSignalingChannel where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateSignalingChannel where
  toJSON UpdateSignalingChannel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SingleMasterConfiguration" Data..=)
              Prelude.<$> singleMasterConfiguration,
            Prelude.Just ("ChannelARN" Data..= channelARN),
            Prelude.Just
              ("CurrentVersion" Data..= currentVersion)
          ]
      )

instance Data.ToPath UpdateSignalingChannel where
  toPath = Prelude.const "/updateSignalingChannel"

instance Data.ToQuery UpdateSignalingChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSignalingChannelResponse' smart constructor.
data UpdateSignalingChannelResponse = UpdateSignalingChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf UpdateSignalingChannelResponse' {..} =
    Prelude.rnf httpStatus
