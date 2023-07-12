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
-- Module      : Amazonka.KinesisVideo.UpdateMediaStorageConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a @SignalingChannel@ to a stream to store the media. There
-- are two signaling modes that can specified :
--
-- -   If the @StorageStatus@ is disabled, no data will be stored, and the
--     @StreamARN@ parameter will not be needed.
--
-- -   If the @StorageStatus@ is enabled, the data will be stored in the
--     @StreamARN@ provided.
module Amazonka.KinesisVideo.UpdateMediaStorageConfiguration
  ( -- * Creating a Request
    UpdateMediaStorageConfiguration (..),
    newUpdateMediaStorageConfiguration,

    -- * Request Lenses
    updateMediaStorageConfiguration_channelARN,
    updateMediaStorageConfiguration_mediaStorageConfiguration,

    -- * Destructuring the Response
    UpdateMediaStorageConfigurationResponse (..),
    newUpdateMediaStorageConfigurationResponse,

    -- * Response Lenses
    updateMediaStorageConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisVideo.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateMediaStorageConfiguration' smart constructor.
data UpdateMediaStorageConfiguration = UpdateMediaStorageConfiguration'
  { -- | The Amazon Resource Name (ARN) of the channel.
    channelARN :: Prelude.Text,
    -- | A structure that encapsulates, or contains, the media storage
    -- configuration properties.
    mediaStorageConfiguration :: MediaStorageConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMediaStorageConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelARN', 'updateMediaStorageConfiguration_channelARN' - The Amazon Resource Name (ARN) of the channel.
--
-- 'mediaStorageConfiguration', 'updateMediaStorageConfiguration_mediaStorageConfiguration' - A structure that encapsulates, or contains, the media storage
-- configuration properties.
newUpdateMediaStorageConfiguration ::
  -- | 'channelARN'
  Prelude.Text ->
  -- | 'mediaStorageConfiguration'
  MediaStorageConfiguration ->
  UpdateMediaStorageConfiguration
newUpdateMediaStorageConfiguration
  pChannelARN_
  pMediaStorageConfiguration_ =
    UpdateMediaStorageConfiguration'
      { channelARN =
          pChannelARN_,
        mediaStorageConfiguration =
          pMediaStorageConfiguration_
      }

-- | The Amazon Resource Name (ARN) of the channel.
updateMediaStorageConfiguration_channelARN :: Lens.Lens' UpdateMediaStorageConfiguration Prelude.Text
updateMediaStorageConfiguration_channelARN = Lens.lens (\UpdateMediaStorageConfiguration' {channelARN} -> channelARN) (\s@UpdateMediaStorageConfiguration' {} a -> s {channelARN = a} :: UpdateMediaStorageConfiguration)

-- | A structure that encapsulates, or contains, the media storage
-- configuration properties.
updateMediaStorageConfiguration_mediaStorageConfiguration :: Lens.Lens' UpdateMediaStorageConfiguration MediaStorageConfiguration
updateMediaStorageConfiguration_mediaStorageConfiguration = Lens.lens (\UpdateMediaStorageConfiguration' {mediaStorageConfiguration} -> mediaStorageConfiguration) (\s@UpdateMediaStorageConfiguration' {} a -> s {mediaStorageConfiguration = a} :: UpdateMediaStorageConfiguration)

instance
  Core.AWSRequest
    UpdateMediaStorageConfiguration
  where
  type
    AWSResponse UpdateMediaStorageConfiguration =
      UpdateMediaStorageConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateMediaStorageConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateMediaStorageConfiguration
  where
  hashWithSalt
    _salt
    UpdateMediaStorageConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` channelARN
        `Prelude.hashWithSalt` mediaStorageConfiguration

instance
  Prelude.NFData
    UpdateMediaStorageConfiguration
  where
  rnf UpdateMediaStorageConfiguration' {..} =
    Prelude.rnf channelARN
      `Prelude.seq` Prelude.rnf mediaStorageConfiguration

instance
  Data.ToHeaders
    UpdateMediaStorageConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateMediaStorageConfiguration where
  toJSON UpdateMediaStorageConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ChannelARN" Data..= channelARN),
            Prelude.Just
              ( "MediaStorageConfiguration"
                  Data..= mediaStorageConfiguration
              )
          ]
      )

instance Data.ToPath UpdateMediaStorageConfiguration where
  toPath =
    Prelude.const "/updateMediaStorageConfiguration"

instance Data.ToQuery UpdateMediaStorageConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateMediaStorageConfigurationResponse' smart constructor.
data UpdateMediaStorageConfigurationResponse = UpdateMediaStorageConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMediaStorageConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateMediaStorageConfigurationResponse_httpStatus' - The response's http status code.
newUpdateMediaStorageConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateMediaStorageConfigurationResponse
newUpdateMediaStorageConfigurationResponse
  pHttpStatus_ =
    UpdateMediaStorageConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateMediaStorageConfigurationResponse_httpStatus :: Lens.Lens' UpdateMediaStorageConfigurationResponse Prelude.Int
updateMediaStorageConfigurationResponse_httpStatus = Lens.lens (\UpdateMediaStorageConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateMediaStorageConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateMediaStorageConfigurationResponse)

instance
  Prelude.NFData
    UpdateMediaStorageConfigurationResponse
  where
  rnf UpdateMediaStorageConfigurationResponse' {..} =
    Prelude.rnf httpStatus
