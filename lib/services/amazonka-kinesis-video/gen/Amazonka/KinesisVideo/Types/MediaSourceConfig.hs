{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.KinesisVideo.Types.MediaSourceConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideo.Types.MediaSourceConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisVideo.Types.MediaUriType
import qualified Amazonka.Prelude as Prelude

-- | The configuration details that consist of the credentials required
-- (@MediaUriSecretArn@ and @MediaUriType@) to access the media files that
-- are streamed to the camera.
--
-- /See:/ 'newMediaSourceConfig' smart constructor.
data MediaSourceConfig = MediaSourceConfig'
  { -- | The AWS Secrets Manager ARN for the username and password of the camera,
    -- or a local media file location.
    mediaUriSecretArn :: Data.Sensitive Prelude.Text,
    -- | The Uniform Resource Identifier (URI) type. The @FILE_URI@ value can be
    -- used to stream local media files.
    --
    -- Preview only supports the @RTSP_URI@ media source URI format .
    mediaUriType :: MediaUriType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MediaSourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mediaUriSecretArn', 'mediaSourceConfig_mediaUriSecretArn' - The AWS Secrets Manager ARN for the username and password of the camera,
-- or a local media file location.
--
-- 'mediaUriType', 'mediaSourceConfig_mediaUriType' - The Uniform Resource Identifier (URI) type. The @FILE_URI@ value can be
-- used to stream local media files.
--
-- Preview only supports the @RTSP_URI@ media source URI format .
newMediaSourceConfig ::
  -- | 'mediaUriSecretArn'
  Prelude.Text ->
  -- | 'mediaUriType'
  MediaUriType ->
  MediaSourceConfig
newMediaSourceConfig
  pMediaUriSecretArn_
  pMediaUriType_ =
    MediaSourceConfig'
      { mediaUriSecretArn =
          Data._Sensitive Lens.# pMediaUriSecretArn_,
        mediaUriType = pMediaUriType_
      }

-- | The AWS Secrets Manager ARN for the username and password of the camera,
-- or a local media file location.
mediaSourceConfig_mediaUriSecretArn :: Lens.Lens' MediaSourceConfig Prelude.Text
mediaSourceConfig_mediaUriSecretArn = Lens.lens (\MediaSourceConfig' {mediaUriSecretArn} -> mediaUriSecretArn) (\s@MediaSourceConfig' {} a -> s {mediaUriSecretArn = a} :: MediaSourceConfig) Prelude.. Data._Sensitive

-- | The Uniform Resource Identifier (URI) type. The @FILE_URI@ value can be
-- used to stream local media files.
--
-- Preview only supports the @RTSP_URI@ media source URI format .
mediaSourceConfig_mediaUriType :: Lens.Lens' MediaSourceConfig MediaUriType
mediaSourceConfig_mediaUriType = Lens.lens (\MediaSourceConfig' {mediaUriType} -> mediaUriType) (\s@MediaSourceConfig' {} a -> s {mediaUriType = a} :: MediaSourceConfig)

instance Data.FromJSON MediaSourceConfig where
  parseJSON =
    Data.withObject
      "MediaSourceConfig"
      ( \x ->
          MediaSourceConfig'
            Prelude.<$> (x Data..: "MediaUriSecretArn")
            Prelude.<*> (x Data..: "MediaUriType")
      )

instance Prelude.Hashable MediaSourceConfig where
  hashWithSalt _salt MediaSourceConfig' {..} =
    _salt `Prelude.hashWithSalt` mediaUriSecretArn
      `Prelude.hashWithSalt` mediaUriType

instance Prelude.NFData MediaSourceConfig where
  rnf MediaSourceConfig' {..} =
    Prelude.rnf mediaUriSecretArn
      `Prelude.seq` Prelude.rnf mediaUriType

instance Data.ToJSON MediaSourceConfig where
  toJSON MediaSourceConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("MediaUriSecretArn" Data..= mediaUriSecretArn),
            Prelude.Just ("MediaUriType" Data..= mediaUriType)
          ]
      )
