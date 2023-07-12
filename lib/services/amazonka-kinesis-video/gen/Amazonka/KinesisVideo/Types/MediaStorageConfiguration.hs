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
-- Module      : Amazonka.KinesisVideo.Types.MediaStorageConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideo.Types.MediaStorageConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisVideo.Types.MediaStorageConfigurationStatus
import qualified Amazonka.Prelude as Prelude

-- | A structure that encapsulates, or contains, the media storage
-- configuration properties.
--
-- /See:/ 'newMediaStorageConfiguration' smart constructor.
data MediaStorageConfiguration = MediaStorageConfiguration'
  { -- | The Amazon Resource Name (ARN) of the stream
    streamARN :: Prelude.Maybe Prelude.Text,
    -- | The status of the media storage configuration.
    status :: MediaStorageConfigurationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MediaStorageConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamARN', 'mediaStorageConfiguration_streamARN' - The Amazon Resource Name (ARN) of the stream
--
-- 'status', 'mediaStorageConfiguration_status' - The status of the media storage configuration.
newMediaStorageConfiguration ::
  -- | 'status'
  MediaStorageConfigurationStatus ->
  MediaStorageConfiguration
newMediaStorageConfiguration pStatus_ =
  MediaStorageConfiguration'
    { streamARN =
        Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Resource Name (ARN) of the stream
mediaStorageConfiguration_streamARN :: Lens.Lens' MediaStorageConfiguration (Prelude.Maybe Prelude.Text)
mediaStorageConfiguration_streamARN = Lens.lens (\MediaStorageConfiguration' {streamARN} -> streamARN) (\s@MediaStorageConfiguration' {} a -> s {streamARN = a} :: MediaStorageConfiguration)

-- | The status of the media storage configuration.
mediaStorageConfiguration_status :: Lens.Lens' MediaStorageConfiguration MediaStorageConfigurationStatus
mediaStorageConfiguration_status = Lens.lens (\MediaStorageConfiguration' {status} -> status) (\s@MediaStorageConfiguration' {} a -> s {status = a} :: MediaStorageConfiguration)

instance Data.FromJSON MediaStorageConfiguration where
  parseJSON =
    Data.withObject
      "MediaStorageConfiguration"
      ( \x ->
          MediaStorageConfiguration'
            Prelude.<$> (x Data..:? "StreamARN")
            Prelude.<*> (x Data..: "Status")
      )

instance Prelude.Hashable MediaStorageConfiguration where
  hashWithSalt _salt MediaStorageConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` streamARN
      `Prelude.hashWithSalt` status

instance Prelude.NFData MediaStorageConfiguration where
  rnf MediaStorageConfiguration' {..} =
    Prelude.rnf streamARN
      `Prelude.seq` Prelude.rnf status

instance Data.ToJSON MediaStorageConfiguration where
  toJSON MediaStorageConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("StreamARN" Data..=) Prelude.<$> streamARN,
            Prelude.Just ("Status" Data..= status)
          ]
      )
