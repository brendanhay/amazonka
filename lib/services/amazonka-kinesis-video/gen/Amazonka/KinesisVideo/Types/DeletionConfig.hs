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
-- Module      : Amazonka.KinesisVideo.Types.DeletionConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideo.Types.DeletionConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisVideo.Types.LocalSizeConfig
import qualified Amazonka.Prelude as Prelude

-- | The configuration details required to delete the connection of the
-- stream from the Edge Agent.
--
-- /See:/ 'newDeletionConfig' smart constructor.
data DeletionConfig = DeletionConfig'
  { -- | The @boolean@ value used to indicate whether or not you want to mark the
    -- media for deletion, once it has been uploaded to the Kinesis Video
    -- Stream cloud. The media files can be deleted if any of the deletion
    -- configuration values are set to @true@, such as when the limit for the
    -- @EdgeRetentionInHours@, or the @MaxLocalMediaSizeInMB@, has been
    -- reached.
    --
    -- Since the default value is set to @true@, configure the uploader
    -- schedule such that the media files are not being deleted before they are
    -- initially uploaded to AWS cloud.
    deleteAfterUpload :: Prelude.Maybe Prelude.Bool,
    -- | The number of hours that you want to retain the data in the stream on
    -- the Edge Agent. The default value of the retention time is 720 hours,
    -- which translates to 30 days.
    edgeRetentionInHours :: Prelude.Maybe Prelude.Natural,
    -- | The value of the local size required in order to delete the edge
    -- configuration.
    localSizeConfig :: Prelude.Maybe LocalSizeConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteAfterUpload', 'deletionConfig_deleteAfterUpload' - The @boolean@ value used to indicate whether or not you want to mark the
-- media for deletion, once it has been uploaded to the Kinesis Video
-- Stream cloud. The media files can be deleted if any of the deletion
-- configuration values are set to @true@, such as when the limit for the
-- @EdgeRetentionInHours@, or the @MaxLocalMediaSizeInMB@, has been
-- reached.
--
-- Since the default value is set to @true@, configure the uploader
-- schedule such that the media files are not being deleted before they are
-- initially uploaded to AWS cloud.
--
-- 'edgeRetentionInHours', 'deletionConfig_edgeRetentionInHours' - The number of hours that you want to retain the data in the stream on
-- the Edge Agent. The default value of the retention time is 720 hours,
-- which translates to 30 days.
--
-- 'localSizeConfig', 'deletionConfig_localSizeConfig' - The value of the local size required in order to delete the edge
-- configuration.
newDeletionConfig ::
  DeletionConfig
newDeletionConfig =
  DeletionConfig'
    { deleteAfterUpload =
        Prelude.Nothing,
      edgeRetentionInHours = Prelude.Nothing,
      localSizeConfig = Prelude.Nothing
    }

-- | The @boolean@ value used to indicate whether or not you want to mark the
-- media for deletion, once it has been uploaded to the Kinesis Video
-- Stream cloud. The media files can be deleted if any of the deletion
-- configuration values are set to @true@, such as when the limit for the
-- @EdgeRetentionInHours@, or the @MaxLocalMediaSizeInMB@, has been
-- reached.
--
-- Since the default value is set to @true@, configure the uploader
-- schedule such that the media files are not being deleted before they are
-- initially uploaded to AWS cloud.
deletionConfig_deleteAfterUpload :: Lens.Lens' DeletionConfig (Prelude.Maybe Prelude.Bool)
deletionConfig_deleteAfterUpload = Lens.lens (\DeletionConfig' {deleteAfterUpload} -> deleteAfterUpload) (\s@DeletionConfig' {} a -> s {deleteAfterUpload = a} :: DeletionConfig)

-- | The number of hours that you want to retain the data in the stream on
-- the Edge Agent. The default value of the retention time is 720 hours,
-- which translates to 30 days.
deletionConfig_edgeRetentionInHours :: Lens.Lens' DeletionConfig (Prelude.Maybe Prelude.Natural)
deletionConfig_edgeRetentionInHours = Lens.lens (\DeletionConfig' {edgeRetentionInHours} -> edgeRetentionInHours) (\s@DeletionConfig' {} a -> s {edgeRetentionInHours = a} :: DeletionConfig)

-- | The value of the local size required in order to delete the edge
-- configuration.
deletionConfig_localSizeConfig :: Lens.Lens' DeletionConfig (Prelude.Maybe LocalSizeConfig)
deletionConfig_localSizeConfig = Lens.lens (\DeletionConfig' {localSizeConfig} -> localSizeConfig) (\s@DeletionConfig' {} a -> s {localSizeConfig = a} :: DeletionConfig)

instance Data.FromJSON DeletionConfig where
  parseJSON =
    Data.withObject
      "DeletionConfig"
      ( \x ->
          DeletionConfig'
            Prelude.<$> (x Data..:? "DeleteAfterUpload")
            Prelude.<*> (x Data..:? "EdgeRetentionInHours")
            Prelude.<*> (x Data..:? "LocalSizeConfig")
      )

instance Prelude.Hashable DeletionConfig where
  hashWithSalt _salt DeletionConfig' {..} =
    _salt
      `Prelude.hashWithSalt` deleteAfterUpload
      `Prelude.hashWithSalt` edgeRetentionInHours
      `Prelude.hashWithSalt` localSizeConfig

instance Prelude.NFData DeletionConfig where
  rnf DeletionConfig' {..} =
    Prelude.rnf deleteAfterUpload `Prelude.seq`
      Prelude.rnf edgeRetentionInHours `Prelude.seq`
        Prelude.rnf localSizeConfig

instance Data.ToJSON DeletionConfig where
  toJSON DeletionConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeleteAfterUpload" Data..=)
              Prelude.<$> deleteAfterUpload,
            ("EdgeRetentionInHours" Data..=)
              Prelude.<$> edgeRetentionInHours,
            ("LocalSizeConfig" Data..=)
              Prelude.<$> localSizeConfig
          ]
      )
