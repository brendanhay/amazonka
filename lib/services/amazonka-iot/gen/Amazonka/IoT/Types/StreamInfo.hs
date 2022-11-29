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
-- Module      : Amazonka.IoT.Types.StreamInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.StreamInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types.StreamFile
import qualified Amazonka.Prelude as Prelude

-- | Information about a stream.
--
-- /See:/ 'newStreamInfo' smart constructor.
data StreamInfo = StreamInfo'
  { -- | The files to stream.
    files :: Prelude.Maybe (Prelude.NonEmpty StreamFile),
    -- | An IAM role IoT assumes to access your S3 files.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The date when the stream was last updated.
    lastUpdatedAt :: Prelude.Maybe Core.POSIX,
    -- | The stream ID.
    streamId :: Prelude.Maybe Prelude.Text,
    -- | The description of the stream.
    description :: Prelude.Maybe Prelude.Text,
    -- | The stream version.
    streamVersion :: Prelude.Maybe Prelude.Natural,
    -- | The stream ARN.
    streamArn :: Prelude.Maybe Prelude.Text,
    -- | The date when the stream was created.
    createdAt :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'files', 'streamInfo_files' - The files to stream.
--
-- 'roleArn', 'streamInfo_roleArn' - An IAM role IoT assumes to access your S3 files.
--
-- 'lastUpdatedAt', 'streamInfo_lastUpdatedAt' - The date when the stream was last updated.
--
-- 'streamId', 'streamInfo_streamId' - The stream ID.
--
-- 'description', 'streamInfo_description' - The description of the stream.
--
-- 'streamVersion', 'streamInfo_streamVersion' - The stream version.
--
-- 'streamArn', 'streamInfo_streamArn' - The stream ARN.
--
-- 'createdAt', 'streamInfo_createdAt' - The date when the stream was created.
newStreamInfo ::
  StreamInfo
newStreamInfo =
  StreamInfo'
    { files = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      streamId = Prelude.Nothing,
      description = Prelude.Nothing,
      streamVersion = Prelude.Nothing,
      streamArn = Prelude.Nothing,
      createdAt = Prelude.Nothing
    }

-- | The files to stream.
streamInfo_files :: Lens.Lens' StreamInfo (Prelude.Maybe (Prelude.NonEmpty StreamFile))
streamInfo_files = Lens.lens (\StreamInfo' {files} -> files) (\s@StreamInfo' {} a -> s {files = a} :: StreamInfo) Prelude.. Lens.mapping Lens.coerced

-- | An IAM role IoT assumes to access your S3 files.
streamInfo_roleArn :: Lens.Lens' StreamInfo (Prelude.Maybe Prelude.Text)
streamInfo_roleArn = Lens.lens (\StreamInfo' {roleArn} -> roleArn) (\s@StreamInfo' {} a -> s {roleArn = a} :: StreamInfo)

-- | The date when the stream was last updated.
streamInfo_lastUpdatedAt :: Lens.Lens' StreamInfo (Prelude.Maybe Prelude.UTCTime)
streamInfo_lastUpdatedAt = Lens.lens (\StreamInfo' {lastUpdatedAt} -> lastUpdatedAt) (\s@StreamInfo' {} a -> s {lastUpdatedAt = a} :: StreamInfo) Prelude.. Lens.mapping Core._Time

-- | The stream ID.
streamInfo_streamId :: Lens.Lens' StreamInfo (Prelude.Maybe Prelude.Text)
streamInfo_streamId = Lens.lens (\StreamInfo' {streamId} -> streamId) (\s@StreamInfo' {} a -> s {streamId = a} :: StreamInfo)

-- | The description of the stream.
streamInfo_description :: Lens.Lens' StreamInfo (Prelude.Maybe Prelude.Text)
streamInfo_description = Lens.lens (\StreamInfo' {description} -> description) (\s@StreamInfo' {} a -> s {description = a} :: StreamInfo)

-- | The stream version.
streamInfo_streamVersion :: Lens.Lens' StreamInfo (Prelude.Maybe Prelude.Natural)
streamInfo_streamVersion = Lens.lens (\StreamInfo' {streamVersion} -> streamVersion) (\s@StreamInfo' {} a -> s {streamVersion = a} :: StreamInfo)

-- | The stream ARN.
streamInfo_streamArn :: Lens.Lens' StreamInfo (Prelude.Maybe Prelude.Text)
streamInfo_streamArn = Lens.lens (\StreamInfo' {streamArn} -> streamArn) (\s@StreamInfo' {} a -> s {streamArn = a} :: StreamInfo)

-- | The date when the stream was created.
streamInfo_createdAt :: Lens.Lens' StreamInfo (Prelude.Maybe Prelude.UTCTime)
streamInfo_createdAt = Lens.lens (\StreamInfo' {createdAt} -> createdAt) (\s@StreamInfo' {} a -> s {createdAt = a} :: StreamInfo) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON StreamInfo where
  parseJSON =
    Core.withObject
      "StreamInfo"
      ( \x ->
          StreamInfo'
            Prelude.<$> (x Core..:? "files")
            Prelude.<*> (x Core..:? "roleArn")
            Prelude.<*> (x Core..:? "lastUpdatedAt")
            Prelude.<*> (x Core..:? "streamId")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "streamVersion")
            Prelude.<*> (x Core..:? "streamArn")
            Prelude.<*> (x Core..:? "createdAt")
      )

instance Prelude.Hashable StreamInfo where
  hashWithSalt _salt StreamInfo' {..} =
    _salt `Prelude.hashWithSalt` files
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` streamId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` streamVersion
      `Prelude.hashWithSalt` streamArn
      `Prelude.hashWithSalt` createdAt

instance Prelude.NFData StreamInfo where
  rnf StreamInfo' {..} =
    Prelude.rnf files
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf streamId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf streamVersion
      `Prelude.seq` Prelude.rnf streamArn
      `Prelude.seq` Prelude.rnf createdAt
