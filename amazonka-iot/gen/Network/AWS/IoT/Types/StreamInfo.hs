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
-- Module      : Network.AWS.IoT.Types.StreamInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.StreamInfo where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.StreamFile
import qualified Network.AWS.Lens as Lens

-- | Information about a stream.
--
-- /See:/ 'newStreamInfo' smart constructor.
data StreamInfo = StreamInfo'
  { -- | An IAM role AWS IoT assumes to access your S3 files.
    roleArn :: Core.Maybe Core.Text,
    -- | The stream version.
    streamVersion :: Core.Maybe Core.Natural,
    -- | The date when the stream was created.
    createdAt :: Core.Maybe Core.POSIX,
    -- | The stream ID.
    streamId :: Core.Maybe Core.Text,
    -- | The stream ARN.
    streamArn :: Core.Maybe Core.Text,
    -- | The description of the stream.
    description :: Core.Maybe Core.Text,
    -- | The files to stream.
    files :: Core.Maybe (Core.NonEmpty StreamFile),
    -- | The date when the stream was last updated.
    lastUpdatedAt :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StreamInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'streamInfo_roleArn' - An IAM role AWS IoT assumes to access your S3 files.
--
-- 'streamVersion', 'streamInfo_streamVersion' - The stream version.
--
-- 'createdAt', 'streamInfo_createdAt' - The date when the stream was created.
--
-- 'streamId', 'streamInfo_streamId' - The stream ID.
--
-- 'streamArn', 'streamInfo_streamArn' - The stream ARN.
--
-- 'description', 'streamInfo_description' - The description of the stream.
--
-- 'files', 'streamInfo_files' - The files to stream.
--
-- 'lastUpdatedAt', 'streamInfo_lastUpdatedAt' - The date when the stream was last updated.
newStreamInfo ::
  StreamInfo
newStreamInfo =
  StreamInfo'
    { roleArn = Core.Nothing,
      streamVersion = Core.Nothing,
      createdAt = Core.Nothing,
      streamId = Core.Nothing,
      streamArn = Core.Nothing,
      description = Core.Nothing,
      files = Core.Nothing,
      lastUpdatedAt = Core.Nothing
    }

-- | An IAM role AWS IoT assumes to access your S3 files.
streamInfo_roleArn :: Lens.Lens' StreamInfo (Core.Maybe Core.Text)
streamInfo_roleArn = Lens.lens (\StreamInfo' {roleArn} -> roleArn) (\s@StreamInfo' {} a -> s {roleArn = a} :: StreamInfo)

-- | The stream version.
streamInfo_streamVersion :: Lens.Lens' StreamInfo (Core.Maybe Core.Natural)
streamInfo_streamVersion = Lens.lens (\StreamInfo' {streamVersion} -> streamVersion) (\s@StreamInfo' {} a -> s {streamVersion = a} :: StreamInfo)

-- | The date when the stream was created.
streamInfo_createdAt :: Lens.Lens' StreamInfo (Core.Maybe Core.UTCTime)
streamInfo_createdAt = Lens.lens (\StreamInfo' {createdAt} -> createdAt) (\s@StreamInfo' {} a -> s {createdAt = a} :: StreamInfo) Core.. Lens.mapping Core._Time

-- | The stream ID.
streamInfo_streamId :: Lens.Lens' StreamInfo (Core.Maybe Core.Text)
streamInfo_streamId = Lens.lens (\StreamInfo' {streamId} -> streamId) (\s@StreamInfo' {} a -> s {streamId = a} :: StreamInfo)

-- | The stream ARN.
streamInfo_streamArn :: Lens.Lens' StreamInfo (Core.Maybe Core.Text)
streamInfo_streamArn = Lens.lens (\StreamInfo' {streamArn} -> streamArn) (\s@StreamInfo' {} a -> s {streamArn = a} :: StreamInfo)

-- | The description of the stream.
streamInfo_description :: Lens.Lens' StreamInfo (Core.Maybe Core.Text)
streamInfo_description = Lens.lens (\StreamInfo' {description} -> description) (\s@StreamInfo' {} a -> s {description = a} :: StreamInfo)

-- | The files to stream.
streamInfo_files :: Lens.Lens' StreamInfo (Core.Maybe (Core.NonEmpty StreamFile))
streamInfo_files = Lens.lens (\StreamInfo' {files} -> files) (\s@StreamInfo' {} a -> s {files = a} :: StreamInfo) Core.. Lens.mapping Lens._Coerce

-- | The date when the stream was last updated.
streamInfo_lastUpdatedAt :: Lens.Lens' StreamInfo (Core.Maybe Core.UTCTime)
streamInfo_lastUpdatedAt = Lens.lens (\StreamInfo' {lastUpdatedAt} -> lastUpdatedAt) (\s@StreamInfo' {} a -> s {lastUpdatedAt = a} :: StreamInfo) Core.. Lens.mapping Core._Time

instance Core.FromJSON StreamInfo where
  parseJSON =
    Core.withObject
      "StreamInfo"
      ( \x ->
          StreamInfo'
            Core.<$> (x Core..:? "roleArn")
            Core.<*> (x Core..:? "streamVersion")
            Core.<*> (x Core..:? "createdAt")
            Core.<*> (x Core..:? "streamId")
            Core.<*> (x Core..:? "streamArn")
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..:? "files")
            Core.<*> (x Core..:? "lastUpdatedAt")
      )

instance Core.Hashable StreamInfo

instance Core.NFData StreamInfo
