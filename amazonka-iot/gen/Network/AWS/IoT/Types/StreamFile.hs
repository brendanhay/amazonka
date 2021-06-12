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
-- Module      : Network.AWS.IoT.Types.StreamFile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.StreamFile where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.S3Location
import qualified Network.AWS.Lens as Lens

-- | Represents a file to stream.
--
-- /See:/ 'newStreamFile' smart constructor.
data StreamFile = StreamFile'
  { -- | The location of the file in S3.
    s3Location :: Core.Maybe S3Location,
    -- | The file ID.
    fileId :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StreamFile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Location', 'streamFile_s3Location' - The location of the file in S3.
--
-- 'fileId', 'streamFile_fileId' - The file ID.
newStreamFile ::
  StreamFile
newStreamFile =
  StreamFile'
    { s3Location = Core.Nothing,
      fileId = Core.Nothing
    }

-- | The location of the file in S3.
streamFile_s3Location :: Lens.Lens' StreamFile (Core.Maybe S3Location)
streamFile_s3Location = Lens.lens (\StreamFile' {s3Location} -> s3Location) (\s@StreamFile' {} a -> s {s3Location = a} :: StreamFile)

-- | The file ID.
streamFile_fileId :: Lens.Lens' StreamFile (Core.Maybe Core.Natural)
streamFile_fileId = Lens.lens (\StreamFile' {fileId} -> fileId) (\s@StreamFile' {} a -> s {fileId = a} :: StreamFile)

instance Core.FromJSON StreamFile where
  parseJSON =
    Core.withObject
      "StreamFile"
      ( \x ->
          StreamFile'
            Core.<$> (x Core..:? "s3Location")
            Core.<*> (x Core..:? "fileId")
      )

instance Core.Hashable StreamFile

instance Core.NFData StreamFile

instance Core.ToJSON StreamFile where
  toJSON StreamFile' {..} =
    Core.object
      ( Core.catMaybes
          [ ("s3Location" Core..=) Core.<$> s3Location,
            ("fileId" Core..=) Core.<$> fileId
          ]
      )
