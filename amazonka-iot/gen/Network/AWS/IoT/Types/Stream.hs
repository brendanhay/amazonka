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
-- Module      : Network.AWS.IoT.Types.Stream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Stream where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes a group of files that can be streamed.
--
-- /See:/ 'newStream' smart constructor.
data Stream = Stream'
  { -- | The stream ID.
    streamId :: Core.Maybe Core.Text,
    -- | The ID of a file associated with a stream.
    fileId :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Stream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamId', 'stream_streamId' - The stream ID.
--
-- 'fileId', 'stream_fileId' - The ID of a file associated with a stream.
newStream ::
  Stream
newStream =
  Stream'
    { streamId = Core.Nothing,
      fileId = Core.Nothing
    }

-- | The stream ID.
stream_streamId :: Lens.Lens' Stream (Core.Maybe Core.Text)
stream_streamId = Lens.lens (\Stream' {streamId} -> streamId) (\s@Stream' {} a -> s {streamId = a} :: Stream)

-- | The ID of a file associated with a stream.
stream_fileId :: Lens.Lens' Stream (Core.Maybe Core.Natural)
stream_fileId = Lens.lens (\Stream' {fileId} -> fileId) (\s@Stream' {} a -> s {fileId = a} :: Stream)

instance Core.FromJSON Stream where
  parseJSON =
    Core.withObject
      "Stream"
      ( \x ->
          Stream'
            Core.<$> (x Core..:? "streamId")
            Core.<*> (x Core..:? "fileId")
      )

instance Core.Hashable Stream

instance Core.NFData Stream

instance Core.ToJSON Stream where
  toJSON Stream' {..} =
    Core.object
      ( Core.catMaybes
          [ ("streamId" Core..=) Core.<$> streamId,
            ("fileId" Core..=) Core.<$> fileId
          ]
      )
