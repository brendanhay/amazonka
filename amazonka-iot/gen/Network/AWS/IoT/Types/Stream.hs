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
import qualified Network.AWS.Prelude as Prelude

-- | Describes a group of files that can be streamed.
--
-- /See:/ 'newStream' smart constructor.
data Stream = Stream'
  { -- | The stream ID.
    streamId :: Prelude.Maybe Prelude.Text,
    -- | The ID of a file associated with a stream.
    fileId :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { streamId = Prelude.Nothing,
      fileId = Prelude.Nothing
    }

-- | The stream ID.
stream_streamId :: Lens.Lens' Stream (Prelude.Maybe Prelude.Text)
stream_streamId = Lens.lens (\Stream' {streamId} -> streamId) (\s@Stream' {} a -> s {streamId = a} :: Stream)

-- | The ID of a file associated with a stream.
stream_fileId :: Lens.Lens' Stream (Prelude.Maybe Prelude.Natural)
stream_fileId = Lens.lens (\Stream' {fileId} -> fileId) (\s@Stream' {} a -> s {fileId = a} :: Stream)

instance Core.FromJSON Stream where
  parseJSON =
    Core.withObject
      "Stream"
      ( \x ->
          Stream'
            Prelude.<$> (x Core..:? "streamId")
            Prelude.<*> (x Core..:? "fileId")
      )

instance Prelude.Hashable Stream

instance Prelude.NFData Stream

instance Core.ToJSON Stream where
  toJSON Stream' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("streamId" Core..=) Prelude.<$> streamId,
            ("fileId" Core..=) Prelude.<$> fileId
          ]
      )
