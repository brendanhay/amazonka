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
-- Module      : Amazonka.IoT.Types.Stream
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.Stream where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a group of files that can be streamed.
--
-- /See:/ 'newStream' smart constructor.
data Stream = Stream'
  { -- | The ID of a file associated with a stream.
    fileId :: Prelude.Maybe Prelude.Natural,
    -- | The stream ID.
    streamId :: Prelude.Maybe Prelude.Text
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
-- 'fileId', 'stream_fileId' - The ID of a file associated with a stream.
--
-- 'streamId', 'stream_streamId' - The stream ID.
newStream ::
  Stream
newStream =
  Stream'
    { fileId = Prelude.Nothing,
      streamId = Prelude.Nothing
    }

-- | The ID of a file associated with a stream.
stream_fileId :: Lens.Lens' Stream (Prelude.Maybe Prelude.Natural)
stream_fileId = Lens.lens (\Stream' {fileId} -> fileId) (\s@Stream' {} a -> s {fileId = a} :: Stream)

-- | The stream ID.
stream_streamId :: Lens.Lens' Stream (Prelude.Maybe Prelude.Text)
stream_streamId = Lens.lens (\Stream' {streamId} -> streamId) (\s@Stream' {} a -> s {streamId = a} :: Stream)

instance Data.FromJSON Stream where
  parseJSON =
    Data.withObject
      "Stream"
      ( \x ->
          Stream'
            Prelude.<$> (x Data..:? "fileId")
            Prelude.<*> (x Data..:? "streamId")
      )

instance Prelude.Hashable Stream where
  hashWithSalt _salt Stream' {..} =
    _salt `Prelude.hashWithSalt` fileId
      `Prelude.hashWithSalt` streamId

instance Prelude.NFData Stream where
  rnf Stream' {..} =
    Prelude.rnf fileId
      `Prelude.seq` Prelude.rnf streamId

instance Data.ToJSON Stream where
  toJSON Stream' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("fileId" Data..=) Prelude.<$> fileId,
            ("streamId" Data..=) Prelude.<$> streamId
          ]
      )
