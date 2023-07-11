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
-- Module      : Amazonka.IoT.Types.StreamFile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.StreamFile where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.S3Location
import qualified Amazonka.Prelude as Prelude

-- | Represents a file to stream.
--
-- /See:/ 'newStreamFile' smart constructor.
data StreamFile = StreamFile'
  { -- | The file ID.
    fileId :: Prelude.Maybe Prelude.Natural,
    -- | The location of the file in S3.
    s3Location :: Prelude.Maybe S3Location
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamFile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileId', 'streamFile_fileId' - The file ID.
--
-- 's3Location', 'streamFile_s3Location' - The location of the file in S3.
newStreamFile ::
  StreamFile
newStreamFile =
  StreamFile'
    { fileId = Prelude.Nothing,
      s3Location = Prelude.Nothing
    }

-- | The file ID.
streamFile_fileId :: Lens.Lens' StreamFile (Prelude.Maybe Prelude.Natural)
streamFile_fileId = Lens.lens (\StreamFile' {fileId} -> fileId) (\s@StreamFile' {} a -> s {fileId = a} :: StreamFile)

-- | The location of the file in S3.
streamFile_s3Location :: Lens.Lens' StreamFile (Prelude.Maybe S3Location)
streamFile_s3Location = Lens.lens (\StreamFile' {s3Location} -> s3Location) (\s@StreamFile' {} a -> s {s3Location = a} :: StreamFile)

instance Data.FromJSON StreamFile where
  parseJSON =
    Data.withObject
      "StreamFile"
      ( \x ->
          StreamFile'
            Prelude.<$> (x Data..:? "fileId")
            Prelude.<*> (x Data..:? "s3Location")
      )

instance Prelude.Hashable StreamFile where
  hashWithSalt _salt StreamFile' {..} =
    _salt
      `Prelude.hashWithSalt` fileId
      `Prelude.hashWithSalt` s3Location

instance Prelude.NFData StreamFile where
  rnf StreamFile' {..} =
    Prelude.rnf fileId
      `Prelude.seq` Prelude.rnf s3Location

instance Data.ToJSON StreamFile where
  toJSON StreamFile' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("fileId" Data..=) Prelude.<$> fileId,
            ("s3Location" Data..=) Prelude.<$> s3Location
          ]
      )
