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
-- Module      : Amazonka.LookoutMetrics.Types.JsonFormatDescriptor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.JsonFormatDescriptor where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.LookoutMetrics.Types.JsonFileCompression
import qualified Amazonka.Prelude as Prelude

-- | Contains information about how a source JSON data file should be
-- analyzed.
--
-- /See:/ 'newJsonFormatDescriptor' smart constructor.
data JsonFormatDescriptor = JsonFormatDescriptor'
  { -- | The character set in which the source JSON file is written.
    charset :: Prelude.Maybe Prelude.Text,
    -- | The level of compression of the source CSV file.
    fileCompression :: Prelude.Maybe JsonFileCompression
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JsonFormatDescriptor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'charset', 'jsonFormatDescriptor_charset' - The character set in which the source JSON file is written.
--
-- 'fileCompression', 'jsonFormatDescriptor_fileCompression' - The level of compression of the source CSV file.
newJsonFormatDescriptor ::
  JsonFormatDescriptor
newJsonFormatDescriptor =
  JsonFormatDescriptor'
    { charset = Prelude.Nothing,
      fileCompression = Prelude.Nothing
    }

-- | The character set in which the source JSON file is written.
jsonFormatDescriptor_charset :: Lens.Lens' JsonFormatDescriptor (Prelude.Maybe Prelude.Text)
jsonFormatDescriptor_charset = Lens.lens (\JsonFormatDescriptor' {charset} -> charset) (\s@JsonFormatDescriptor' {} a -> s {charset = a} :: JsonFormatDescriptor)

-- | The level of compression of the source CSV file.
jsonFormatDescriptor_fileCompression :: Lens.Lens' JsonFormatDescriptor (Prelude.Maybe JsonFileCompression)
jsonFormatDescriptor_fileCompression = Lens.lens (\JsonFormatDescriptor' {fileCompression} -> fileCompression) (\s@JsonFormatDescriptor' {} a -> s {fileCompression = a} :: JsonFormatDescriptor)

instance Core.FromJSON JsonFormatDescriptor where
  parseJSON =
    Core.withObject
      "JsonFormatDescriptor"
      ( \x ->
          JsonFormatDescriptor'
            Prelude.<$> (x Core..:? "Charset")
            Prelude.<*> (x Core..:? "FileCompression")
      )

instance Prelude.Hashable JsonFormatDescriptor

instance Prelude.NFData JsonFormatDescriptor

instance Core.ToJSON JsonFormatDescriptor where
  toJSON JsonFormatDescriptor' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Charset" Core..=) Prelude.<$> charset,
            ("FileCompression" Core..=)
              Prelude.<$> fileCompression
          ]
      )
