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
-- Module      : Amazonka.LookoutMetrics.Types.DetectedJsonFormatDescriptor
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.DetectedJsonFormatDescriptor where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LookoutMetrics.Types.DetectedField
import qualified Amazonka.Prelude as Prelude

-- | A detected JSON format descriptor.
--
-- /See:/ 'newDetectedJsonFormatDescriptor' smart constructor.
data DetectedJsonFormatDescriptor = DetectedJsonFormatDescriptor'
  { -- | The format\'s file compression.
    fileCompression :: Prelude.Maybe DetectedField,
    -- | The format\'s character set.
    charset :: Prelude.Maybe DetectedField
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectedJsonFormatDescriptor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileCompression', 'detectedJsonFormatDescriptor_fileCompression' - The format\'s file compression.
--
-- 'charset', 'detectedJsonFormatDescriptor_charset' - The format\'s character set.
newDetectedJsonFormatDescriptor ::
  DetectedJsonFormatDescriptor
newDetectedJsonFormatDescriptor =
  DetectedJsonFormatDescriptor'
    { fileCompression =
        Prelude.Nothing,
      charset = Prelude.Nothing
    }

-- | The format\'s file compression.
detectedJsonFormatDescriptor_fileCompression :: Lens.Lens' DetectedJsonFormatDescriptor (Prelude.Maybe DetectedField)
detectedJsonFormatDescriptor_fileCompression = Lens.lens (\DetectedJsonFormatDescriptor' {fileCompression} -> fileCompression) (\s@DetectedJsonFormatDescriptor' {} a -> s {fileCompression = a} :: DetectedJsonFormatDescriptor)

-- | The format\'s character set.
detectedJsonFormatDescriptor_charset :: Lens.Lens' DetectedJsonFormatDescriptor (Prelude.Maybe DetectedField)
detectedJsonFormatDescriptor_charset = Lens.lens (\DetectedJsonFormatDescriptor' {charset} -> charset) (\s@DetectedJsonFormatDescriptor' {} a -> s {charset = a} :: DetectedJsonFormatDescriptor)

instance Core.FromJSON DetectedJsonFormatDescriptor where
  parseJSON =
    Core.withObject
      "DetectedJsonFormatDescriptor"
      ( \x ->
          DetectedJsonFormatDescriptor'
            Prelude.<$> (x Core..:? "FileCompression")
            Prelude.<*> (x Core..:? "Charset")
      )

instance
  Prelude.Hashable
    DetectedJsonFormatDescriptor
  where
  hashWithSalt _salt DetectedJsonFormatDescriptor' {..} =
    _salt `Prelude.hashWithSalt` fileCompression
      `Prelude.hashWithSalt` charset

instance Prelude.NFData DetectedJsonFormatDescriptor where
  rnf DetectedJsonFormatDescriptor' {..} =
    Prelude.rnf fileCompression
      `Prelude.seq` Prelude.rnf charset
