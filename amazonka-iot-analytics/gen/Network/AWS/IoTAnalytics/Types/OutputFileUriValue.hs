{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoTAnalytics.Types.OutputFileUriValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.OutputFileUriValue where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The value of the variable as a structure that specifies an output file
-- URI.
--
-- /See:/ 'newOutputFileUriValue' smart constructor.
data OutputFileUriValue = OutputFileUriValue'
  { -- | The URI of the location where dataset contents are stored, usually the
    -- URI of a file in an S3 bucket.
    fileName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OutputFileUriValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileName', 'outputFileUriValue_fileName' - The URI of the location where dataset contents are stored, usually the
-- URI of a file in an S3 bucket.
newOutputFileUriValue ::
  -- | 'fileName'
  Prelude.Text ->
  OutputFileUriValue
newOutputFileUriValue pFileName_ =
  OutputFileUriValue' {fileName = pFileName_}

-- | The URI of the location where dataset contents are stored, usually the
-- URI of a file in an S3 bucket.
outputFileUriValue_fileName :: Lens.Lens' OutputFileUriValue Prelude.Text
outputFileUriValue_fileName = Lens.lens (\OutputFileUriValue' {fileName} -> fileName) (\s@OutputFileUriValue' {} a -> s {fileName = a} :: OutputFileUriValue)

instance Prelude.FromJSON OutputFileUriValue where
  parseJSON =
    Prelude.withObject
      "OutputFileUriValue"
      ( \x ->
          OutputFileUriValue'
            Prelude.<$> (x Prelude..: "fileName")
      )

instance Prelude.Hashable OutputFileUriValue

instance Prelude.NFData OutputFileUriValue

instance Prelude.ToJSON OutputFileUriValue where
  toJSON OutputFileUriValue' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("fileName" Prelude..= fileName)]
      )
