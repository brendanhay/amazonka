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
-- Module      : Amazonka.IoTAnalytics.Types.OutputFileUriValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.OutputFileUriValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The value of the variable as a structure that specifies an output file
-- URI.
--
-- /See:/ 'newOutputFileUriValue' smart constructor.
data OutputFileUriValue = OutputFileUriValue'
  { -- | The URI of the location where dataset contents are stored, usually the
    -- URI of a file in an S3 bucket.
    fileName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON OutputFileUriValue where
  parseJSON =
    Data.withObject
      "OutputFileUriValue"
      ( \x ->
          OutputFileUriValue'
            Prelude.<$> (x Data..: "fileName")
      )

instance Prelude.Hashable OutputFileUriValue where
  hashWithSalt _salt OutputFileUriValue' {..} =
    _salt `Prelude.hashWithSalt` fileName

instance Prelude.NFData OutputFileUriValue where
  rnf OutputFileUriValue' {..} = Prelude.rnf fileName

instance Data.ToJSON OutputFileUriValue where
  toJSON OutputFileUriValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("fileName" Data..= fileName)]
      )
