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
-- Module      : Amazonka.Omics.Types.FormatOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.FormatOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.TsvOptions
import Amazonka.Omics.Types.VcfOptions
import qualified Amazonka.Prelude as Prelude

-- | Formatting options for a file.
--
-- /See:/ 'newFormatOptions' smart constructor.
data FormatOptions = FormatOptions'
  { -- | Options for a TSV file.
    tsvOptions :: Prelude.Maybe TsvOptions,
    -- | Options for a VCF file.
    vcfOptions :: Prelude.Maybe VcfOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FormatOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tsvOptions', 'formatOptions_tsvOptions' - Options for a TSV file.
--
-- 'vcfOptions', 'formatOptions_vcfOptions' - Options for a VCF file.
newFormatOptions ::
  FormatOptions
newFormatOptions =
  FormatOptions'
    { tsvOptions = Prelude.Nothing,
      vcfOptions = Prelude.Nothing
    }

-- | Options for a TSV file.
formatOptions_tsvOptions :: Lens.Lens' FormatOptions (Prelude.Maybe TsvOptions)
formatOptions_tsvOptions = Lens.lens (\FormatOptions' {tsvOptions} -> tsvOptions) (\s@FormatOptions' {} a -> s {tsvOptions = a} :: FormatOptions)

-- | Options for a VCF file.
formatOptions_vcfOptions :: Lens.Lens' FormatOptions (Prelude.Maybe VcfOptions)
formatOptions_vcfOptions = Lens.lens (\FormatOptions' {vcfOptions} -> vcfOptions) (\s@FormatOptions' {} a -> s {vcfOptions = a} :: FormatOptions)

instance Data.FromJSON FormatOptions where
  parseJSON =
    Data.withObject
      "FormatOptions"
      ( \x ->
          FormatOptions'
            Prelude.<$> (x Data..:? "tsvOptions")
            Prelude.<*> (x Data..:? "vcfOptions")
      )

instance Prelude.Hashable FormatOptions where
  hashWithSalt _salt FormatOptions' {..} =
    _salt
      `Prelude.hashWithSalt` tsvOptions
      `Prelude.hashWithSalt` vcfOptions

instance Prelude.NFData FormatOptions where
  rnf FormatOptions' {..} =
    Prelude.rnf tsvOptions
      `Prelude.seq` Prelude.rnf vcfOptions

instance Data.ToJSON FormatOptions where
  toJSON FormatOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tsvOptions" Data..=) Prelude.<$> tsvOptions,
            ("vcfOptions" Data..=) Prelude.<$> vcfOptions
          ]
      )
