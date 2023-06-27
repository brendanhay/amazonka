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
-- Module      : Amazonka.M2.Types.PsAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.PsAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The supported properties for a PS type data set.
--
-- /See:/ 'newPsAttributes' smart constructor.
data PsAttributes = PsAttributes'
  { -- | The character set encoding of the data set.
    encoding :: Prelude.Maybe Prelude.Text,
    -- | The format of the data set records.
    format :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PsAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encoding', 'psAttributes_encoding' - The character set encoding of the data set.
--
-- 'format', 'psAttributes_format' - The format of the data set records.
newPsAttributes ::
  -- | 'format'
  Prelude.Text ->
  PsAttributes
newPsAttributes pFormat_ =
  PsAttributes'
    { encoding = Prelude.Nothing,
      format = pFormat_
    }

-- | The character set encoding of the data set.
psAttributes_encoding :: Lens.Lens' PsAttributes (Prelude.Maybe Prelude.Text)
psAttributes_encoding = Lens.lens (\PsAttributes' {encoding} -> encoding) (\s@PsAttributes' {} a -> s {encoding = a} :: PsAttributes)

-- | The format of the data set records.
psAttributes_format :: Lens.Lens' PsAttributes Prelude.Text
psAttributes_format = Lens.lens (\PsAttributes' {format} -> format) (\s@PsAttributes' {} a -> s {format = a} :: PsAttributes)

instance Prelude.Hashable PsAttributes where
  hashWithSalt _salt PsAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` encoding
      `Prelude.hashWithSalt` format

instance Prelude.NFData PsAttributes where
  rnf PsAttributes' {..} =
    Prelude.rnf encoding
      `Prelude.seq` Prelude.rnf format

instance Data.ToJSON PsAttributes where
  toJSON PsAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("encoding" Data..=) Prelude.<$> encoding,
            Prelude.Just ("format" Data..= format)
          ]
      )
