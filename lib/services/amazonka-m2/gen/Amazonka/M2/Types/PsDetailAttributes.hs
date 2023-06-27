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
-- Module      : Amazonka.M2.Types.PsDetailAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.PsDetailAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The supported properties for a PS type data set.
--
-- /See:/ 'newPsDetailAttributes' smart constructor.
data PsDetailAttributes = PsDetailAttributes'
  { -- | The character set encoding of the data set.
    encoding :: Prelude.Text,
    -- | The format of the data set records.
    format :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PsDetailAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encoding', 'psDetailAttributes_encoding' - The character set encoding of the data set.
--
-- 'format', 'psDetailAttributes_format' - The format of the data set records.
newPsDetailAttributes ::
  -- | 'encoding'
  Prelude.Text ->
  -- | 'format'
  Prelude.Text ->
  PsDetailAttributes
newPsDetailAttributes pEncoding_ pFormat_ =
  PsDetailAttributes'
    { encoding = pEncoding_,
      format = pFormat_
    }

-- | The character set encoding of the data set.
psDetailAttributes_encoding :: Lens.Lens' PsDetailAttributes Prelude.Text
psDetailAttributes_encoding = Lens.lens (\PsDetailAttributes' {encoding} -> encoding) (\s@PsDetailAttributes' {} a -> s {encoding = a} :: PsDetailAttributes)

-- | The format of the data set records.
psDetailAttributes_format :: Lens.Lens' PsDetailAttributes Prelude.Text
psDetailAttributes_format = Lens.lens (\PsDetailAttributes' {format} -> format) (\s@PsDetailAttributes' {} a -> s {format = a} :: PsDetailAttributes)

instance Data.FromJSON PsDetailAttributes where
  parseJSON =
    Data.withObject
      "PsDetailAttributes"
      ( \x ->
          PsDetailAttributes'
            Prelude.<$> (x Data..: "encoding")
            Prelude.<*> (x Data..: "format")
      )

instance Prelude.Hashable PsDetailAttributes where
  hashWithSalt _salt PsDetailAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` encoding
      `Prelude.hashWithSalt` format

instance Prelude.NFData PsDetailAttributes where
  rnf PsDetailAttributes' {..} =
    Prelude.rnf encoding
      `Prelude.seq` Prelude.rnf format
