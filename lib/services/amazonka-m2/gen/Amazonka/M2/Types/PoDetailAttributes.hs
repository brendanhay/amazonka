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
-- Module      : Amazonka.M2.Types.PoDetailAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.PoDetailAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The supported properties for a PO type data set.
--
-- /See:/ 'newPoDetailAttributes' smart constructor.
data PoDetailAttributes = PoDetailAttributes'
  { -- | The character set encoding of the data set.
    encoding :: Prelude.Text,
    -- | The format of the data set records.
    format :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PoDetailAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encoding', 'poDetailAttributes_encoding' - The character set encoding of the data set.
--
-- 'format', 'poDetailAttributes_format' - The format of the data set records.
newPoDetailAttributes ::
  -- | 'encoding'
  Prelude.Text ->
  -- | 'format'
  Prelude.Text ->
  PoDetailAttributes
newPoDetailAttributes pEncoding_ pFormat_ =
  PoDetailAttributes'
    { encoding = pEncoding_,
      format = pFormat_
    }

-- | The character set encoding of the data set.
poDetailAttributes_encoding :: Lens.Lens' PoDetailAttributes Prelude.Text
poDetailAttributes_encoding = Lens.lens (\PoDetailAttributes' {encoding} -> encoding) (\s@PoDetailAttributes' {} a -> s {encoding = a} :: PoDetailAttributes)

-- | The format of the data set records.
poDetailAttributes_format :: Lens.Lens' PoDetailAttributes Prelude.Text
poDetailAttributes_format = Lens.lens (\PoDetailAttributes' {format} -> format) (\s@PoDetailAttributes' {} a -> s {format = a} :: PoDetailAttributes)

instance Data.FromJSON PoDetailAttributes where
  parseJSON =
    Data.withObject
      "PoDetailAttributes"
      ( \x ->
          PoDetailAttributes'
            Prelude.<$> (x Data..: "encoding")
            Prelude.<*> (x Data..: "format")
      )

instance Prelude.Hashable PoDetailAttributes where
  hashWithSalt _salt PoDetailAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` encoding
      `Prelude.hashWithSalt` format

instance Prelude.NFData PoDetailAttributes where
  rnf PoDetailAttributes' {..} =
    Prelude.rnf encoding
      `Prelude.seq` Prelude.rnf format
