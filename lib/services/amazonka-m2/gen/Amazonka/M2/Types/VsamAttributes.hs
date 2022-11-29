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
-- Module      : Amazonka.M2.Types.VsamAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.VsamAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.M2.Types.AlternateKey
import Amazonka.M2.Types.PrimaryKey
import qualified Amazonka.Prelude as Prelude

-- | The attributes of a VSAM type data set.
--
-- /See:/ 'newVsamAttributes' smart constructor.
data VsamAttributes = VsamAttributes'
  { -- | The primary key of the data set.
    primaryKey :: Prelude.Maybe PrimaryKey,
    -- | The character set used by the data set. Can be ASCII, EBCDIC, or
    -- unknown.
    encoding :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether indexes for this dataset are stored as compressed
    -- values. If you have a large data set (typically > 100 Mb), consider
    -- setting this flag to True.
    compressed :: Prelude.Maybe Prelude.Bool,
    -- | The alternate key definitions, if any. A legacy dataset might not have
    -- any alternate key defined, but if those alternate keys definitions
    -- exist, provide them as some applications will make use of them.
    alternateKeys :: Prelude.Maybe [AlternateKey],
    -- | The record format of the data set.
    format :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VsamAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'primaryKey', 'vsamAttributes_primaryKey' - The primary key of the data set.
--
-- 'encoding', 'vsamAttributes_encoding' - The character set used by the data set. Can be ASCII, EBCDIC, or
-- unknown.
--
-- 'compressed', 'vsamAttributes_compressed' - Indicates whether indexes for this dataset are stored as compressed
-- values. If you have a large data set (typically > 100 Mb), consider
-- setting this flag to True.
--
-- 'alternateKeys', 'vsamAttributes_alternateKeys' - The alternate key definitions, if any. A legacy dataset might not have
-- any alternate key defined, but if those alternate keys definitions
-- exist, provide them as some applications will make use of them.
--
-- 'format', 'vsamAttributes_format' - The record format of the data set.
newVsamAttributes ::
  -- | 'format'
  Prelude.Text ->
  VsamAttributes
newVsamAttributes pFormat_ =
  VsamAttributes'
    { primaryKey = Prelude.Nothing,
      encoding = Prelude.Nothing,
      compressed = Prelude.Nothing,
      alternateKeys = Prelude.Nothing,
      format = pFormat_
    }

-- | The primary key of the data set.
vsamAttributes_primaryKey :: Lens.Lens' VsamAttributes (Prelude.Maybe PrimaryKey)
vsamAttributes_primaryKey = Lens.lens (\VsamAttributes' {primaryKey} -> primaryKey) (\s@VsamAttributes' {} a -> s {primaryKey = a} :: VsamAttributes)

-- | The character set used by the data set. Can be ASCII, EBCDIC, or
-- unknown.
vsamAttributes_encoding :: Lens.Lens' VsamAttributes (Prelude.Maybe Prelude.Text)
vsamAttributes_encoding = Lens.lens (\VsamAttributes' {encoding} -> encoding) (\s@VsamAttributes' {} a -> s {encoding = a} :: VsamAttributes)

-- | Indicates whether indexes for this dataset are stored as compressed
-- values. If you have a large data set (typically > 100 Mb), consider
-- setting this flag to True.
vsamAttributes_compressed :: Lens.Lens' VsamAttributes (Prelude.Maybe Prelude.Bool)
vsamAttributes_compressed = Lens.lens (\VsamAttributes' {compressed} -> compressed) (\s@VsamAttributes' {} a -> s {compressed = a} :: VsamAttributes)

-- | The alternate key definitions, if any. A legacy dataset might not have
-- any alternate key defined, but if those alternate keys definitions
-- exist, provide them as some applications will make use of them.
vsamAttributes_alternateKeys :: Lens.Lens' VsamAttributes (Prelude.Maybe [AlternateKey])
vsamAttributes_alternateKeys = Lens.lens (\VsamAttributes' {alternateKeys} -> alternateKeys) (\s@VsamAttributes' {} a -> s {alternateKeys = a} :: VsamAttributes) Prelude.. Lens.mapping Lens.coerced

-- | The record format of the data set.
vsamAttributes_format :: Lens.Lens' VsamAttributes Prelude.Text
vsamAttributes_format = Lens.lens (\VsamAttributes' {format} -> format) (\s@VsamAttributes' {} a -> s {format = a} :: VsamAttributes)

instance Prelude.Hashable VsamAttributes where
  hashWithSalt _salt VsamAttributes' {..} =
    _salt `Prelude.hashWithSalt` primaryKey
      `Prelude.hashWithSalt` encoding
      `Prelude.hashWithSalt` compressed
      `Prelude.hashWithSalt` alternateKeys
      `Prelude.hashWithSalt` format

instance Prelude.NFData VsamAttributes where
  rnf VsamAttributes' {..} =
    Prelude.rnf primaryKey
      `Prelude.seq` Prelude.rnf encoding
      `Prelude.seq` Prelude.rnf compressed
      `Prelude.seq` Prelude.rnf alternateKeys
      `Prelude.seq` Prelude.rnf format

instance Core.ToJSON VsamAttributes where
  toJSON VsamAttributes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("primaryKey" Core..=) Prelude.<$> primaryKey,
            ("encoding" Core..=) Prelude.<$> encoding,
            ("compressed" Core..=) Prelude.<$> compressed,
            ("alternateKeys" Core..=) Prelude.<$> alternateKeys,
            Prelude.Just ("format" Core..= format)
          ]
      )
