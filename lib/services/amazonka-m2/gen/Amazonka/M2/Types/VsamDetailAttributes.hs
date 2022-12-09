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
-- Module      : Amazonka.M2.Types.VsamDetailAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.VsamDetailAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.M2.Types.AlternateKey
import Amazonka.M2.Types.PrimaryKey
import qualified Amazonka.Prelude as Prelude

-- | The attributes of a VSAM type data set.
--
-- /See:/ 'newVsamDetailAttributes' smart constructor.
data VsamDetailAttributes = VsamDetailAttributes'
  { -- | The alternate key definitions, if any. A legacy dataset might not have
    -- any alternate key defined, but if those alternate keys definitions
    -- exist, provide them as some applications will make use of them.
    alternateKeys :: Prelude.Maybe [AlternateKey],
    -- | If set to True, enforces loading the data set into cache before it’s
    -- used by the application.
    cacheAtStartup :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether indexes for this dataset are stored as compressed
    -- values. If you have a large data set (typically > 100 Mb), consider
    -- setting this flag to True.
    compressed :: Prelude.Maybe Prelude.Bool,
    -- | The character set used by the data set. Can be ASCII, EBCDIC, or
    -- unknown.
    encoding :: Prelude.Maybe Prelude.Text,
    -- | The primary key of the data set.
    primaryKey :: Prelude.Maybe PrimaryKey,
    -- | The record format of the data set.
    recordFormat :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VsamDetailAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alternateKeys', 'vsamDetailAttributes_alternateKeys' - The alternate key definitions, if any. A legacy dataset might not have
-- any alternate key defined, but if those alternate keys definitions
-- exist, provide them as some applications will make use of them.
--
-- 'cacheAtStartup', 'vsamDetailAttributes_cacheAtStartup' - If set to True, enforces loading the data set into cache before it’s
-- used by the application.
--
-- 'compressed', 'vsamDetailAttributes_compressed' - Indicates whether indexes for this dataset are stored as compressed
-- values. If you have a large data set (typically > 100 Mb), consider
-- setting this flag to True.
--
-- 'encoding', 'vsamDetailAttributes_encoding' - The character set used by the data set. Can be ASCII, EBCDIC, or
-- unknown.
--
-- 'primaryKey', 'vsamDetailAttributes_primaryKey' - The primary key of the data set.
--
-- 'recordFormat', 'vsamDetailAttributes_recordFormat' - The record format of the data set.
newVsamDetailAttributes ::
  VsamDetailAttributes
newVsamDetailAttributes =
  VsamDetailAttributes'
    { alternateKeys =
        Prelude.Nothing,
      cacheAtStartup = Prelude.Nothing,
      compressed = Prelude.Nothing,
      encoding = Prelude.Nothing,
      primaryKey = Prelude.Nothing,
      recordFormat = Prelude.Nothing
    }

-- | The alternate key definitions, if any. A legacy dataset might not have
-- any alternate key defined, but if those alternate keys definitions
-- exist, provide them as some applications will make use of them.
vsamDetailAttributes_alternateKeys :: Lens.Lens' VsamDetailAttributes (Prelude.Maybe [AlternateKey])
vsamDetailAttributes_alternateKeys = Lens.lens (\VsamDetailAttributes' {alternateKeys} -> alternateKeys) (\s@VsamDetailAttributes' {} a -> s {alternateKeys = a} :: VsamDetailAttributes) Prelude.. Lens.mapping Lens.coerced

-- | If set to True, enforces loading the data set into cache before it’s
-- used by the application.
vsamDetailAttributes_cacheAtStartup :: Lens.Lens' VsamDetailAttributes (Prelude.Maybe Prelude.Bool)
vsamDetailAttributes_cacheAtStartup = Lens.lens (\VsamDetailAttributes' {cacheAtStartup} -> cacheAtStartup) (\s@VsamDetailAttributes' {} a -> s {cacheAtStartup = a} :: VsamDetailAttributes)

-- | Indicates whether indexes for this dataset are stored as compressed
-- values. If you have a large data set (typically > 100 Mb), consider
-- setting this flag to True.
vsamDetailAttributes_compressed :: Lens.Lens' VsamDetailAttributes (Prelude.Maybe Prelude.Bool)
vsamDetailAttributes_compressed = Lens.lens (\VsamDetailAttributes' {compressed} -> compressed) (\s@VsamDetailAttributes' {} a -> s {compressed = a} :: VsamDetailAttributes)

-- | The character set used by the data set. Can be ASCII, EBCDIC, or
-- unknown.
vsamDetailAttributes_encoding :: Lens.Lens' VsamDetailAttributes (Prelude.Maybe Prelude.Text)
vsamDetailAttributes_encoding = Lens.lens (\VsamDetailAttributes' {encoding} -> encoding) (\s@VsamDetailAttributes' {} a -> s {encoding = a} :: VsamDetailAttributes)

-- | The primary key of the data set.
vsamDetailAttributes_primaryKey :: Lens.Lens' VsamDetailAttributes (Prelude.Maybe PrimaryKey)
vsamDetailAttributes_primaryKey = Lens.lens (\VsamDetailAttributes' {primaryKey} -> primaryKey) (\s@VsamDetailAttributes' {} a -> s {primaryKey = a} :: VsamDetailAttributes)

-- | The record format of the data set.
vsamDetailAttributes_recordFormat :: Lens.Lens' VsamDetailAttributes (Prelude.Maybe Prelude.Text)
vsamDetailAttributes_recordFormat = Lens.lens (\VsamDetailAttributes' {recordFormat} -> recordFormat) (\s@VsamDetailAttributes' {} a -> s {recordFormat = a} :: VsamDetailAttributes)

instance Data.FromJSON VsamDetailAttributes where
  parseJSON =
    Data.withObject
      "VsamDetailAttributes"
      ( \x ->
          VsamDetailAttributes'
            Prelude.<$> (x Data..:? "alternateKeys" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "cacheAtStartup")
            Prelude.<*> (x Data..:? "compressed")
            Prelude.<*> (x Data..:? "encoding")
            Prelude.<*> (x Data..:? "primaryKey")
            Prelude.<*> (x Data..:? "recordFormat")
      )

instance Prelude.Hashable VsamDetailAttributes where
  hashWithSalt _salt VsamDetailAttributes' {..} =
    _salt `Prelude.hashWithSalt` alternateKeys
      `Prelude.hashWithSalt` cacheAtStartup
      `Prelude.hashWithSalt` compressed
      `Prelude.hashWithSalt` encoding
      `Prelude.hashWithSalt` primaryKey
      `Prelude.hashWithSalt` recordFormat

instance Prelude.NFData VsamDetailAttributes where
  rnf VsamDetailAttributes' {..} =
    Prelude.rnf alternateKeys
      `Prelude.seq` Prelude.rnf cacheAtStartup
      `Prelude.seq` Prelude.rnf compressed
      `Prelude.seq` Prelude.rnf encoding
      `Prelude.seq` Prelude.rnf primaryKey
      `Prelude.seq` Prelude.rnf recordFormat
