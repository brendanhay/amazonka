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
-- Module      : Amazonka.LakeFormation.Types.LFTagPair
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.LFTagPair where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure containing an LF-tag key-value pair.
--
-- /See:/ 'newLFTagPair' smart constructor.
data LFTagPair = LFTagPair'
  { -- | The identifier for the Data Catalog. By default, the account ID. The
    -- Data Catalog is the persistent metadata store. It contains database
    -- definitions, table definitions, and other control information to manage
    -- your Lake Formation environment.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The key-name for the LF-tag.
    tagKey :: Prelude.Text,
    -- | A list of possible values an attribute can take.
    tagValues :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LFTagPair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'lFTagPair_catalogId' - The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your Lake Formation environment.
--
-- 'tagKey', 'lFTagPair_tagKey' - The key-name for the LF-tag.
--
-- 'tagValues', 'lFTagPair_tagValues' - A list of possible values an attribute can take.
newLFTagPair ::
  -- | 'tagKey'
  Prelude.Text ->
  -- | 'tagValues'
  Prelude.NonEmpty Prelude.Text ->
  LFTagPair
newLFTagPair pTagKey_ pTagValues_ =
  LFTagPair'
    { catalogId = Prelude.Nothing,
      tagKey = pTagKey_,
      tagValues = Lens.coerced Lens.# pTagValues_
    }

-- | The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your Lake Formation environment.
lFTagPair_catalogId :: Lens.Lens' LFTagPair (Prelude.Maybe Prelude.Text)
lFTagPair_catalogId = Lens.lens (\LFTagPair' {catalogId} -> catalogId) (\s@LFTagPair' {} a -> s {catalogId = a} :: LFTagPair)

-- | The key-name for the LF-tag.
lFTagPair_tagKey :: Lens.Lens' LFTagPair Prelude.Text
lFTagPair_tagKey = Lens.lens (\LFTagPair' {tagKey} -> tagKey) (\s@LFTagPair' {} a -> s {tagKey = a} :: LFTagPair)

-- | A list of possible values an attribute can take.
lFTagPair_tagValues :: Lens.Lens' LFTagPair (Prelude.NonEmpty Prelude.Text)
lFTagPair_tagValues = Lens.lens (\LFTagPair' {tagValues} -> tagValues) (\s@LFTagPair' {} a -> s {tagValues = a} :: LFTagPair) Prelude.. Lens.coerced

instance Data.FromJSON LFTagPair where
  parseJSON =
    Data.withObject
      "LFTagPair"
      ( \x ->
          LFTagPair'
            Prelude.<$> (x Data..:? "CatalogId")
            Prelude.<*> (x Data..: "TagKey")
            Prelude.<*> (x Data..: "TagValues")
      )

instance Prelude.Hashable LFTagPair where
  hashWithSalt _salt LFTagPair' {..} =
    _salt
      `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` tagKey
      `Prelude.hashWithSalt` tagValues

instance Prelude.NFData LFTagPair where
  rnf LFTagPair' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf tagKey
      `Prelude.seq` Prelude.rnf tagValues

instance Data.ToJSON LFTagPair where
  toJSON LFTagPair' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            Prelude.Just ("TagKey" Data..= tagKey),
            Prelude.Just ("TagValues" Data..= tagValues)
          ]
      )
