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
-- Module      : Amazonka.Omics.Types.ReferenceFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.ReferenceFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A filter for references.
--
-- /See:/ 'newReferenceFilter' smart constructor.
data ReferenceFilter = ReferenceFilter'
  { -- | The filter\'s start date.
    createdAfter :: Prelude.Maybe Data.ISO8601,
    -- | The filter\'s end date.
    createdBefore :: Prelude.Maybe Data.ISO8601,
    -- | An MD5 checksum to filter on.
    md5 :: Prelude.Maybe Prelude.Text,
    -- | A name to filter on.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReferenceFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAfter', 'referenceFilter_createdAfter' - The filter\'s start date.
--
-- 'createdBefore', 'referenceFilter_createdBefore' - The filter\'s end date.
--
-- 'md5', 'referenceFilter_md5' - An MD5 checksum to filter on.
--
-- 'name', 'referenceFilter_name' - A name to filter on.
newReferenceFilter ::
  ReferenceFilter
newReferenceFilter =
  ReferenceFilter'
    { createdAfter = Prelude.Nothing,
      createdBefore = Prelude.Nothing,
      md5 = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The filter\'s start date.
referenceFilter_createdAfter :: Lens.Lens' ReferenceFilter (Prelude.Maybe Prelude.UTCTime)
referenceFilter_createdAfter = Lens.lens (\ReferenceFilter' {createdAfter} -> createdAfter) (\s@ReferenceFilter' {} a -> s {createdAfter = a} :: ReferenceFilter) Prelude.. Lens.mapping Data._Time

-- | The filter\'s end date.
referenceFilter_createdBefore :: Lens.Lens' ReferenceFilter (Prelude.Maybe Prelude.UTCTime)
referenceFilter_createdBefore = Lens.lens (\ReferenceFilter' {createdBefore} -> createdBefore) (\s@ReferenceFilter' {} a -> s {createdBefore = a} :: ReferenceFilter) Prelude.. Lens.mapping Data._Time

-- | An MD5 checksum to filter on.
referenceFilter_md5 :: Lens.Lens' ReferenceFilter (Prelude.Maybe Prelude.Text)
referenceFilter_md5 = Lens.lens (\ReferenceFilter' {md5} -> md5) (\s@ReferenceFilter' {} a -> s {md5 = a} :: ReferenceFilter)

-- | A name to filter on.
referenceFilter_name :: Lens.Lens' ReferenceFilter (Prelude.Maybe Prelude.Text)
referenceFilter_name = Lens.lens (\ReferenceFilter' {name} -> name) (\s@ReferenceFilter' {} a -> s {name = a} :: ReferenceFilter)

instance Prelude.Hashable ReferenceFilter where
  hashWithSalt _salt ReferenceFilter' {..} =
    _salt
      `Prelude.hashWithSalt` createdAfter
      `Prelude.hashWithSalt` createdBefore
      `Prelude.hashWithSalt` md5
      `Prelude.hashWithSalt` name

instance Prelude.NFData ReferenceFilter where
  rnf ReferenceFilter' {..} =
    Prelude.rnf createdAfter `Prelude.seq`
      Prelude.rnf createdBefore `Prelude.seq`
        Prelude.rnf md5 `Prelude.seq`
          Prelude.rnf name

instance Data.ToJSON ReferenceFilter where
  toJSON ReferenceFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("createdAfter" Data..=) Prelude.<$> createdAfter,
            ("createdBefore" Data..=) Prelude.<$> createdBefore,
            ("md5" Data..=) Prelude.<$> md5,
            ("name" Data..=) Prelude.<$> name
          ]
      )
