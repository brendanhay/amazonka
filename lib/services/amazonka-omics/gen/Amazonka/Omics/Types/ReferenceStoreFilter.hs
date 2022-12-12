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
-- Module      : Amazonka.Omics.Types.ReferenceStoreFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.ReferenceStoreFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A filter for reference stores.
--
-- /See:/ 'newReferenceStoreFilter' smart constructor.
data ReferenceStoreFilter = ReferenceStoreFilter'
  { -- | The filter\'s start date.
    createdAfter :: Prelude.Maybe Data.POSIX,
    -- | The filter\'s end date.
    createdBefore :: Prelude.Maybe Data.POSIX,
    -- | The name to filter on.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReferenceStoreFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAfter', 'referenceStoreFilter_createdAfter' - The filter\'s start date.
--
-- 'createdBefore', 'referenceStoreFilter_createdBefore' - The filter\'s end date.
--
-- 'name', 'referenceStoreFilter_name' - The name to filter on.
newReferenceStoreFilter ::
  ReferenceStoreFilter
newReferenceStoreFilter =
  ReferenceStoreFilter'
    { createdAfter =
        Prelude.Nothing,
      createdBefore = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The filter\'s start date.
referenceStoreFilter_createdAfter :: Lens.Lens' ReferenceStoreFilter (Prelude.Maybe Prelude.UTCTime)
referenceStoreFilter_createdAfter = Lens.lens (\ReferenceStoreFilter' {createdAfter} -> createdAfter) (\s@ReferenceStoreFilter' {} a -> s {createdAfter = a} :: ReferenceStoreFilter) Prelude.. Lens.mapping Data._Time

-- | The filter\'s end date.
referenceStoreFilter_createdBefore :: Lens.Lens' ReferenceStoreFilter (Prelude.Maybe Prelude.UTCTime)
referenceStoreFilter_createdBefore = Lens.lens (\ReferenceStoreFilter' {createdBefore} -> createdBefore) (\s@ReferenceStoreFilter' {} a -> s {createdBefore = a} :: ReferenceStoreFilter) Prelude.. Lens.mapping Data._Time

-- | The name to filter on.
referenceStoreFilter_name :: Lens.Lens' ReferenceStoreFilter (Prelude.Maybe Prelude.Text)
referenceStoreFilter_name = Lens.lens (\ReferenceStoreFilter' {name} -> name) (\s@ReferenceStoreFilter' {} a -> s {name = a} :: ReferenceStoreFilter)

instance Prelude.Hashable ReferenceStoreFilter where
  hashWithSalt _salt ReferenceStoreFilter' {..} =
    _salt `Prelude.hashWithSalt` createdAfter
      `Prelude.hashWithSalt` createdBefore
      `Prelude.hashWithSalt` name

instance Prelude.NFData ReferenceStoreFilter where
  rnf ReferenceStoreFilter' {..} =
    Prelude.rnf createdAfter
      `Prelude.seq` Prelude.rnf createdBefore
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON ReferenceStoreFilter where
  toJSON ReferenceStoreFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("createdAfter" Data..=) Prelude.<$> createdAfter,
            ("createdBefore" Data..=) Prelude.<$> createdBefore,
            ("name" Data..=) Prelude.<$> name
          ]
      )
