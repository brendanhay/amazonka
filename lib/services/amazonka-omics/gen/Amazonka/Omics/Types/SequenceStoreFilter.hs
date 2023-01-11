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
-- Module      : Amazonka.Omics.Types.SequenceStoreFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.SequenceStoreFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A filter for a sequence store.
--
-- /See:/ 'newSequenceStoreFilter' smart constructor.
data SequenceStoreFilter = SequenceStoreFilter'
  { -- | The filter\'s start date.
    createdAfter :: Prelude.Maybe Data.ISO8601,
    -- | The filter\'s end date.
    createdBefore :: Prelude.Maybe Data.ISO8601,
    -- | A name to filter on.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SequenceStoreFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAfter', 'sequenceStoreFilter_createdAfter' - The filter\'s start date.
--
-- 'createdBefore', 'sequenceStoreFilter_createdBefore' - The filter\'s end date.
--
-- 'name', 'sequenceStoreFilter_name' - A name to filter on.
newSequenceStoreFilter ::
  SequenceStoreFilter
newSequenceStoreFilter =
  SequenceStoreFilter'
    { createdAfter =
        Prelude.Nothing,
      createdBefore = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The filter\'s start date.
sequenceStoreFilter_createdAfter :: Lens.Lens' SequenceStoreFilter (Prelude.Maybe Prelude.UTCTime)
sequenceStoreFilter_createdAfter = Lens.lens (\SequenceStoreFilter' {createdAfter} -> createdAfter) (\s@SequenceStoreFilter' {} a -> s {createdAfter = a} :: SequenceStoreFilter) Prelude.. Lens.mapping Data._Time

-- | The filter\'s end date.
sequenceStoreFilter_createdBefore :: Lens.Lens' SequenceStoreFilter (Prelude.Maybe Prelude.UTCTime)
sequenceStoreFilter_createdBefore = Lens.lens (\SequenceStoreFilter' {createdBefore} -> createdBefore) (\s@SequenceStoreFilter' {} a -> s {createdBefore = a} :: SequenceStoreFilter) Prelude.. Lens.mapping Data._Time

-- | A name to filter on.
sequenceStoreFilter_name :: Lens.Lens' SequenceStoreFilter (Prelude.Maybe Prelude.Text)
sequenceStoreFilter_name = Lens.lens (\SequenceStoreFilter' {name} -> name) (\s@SequenceStoreFilter' {} a -> s {name = a} :: SequenceStoreFilter)

instance Prelude.Hashable SequenceStoreFilter where
  hashWithSalt _salt SequenceStoreFilter' {..} =
    _salt `Prelude.hashWithSalt` createdAfter
      `Prelude.hashWithSalt` createdBefore
      `Prelude.hashWithSalt` name

instance Prelude.NFData SequenceStoreFilter where
  rnf SequenceStoreFilter' {..} =
    Prelude.rnf createdAfter
      `Prelude.seq` Prelude.rnf createdBefore
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON SequenceStoreFilter where
  toJSON SequenceStoreFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("createdAfter" Data..=) Prelude.<$> createdAfter,
            ("createdBefore" Data..=) Prelude.<$> createdBefore,
            ("name" Data..=) Prelude.<$> name
          ]
      )
