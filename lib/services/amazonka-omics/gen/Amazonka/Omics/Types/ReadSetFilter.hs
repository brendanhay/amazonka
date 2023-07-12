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
-- Module      : Amazonka.Omics.Types.ReadSetFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.ReadSetFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.ReadSetStatus
import qualified Amazonka.Prelude as Prelude

-- | A filter for read sets.
--
-- /See:/ 'newReadSetFilter' smart constructor.
data ReadSetFilter = ReadSetFilter'
  { -- | The filter\'s start date.
    createdAfter :: Prelude.Maybe Data.ISO8601,
    -- | The filter\'s end date.
    createdBefore :: Prelude.Maybe Data.ISO8601,
    -- | A name to filter on.
    name :: Prelude.Maybe Prelude.Text,
    -- | A genome reference ARN to filter on.
    referenceArn :: Prelude.Maybe Prelude.Text,
    -- | A status to filter on.
    status :: Prelude.Maybe ReadSetStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReadSetFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAfter', 'readSetFilter_createdAfter' - The filter\'s start date.
--
-- 'createdBefore', 'readSetFilter_createdBefore' - The filter\'s end date.
--
-- 'name', 'readSetFilter_name' - A name to filter on.
--
-- 'referenceArn', 'readSetFilter_referenceArn' - A genome reference ARN to filter on.
--
-- 'status', 'readSetFilter_status' - A status to filter on.
newReadSetFilter ::
  ReadSetFilter
newReadSetFilter =
  ReadSetFilter'
    { createdAfter = Prelude.Nothing,
      createdBefore = Prelude.Nothing,
      name = Prelude.Nothing,
      referenceArn = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The filter\'s start date.
readSetFilter_createdAfter :: Lens.Lens' ReadSetFilter (Prelude.Maybe Prelude.UTCTime)
readSetFilter_createdAfter = Lens.lens (\ReadSetFilter' {createdAfter} -> createdAfter) (\s@ReadSetFilter' {} a -> s {createdAfter = a} :: ReadSetFilter) Prelude.. Lens.mapping Data._Time

-- | The filter\'s end date.
readSetFilter_createdBefore :: Lens.Lens' ReadSetFilter (Prelude.Maybe Prelude.UTCTime)
readSetFilter_createdBefore = Lens.lens (\ReadSetFilter' {createdBefore} -> createdBefore) (\s@ReadSetFilter' {} a -> s {createdBefore = a} :: ReadSetFilter) Prelude.. Lens.mapping Data._Time

-- | A name to filter on.
readSetFilter_name :: Lens.Lens' ReadSetFilter (Prelude.Maybe Prelude.Text)
readSetFilter_name = Lens.lens (\ReadSetFilter' {name} -> name) (\s@ReadSetFilter' {} a -> s {name = a} :: ReadSetFilter)

-- | A genome reference ARN to filter on.
readSetFilter_referenceArn :: Lens.Lens' ReadSetFilter (Prelude.Maybe Prelude.Text)
readSetFilter_referenceArn = Lens.lens (\ReadSetFilter' {referenceArn} -> referenceArn) (\s@ReadSetFilter' {} a -> s {referenceArn = a} :: ReadSetFilter)

-- | A status to filter on.
readSetFilter_status :: Lens.Lens' ReadSetFilter (Prelude.Maybe ReadSetStatus)
readSetFilter_status = Lens.lens (\ReadSetFilter' {status} -> status) (\s@ReadSetFilter' {} a -> s {status = a} :: ReadSetFilter)

instance Prelude.Hashable ReadSetFilter where
  hashWithSalt _salt ReadSetFilter' {..} =
    _salt
      `Prelude.hashWithSalt` createdAfter
      `Prelude.hashWithSalt` createdBefore
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` referenceArn
      `Prelude.hashWithSalt` status

instance Prelude.NFData ReadSetFilter where
  rnf ReadSetFilter' {..} =
    Prelude.rnf createdAfter
      `Prelude.seq` Prelude.rnf createdBefore
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf referenceArn
      `Prelude.seq` Prelude.rnf status

instance Data.ToJSON ReadSetFilter where
  toJSON ReadSetFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("createdAfter" Data..=) Prelude.<$> createdAfter,
            ("createdBefore" Data..=) Prelude.<$> createdBefore,
            ("name" Data..=) Prelude.<$> name,
            ("referenceArn" Data..=) Prelude.<$> referenceArn,
            ("status" Data..=) Prelude.<$> status
          ]
      )
