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
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    -- | Where the source originated.
    generatedFrom :: Prelude.Maybe Prelude.Text,
    -- | A name to filter on.
    name :: Prelude.Maybe Prelude.Text,
    -- | A genome reference ARN to filter on.
    referenceArn :: Prelude.Maybe Prelude.Text,
    -- | The read set source\'s sample ID.
    sampleId :: Prelude.Maybe Prelude.Text,
    -- | A status to filter on.
    status :: Prelude.Maybe ReadSetStatus,
    -- | The read set source\'s subject ID.
    subjectId :: Prelude.Maybe Prelude.Text
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
-- 'generatedFrom', 'readSetFilter_generatedFrom' - Where the source originated.
--
-- 'name', 'readSetFilter_name' - A name to filter on.
--
-- 'referenceArn', 'readSetFilter_referenceArn' - A genome reference ARN to filter on.
--
-- 'sampleId', 'readSetFilter_sampleId' - The read set source\'s sample ID.
--
-- 'status', 'readSetFilter_status' - A status to filter on.
--
-- 'subjectId', 'readSetFilter_subjectId' - The read set source\'s subject ID.
newReadSetFilter ::
  ReadSetFilter
newReadSetFilter =
  ReadSetFilter'
    { createdAfter = Prelude.Nothing,
      createdBefore = Prelude.Nothing,
      generatedFrom = Prelude.Nothing,
      name = Prelude.Nothing,
      referenceArn = Prelude.Nothing,
      sampleId = Prelude.Nothing,
      status = Prelude.Nothing,
      subjectId = Prelude.Nothing
    }

-- | The filter\'s start date.
readSetFilter_createdAfter :: Lens.Lens' ReadSetFilter (Prelude.Maybe Prelude.UTCTime)
readSetFilter_createdAfter = Lens.lens (\ReadSetFilter' {createdAfter} -> createdAfter) (\s@ReadSetFilter' {} a -> s {createdAfter = a} :: ReadSetFilter) Prelude.. Lens.mapping Data._Time

-- | The filter\'s end date.
readSetFilter_createdBefore :: Lens.Lens' ReadSetFilter (Prelude.Maybe Prelude.UTCTime)
readSetFilter_createdBefore = Lens.lens (\ReadSetFilter' {createdBefore} -> createdBefore) (\s@ReadSetFilter' {} a -> s {createdBefore = a} :: ReadSetFilter) Prelude.. Lens.mapping Data._Time

-- | Where the source originated.
readSetFilter_generatedFrom :: Lens.Lens' ReadSetFilter (Prelude.Maybe Prelude.Text)
readSetFilter_generatedFrom = Lens.lens (\ReadSetFilter' {generatedFrom} -> generatedFrom) (\s@ReadSetFilter' {} a -> s {generatedFrom = a} :: ReadSetFilter)

-- | A name to filter on.
readSetFilter_name :: Lens.Lens' ReadSetFilter (Prelude.Maybe Prelude.Text)
readSetFilter_name = Lens.lens (\ReadSetFilter' {name} -> name) (\s@ReadSetFilter' {} a -> s {name = a} :: ReadSetFilter)

-- | A genome reference ARN to filter on.
readSetFilter_referenceArn :: Lens.Lens' ReadSetFilter (Prelude.Maybe Prelude.Text)
readSetFilter_referenceArn = Lens.lens (\ReadSetFilter' {referenceArn} -> referenceArn) (\s@ReadSetFilter' {} a -> s {referenceArn = a} :: ReadSetFilter)

-- | The read set source\'s sample ID.
readSetFilter_sampleId :: Lens.Lens' ReadSetFilter (Prelude.Maybe Prelude.Text)
readSetFilter_sampleId = Lens.lens (\ReadSetFilter' {sampleId} -> sampleId) (\s@ReadSetFilter' {} a -> s {sampleId = a} :: ReadSetFilter)

-- | A status to filter on.
readSetFilter_status :: Lens.Lens' ReadSetFilter (Prelude.Maybe ReadSetStatus)
readSetFilter_status = Lens.lens (\ReadSetFilter' {status} -> status) (\s@ReadSetFilter' {} a -> s {status = a} :: ReadSetFilter)

-- | The read set source\'s subject ID.
readSetFilter_subjectId :: Lens.Lens' ReadSetFilter (Prelude.Maybe Prelude.Text)
readSetFilter_subjectId = Lens.lens (\ReadSetFilter' {subjectId} -> subjectId) (\s@ReadSetFilter' {} a -> s {subjectId = a} :: ReadSetFilter)

instance Prelude.Hashable ReadSetFilter where
  hashWithSalt _salt ReadSetFilter' {..} =
    _salt
      `Prelude.hashWithSalt` createdAfter
      `Prelude.hashWithSalt` createdBefore
      `Prelude.hashWithSalt` generatedFrom
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` referenceArn
      `Prelude.hashWithSalt` sampleId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` subjectId

instance Prelude.NFData ReadSetFilter where
  rnf ReadSetFilter' {..} =
    Prelude.rnf createdAfter
      `Prelude.seq` Prelude.rnf createdBefore
      `Prelude.seq` Prelude.rnf generatedFrom
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf referenceArn
      `Prelude.seq` Prelude.rnf sampleId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf subjectId

instance Data.ToJSON ReadSetFilter where
  toJSON ReadSetFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("createdAfter" Data..=) Prelude.<$> createdAfter,
            ("createdBefore" Data..=) Prelude.<$> createdBefore,
            ("generatedFrom" Data..=) Prelude.<$> generatedFrom,
            ("name" Data..=) Prelude.<$> name,
            ("referenceArn" Data..=) Prelude.<$> referenceArn,
            ("sampleId" Data..=) Prelude.<$> sampleId,
            ("status" Data..=) Prelude.<$> status,
            ("subjectId" Data..=) Prelude.<$> subjectId
          ]
      )
