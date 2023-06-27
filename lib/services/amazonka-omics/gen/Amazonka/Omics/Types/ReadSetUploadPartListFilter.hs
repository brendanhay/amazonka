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
-- Module      : Amazonka.Omics.Types.ReadSetUploadPartListFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.ReadSetUploadPartListFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Filter settings that select for read set upload parts of interest.
--
-- /See:/ 'newReadSetUploadPartListFilter' smart constructor.
data ReadSetUploadPartListFilter = ReadSetUploadPartListFilter'
  { -- | Filters for read set uploads after a specified time.
    createdAfter :: Prelude.Maybe Data.ISO8601,
    -- | Filters for read set part uploads before a specified time.
    createdBefore :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReadSetUploadPartListFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAfter', 'readSetUploadPartListFilter_createdAfter' - Filters for read set uploads after a specified time.
--
-- 'createdBefore', 'readSetUploadPartListFilter_createdBefore' - Filters for read set part uploads before a specified time.
newReadSetUploadPartListFilter ::
  ReadSetUploadPartListFilter
newReadSetUploadPartListFilter =
  ReadSetUploadPartListFilter'
    { createdAfter =
        Prelude.Nothing,
      createdBefore = Prelude.Nothing
    }

-- | Filters for read set uploads after a specified time.
readSetUploadPartListFilter_createdAfter :: Lens.Lens' ReadSetUploadPartListFilter (Prelude.Maybe Prelude.UTCTime)
readSetUploadPartListFilter_createdAfter = Lens.lens (\ReadSetUploadPartListFilter' {createdAfter} -> createdAfter) (\s@ReadSetUploadPartListFilter' {} a -> s {createdAfter = a} :: ReadSetUploadPartListFilter) Prelude.. Lens.mapping Data._Time

-- | Filters for read set part uploads before a specified time.
readSetUploadPartListFilter_createdBefore :: Lens.Lens' ReadSetUploadPartListFilter (Prelude.Maybe Prelude.UTCTime)
readSetUploadPartListFilter_createdBefore = Lens.lens (\ReadSetUploadPartListFilter' {createdBefore} -> createdBefore) (\s@ReadSetUploadPartListFilter' {} a -> s {createdBefore = a} :: ReadSetUploadPartListFilter) Prelude.. Lens.mapping Data._Time

instance Prelude.Hashable ReadSetUploadPartListFilter where
  hashWithSalt _salt ReadSetUploadPartListFilter' {..} =
    _salt
      `Prelude.hashWithSalt` createdAfter
      `Prelude.hashWithSalt` createdBefore

instance Prelude.NFData ReadSetUploadPartListFilter where
  rnf ReadSetUploadPartListFilter' {..} =
    Prelude.rnf createdAfter
      `Prelude.seq` Prelude.rnf createdBefore

instance Data.ToJSON ReadSetUploadPartListFilter where
  toJSON ReadSetUploadPartListFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("createdAfter" Data..=) Prelude.<$> createdAfter,
            ("createdBefore" Data..=) Prelude.<$> createdBefore
          ]
      )
