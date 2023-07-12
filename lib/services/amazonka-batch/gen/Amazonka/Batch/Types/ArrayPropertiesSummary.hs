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
-- Module      : Amazonka.Batch.Types.ArrayPropertiesSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.ArrayPropertiesSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the array properties of a job.
--
-- /See:/ 'newArrayPropertiesSummary' smart constructor.
data ArrayPropertiesSummary = ArrayPropertiesSummary'
  { -- | The job index within the array that\'s associated with this job. This
    -- parameter is returned for children of array jobs.
    index :: Prelude.Maybe Prelude.Int,
    -- | The size of the array job. This parameter is returned for parent array
    -- jobs.
    size :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ArrayPropertiesSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'index', 'arrayPropertiesSummary_index' - The job index within the array that\'s associated with this job. This
-- parameter is returned for children of array jobs.
--
-- 'size', 'arrayPropertiesSummary_size' - The size of the array job. This parameter is returned for parent array
-- jobs.
newArrayPropertiesSummary ::
  ArrayPropertiesSummary
newArrayPropertiesSummary =
  ArrayPropertiesSummary'
    { index = Prelude.Nothing,
      size = Prelude.Nothing
    }

-- | The job index within the array that\'s associated with this job. This
-- parameter is returned for children of array jobs.
arrayPropertiesSummary_index :: Lens.Lens' ArrayPropertiesSummary (Prelude.Maybe Prelude.Int)
arrayPropertiesSummary_index = Lens.lens (\ArrayPropertiesSummary' {index} -> index) (\s@ArrayPropertiesSummary' {} a -> s {index = a} :: ArrayPropertiesSummary)

-- | The size of the array job. This parameter is returned for parent array
-- jobs.
arrayPropertiesSummary_size :: Lens.Lens' ArrayPropertiesSummary (Prelude.Maybe Prelude.Int)
arrayPropertiesSummary_size = Lens.lens (\ArrayPropertiesSummary' {size} -> size) (\s@ArrayPropertiesSummary' {} a -> s {size = a} :: ArrayPropertiesSummary)

instance Data.FromJSON ArrayPropertiesSummary where
  parseJSON =
    Data.withObject
      "ArrayPropertiesSummary"
      ( \x ->
          ArrayPropertiesSummary'
            Prelude.<$> (x Data..:? "index")
            Prelude.<*> (x Data..:? "size")
      )

instance Prelude.Hashable ArrayPropertiesSummary where
  hashWithSalt _salt ArrayPropertiesSummary' {..} =
    _salt
      `Prelude.hashWithSalt` index
      `Prelude.hashWithSalt` size

instance Prelude.NFData ArrayPropertiesSummary where
  rnf ArrayPropertiesSummary' {..} =
    Prelude.rnf index `Prelude.seq` Prelude.rnf size
