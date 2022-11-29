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
-- Module      : Amazonka.CloudWatchEvents.Types.BatchArrayProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.BatchArrayProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The array properties for the submitted job, such as the size of the
-- array. The array size can be between 2 and 10,000. If you specify array
-- properties for a job, it becomes an array job. This parameter is used
-- only if the target is an Batch job.
--
-- /See:/ 'newBatchArrayProperties' smart constructor.
data BatchArrayProperties = BatchArrayProperties'
  { -- | The size of the array, if this is an array batch job. Valid values are
    -- integers between 2 and 10,000.
    size :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchArrayProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'size', 'batchArrayProperties_size' - The size of the array, if this is an array batch job. Valid values are
-- integers between 2 and 10,000.
newBatchArrayProperties ::
  BatchArrayProperties
newBatchArrayProperties =
  BatchArrayProperties' {size = Prelude.Nothing}

-- | The size of the array, if this is an array batch job. Valid values are
-- integers between 2 and 10,000.
batchArrayProperties_size :: Lens.Lens' BatchArrayProperties (Prelude.Maybe Prelude.Int)
batchArrayProperties_size = Lens.lens (\BatchArrayProperties' {size} -> size) (\s@BatchArrayProperties' {} a -> s {size = a} :: BatchArrayProperties)

instance Core.FromJSON BatchArrayProperties where
  parseJSON =
    Core.withObject
      "BatchArrayProperties"
      ( \x ->
          BatchArrayProperties'
            Prelude.<$> (x Core..:? "Size")
      )

instance Prelude.Hashable BatchArrayProperties where
  hashWithSalt _salt BatchArrayProperties' {..} =
    _salt `Prelude.hashWithSalt` size

instance Prelude.NFData BatchArrayProperties where
  rnf BatchArrayProperties' {..} = Prelude.rnf size

instance Core.ToJSON BatchArrayProperties where
  toJSON BatchArrayProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [("Size" Core..=) Prelude.<$> size]
      )
