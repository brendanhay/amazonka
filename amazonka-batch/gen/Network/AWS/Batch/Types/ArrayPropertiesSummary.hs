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
-- Module      : Network.AWS.Batch.Types.ArrayPropertiesSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.ArrayPropertiesSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An object representing the array properties of a job.
--
-- /See:/ 'newArrayPropertiesSummary' smart constructor.
data ArrayPropertiesSummary = ArrayPropertiesSummary'
  { -- | The job index within the array that\'s associated with this job. This
    -- parameter is returned for children of array jobs.
    index :: Core.Maybe Core.Int,
    -- | The size of the array job. This parameter is returned for parent array
    -- jobs.
    size :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { index = Core.Nothing,
      size = Core.Nothing
    }

-- | The job index within the array that\'s associated with this job. This
-- parameter is returned for children of array jobs.
arrayPropertiesSummary_index :: Lens.Lens' ArrayPropertiesSummary (Core.Maybe Core.Int)
arrayPropertiesSummary_index = Lens.lens (\ArrayPropertiesSummary' {index} -> index) (\s@ArrayPropertiesSummary' {} a -> s {index = a} :: ArrayPropertiesSummary)

-- | The size of the array job. This parameter is returned for parent array
-- jobs.
arrayPropertiesSummary_size :: Lens.Lens' ArrayPropertiesSummary (Core.Maybe Core.Int)
arrayPropertiesSummary_size = Lens.lens (\ArrayPropertiesSummary' {size} -> size) (\s@ArrayPropertiesSummary' {} a -> s {size = a} :: ArrayPropertiesSummary)

instance Core.FromJSON ArrayPropertiesSummary where
  parseJSON =
    Core.withObject
      "ArrayPropertiesSummary"
      ( \x ->
          ArrayPropertiesSummary'
            Core.<$> (x Core..:? "index") Core.<*> (x Core..:? "size")
      )

instance Core.Hashable ArrayPropertiesSummary

instance Core.NFData ArrayPropertiesSummary
