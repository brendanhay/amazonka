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
-- Module      : Amazonka.Batch.Types.ArrayPropertiesDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.ArrayPropertiesDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object representing the array properties of a job.
--
-- /See:/ 'newArrayPropertiesDetail' smart constructor.
data ArrayPropertiesDetail = ArrayPropertiesDetail'
  { -- | The size of the array job. This parameter is returned for parent array
    -- jobs.
    size :: Prelude.Maybe Prelude.Int,
    -- | A summary of the number of array job children in each available job
    -- status. This parameter is returned for parent array jobs.
    statusSummary :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Int),
    -- | The job index within the array that\'s associated with this job. This
    -- parameter is returned for array job children.
    index :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ArrayPropertiesDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'size', 'arrayPropertiesDetail_size' - The size of the array job. This parameter is returned for parent array
-- jobs.
--
-- 'statusSummary', 'arrayPropertiesDetail_statusSummary' - A summary of the number of array job children in each available job
-- status. This parameter is returned for parent array jobs.
--
-- 'index', 'arrayPropertiesDetail_index' - The job index within the array that\'s associated with this job. This
-- parameter is returned for array job children.
newArrayPropertiesDetail ::
  ArrayPropertiesDetail
newArrayPropertiesDetail =
  ArrayPropertiesDetail'
    { size = Prelude.Nothing,
      statusSummary = Prelude.Nothing,
      index = Prelude.Nothing
    }

-- | The size of the array job. This parameter is returned for parent array
-- jobs.
arrayPropertiesDetail_size :: Lens.Lens' ArrayPropertiesDetail (Prelude.Maybe Prelude.Int)
arrayPropertiesDetail_size = Lens.lens (\ArrayPropertiesDetail' {size} -> size) (\s@ArrayPropertiesDetail' {} a -> s {size = a} :: ArrayPropertiesDetail)

-- | A summary of the number of array job children in each available job
-- status. This parameter is returned for parent array jobs.
arrayPropertiesDetail_statusSummary :: Lens.Lens' ArrayPropertiesDetail (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Int))
arrayPropertiesDetail_statusSummary = Lens.lens (\ArrayPropertiesDetail' {statusSummary} -> statusSummary) (\s@ArrayPropertiesDetail' {} a -> s {statusSummary = a} :: ArrayPropertiesDetail) Prelude.. Lens.mapping Lens.coerced

-- | The job index within the array that\'s associated with this job. This
-- parameter is returned for array job children.
arrayPropertiesDetail_index :: Lens.Lens' ArrayPropertiesDetail (Prelude.Maybe Prelude.Int)
arrayPropertiesDetail_index = Lens.lens (\ArrayPropertiesDetail' {index} -> index) (\s@ArrayPropertiesDetail' {} a -> s {index = a} :: ArrayPropertiesDetail)

instance Core.FromJSON ArrayPropertiesDetail where
  parseJSON =
    Core.withObject
      "ArrayPropertiesDetail"
      ( \x ->
          ArrayPropertiesDetail'
            Prelude.<$> (x Core..:? "size")
            Prelude.<*> (x Core..:? "statusSummary" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "index")
      )

instance Prelude.Hashable ArrayPropertiesDetail where
  hashWithSalt salt' ArrayPropertiesDetail' {..} =
    salt' `Prelude.hashWithSalt` index
      `Prelude.hashWithSalt` statusSummary
      `Prelude.hashWithSalt` size

instance Prelude.NFData ArrayPropertiesDetail where
  rnf ArrayPropertiesDetail' {..} =
    Prelude.rnf size `Prelude.seq` Prelude.rnf index
      `Prelude.seq` Prelude.rnf statusSummary
