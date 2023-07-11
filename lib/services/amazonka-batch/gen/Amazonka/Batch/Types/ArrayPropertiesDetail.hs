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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.ArrayPropertiesDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the array properties of a job.
--
-- /See:/ 'newArrayPropertiesDetail' smart constructor.
data ArrayPropertiesDetail = ArrayPropertiesDetail'
  { -- | The job index within the array that\'s associated with this job. This
    -- parameter is returned for array job children.
    index :: Prelude.Maybe Prelude.Int,
    -- | The size of the array job. This parameter is returned for parent array
    -- jobs.
    size :: Prelude.Maybe Prelude.Int,
    -- | A summary of the number of array job children in each available job
    -- status. This parameter is returned for parent array jobs.
    statusSummary :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Int)
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
-- 'index', 'arrayPropertiesDetail_index' - The job index within the array that\'s associated with this job. This
-- parameter is returned for array job children.
--
-- 'size', 'arrayPropertiesDetail_size' - The size of the array job. This parameter is returned for parent array
-- jobs.
--
-- 'statusSummary', 'arrayPropertiesDetail_statusSummary' - A summary of the number of array job children in each available job
-- status. This parameter is returned for parent array jobs.
newArrayPropertiesDetail ::
  ArrayPropertiesDetail
newArrayPropertiesDetail =
  ArrayPropertiesDetail'
    { index = Prelude.Nothing,
      size = Prelude.Nothing,
      statusSummary = Prelude.Nothing
    }

-- | The job index within the array that\'s associated with this job. This
-- parameter is returned for array job children.
arrayPropertiesDetail_index :: Lens.Lens' ArrayPropertiesDetail (Prelude.Maybe Prelude.Int)
arrayPropertiesDetail_index = Lens.lens (\ArrayPropertiesDetail' {index} -> index) (\s@ArrayPropertiesDetail' {} a -> s {index = a} :: ArrayPropertiesDetail)

-- | The size of the array job. This parameter is returned for parent array
-- jobs.
arrayPropertiesDetail_size :: Lens.Lens' ArrayPropertiesDetail (Prelude.Maybe Prelude.Int)
arrayPropertiesDetail_size = Lens.lens (\ArrayPropertiesDetail' {size} -> size) (\s@ArrayPropertiesDetail' {} a -> s {size = a} :: ArrayPropertiesDetail)

-- | A summary of the number of array job children in each available job
-- status. This parameter is returned for parent array jobs.
arrayPropertiesDetail_statusSummary :: Lens.Lens' ArrayPropertiesDetail (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Int))
arrayPropertiesDetail_statusSummary = Lens.lens (\ArrayPropertiesDetail' {statusSummary} -> statusSummary) (\s@ArrayPropertiesDetail' {} a -> s {statusSummary = a} :: ArrayPropertiesDetail) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ArrayPropertiesDetail where
  parseJSON =
    Data.withObject
      "ArrayPropertiesDetail"
      ( \x ->
          ArrayPropertiesDetail'
            Prelude.<$> (x Data..:? "index")
            Prelude.<*> (x Data..:? "size")
            Prelude.<*> (x Data..:? "statusSummary" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ArrayPropertiesDetail where
  hashWithSalt _salt ArrayPropertiesDetail' {..} =
    _salt
      `Prelude.hashWithSalt` index
      `Prelude.hashWithSalt` size
      `Prelude.hashWithSalt` statusSummary

instance Prelude.NFData ArrayPropertiesDetail where
  rnf ArrayPropertiesDetail' {..} =
    Prelude.rnf index
      `Prelude.seq` Prelude.rnf size
      `Prelude.seq` Prelude.rnf statusSummary
