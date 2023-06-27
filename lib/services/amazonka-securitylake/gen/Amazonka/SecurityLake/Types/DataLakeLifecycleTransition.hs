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
-- Module      : Amazonka.SecurityLake.Types.DataLakeLifecycleTransition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.DataLakeLifecycleTransition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provide transition lifecycle details of Amazon Security Lake object.
--
-- /See:/ 'newDataLakeLifecycleTransition' smart constructor.
data DataLakeLifecycleTransition = DataLakeLifecycleTransition'
  { -- | Number of days before data transitions to a different S3 Storage Class
    -- in the Amazon Security Lake object.
    days :: Prelude.Maybe Prelude.Natural,
    -- | The range of storage classes that you can choose from based on the data
    -- access, resiliency, and cost requirements of your workloads.
    storageClass :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataLakeLifecycleTransition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'days', 'dataLakeLifecycleTransition_days' - Number of days before data transitions to a different S3 Storage Class
-- in the Amazon Security Lake object.
--
-- 'storageClass', 'dataLakeLifecycleTransition_storageClass' - The range of storage classes that you can choose from based on the data
-- access, resiliency, and cost requirements of your workloads.
newDataLakeLifecycleTransition ::
  DataLakeLifecycleTransition
newDataLakeLifecycleTransition =
  DataLakeLifecycleTransition'
    { days =
        Prelude.Nothing,
      storageClass = Prelude.Nothing
    }

-- | Number of days before data transitions to a different S3 Storage Class
-- in the Amazon Security Lake object.
dataLakeLifecycleTransition_days :: Lens.Lens' DataLakeLifecycleTransition (Prelude.Maybe Prelude.Natural)
dataLakeLifecycleTransition_days = Lens.lens (\DataLakeLifecycleTransition' {days} -> days) (\s@DataLakeLifecycleTransition' {} a -> s {days = a} :: DataLakeLifecycleTransition)

-- | The range of storage classes that you can choose from based on the data
-- access, resiliency, and cost requirements of your workloads.
dataLakeLifecycleTransition_storageClass :: Lens.Lens' DataLakeLifecycleTransition (Prelude.Maybe Prelude.Text)
dataLakeLifecycleTransition_storageClass = Lens.lens (\DataLakeLifecycleTransition' {storageClass} -> storageClass) (\s@DataLakeLifecycleTransition' {} a -> s {storageClass = a} :: DataLakeLifecycleTransition)

instance Data.FromJSON DataLakeLifecycleTransition where
  parseJSON =
    Data.withObject
      "DataLakeLifecycleTransition"
      ( \x ->
          DataLakeLifecycleTransition'
            Prelude.<$> (x Data..:? "days")
            Prelude.<*> (x Data..:? "storageClass")
      )

instance Prelude.Hashable DataLakeLifecycleTransition where
  hashWithSalt _salt DataLakeLifecycleTransition' {..} =
    _salt
      `Prelude.hashWithSalt` days
      `Prelude.hashWithSalt` storageClass

instance Prelude.NFData DataLakeLifecycleTransition where
  rnf DataLakeLifecycleTransition' {..} =
    Prelude.rnf days
      `Prelude.seq` Prelude.rnf storageClass

instance Data.ToJSON DataLakeLifecycleTransition where
  toJSON DataLakeLifecycleTransition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("days" Data..=) Prelude.<$> days,
            ("storageClass" Data..=) Prelude.<$> storageClass
          ]
      )
