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
-- Module      : Amazonka.SecurityLake.Types.RetentionSetting
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.RetentionSetting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityLake.Types.StorageClass

-- | Retention settings for the destination Amazon S3 buckets in Amazon
-- Security Lake.
--
-- /See:/ 'newRetentionSetting' smart constructor.
data RetentionSetting = RetentionSetting'
  { -- | The retention period specifies a fixed period of time during which the
    -- Security Lake object remains locked. You can specify the retention
    -- period in days for one or more sources.
    retentionPeriod :: Prelude.Maybe Prelude.Natural,
    -- | The range of storage classes that you can choose from based on the data
    -- access, resiliency, and cost requirements of your workloads.
    storageClass :: Prelude.Maybe StorageClass
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RetentionSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'retentionPeriod', 'retentionSetting_retentionPeriod' - The retention period specifies a fixed period of time during which the
-- Security Lake object remains locked. You can specify the retention
-- period in days for one or more sources.
--
-- 'storageClass', 'retentionSetting_storageClass' - The range of storage classes that you can choose from based on the data
-- access, resiliency, and cost requirements of your workloads.
newRetentionSetting ::
  RetentionSetting
newRetentionSetting =
  RetentionSetting'
    { retentionPeriod =
        Prelude.Nothing,
      storageClass = Prelude.Nothing
    }

-- | The retention period specifies a fixed period of time during which the
-- Security Lake object remains locked. You can specify the retention
-- period in days for one or more sources.
retentionSetting_retentionPeriod :: Lens.Lens' RetentionSetting (Prelude.Maybe Prelude.Natural)
retentionSetting_retentionPeriod = Lens.lens (\RetentionSetting' {retentionPeriod} -> retentionPeriod) (\s@RetentionSetting' {} a -> s {retentionPeriod = a} :: RetentionSetting)

-- | The range of storage classes that you can choose from based on the data
-- access, resiliency, and cost requirements of your workloads.
retentionSetting_storageClass :: Lens.Lens' RetentionSetting (Prelude.Maybe StorageClass)
retentionSetting_storageClass = Lens.lens (\RetentionSetting' {storageClass} -> storageClass) (\s@RetentionSetting' {} a -> s {storageClass = a} :: RetentionSetting)

instance Data.FromJSON RetentionSetting where
  parseJSON =
    Data.withObject
      "RetentionSetting"
      ( \x ->
          RetentionSetting'
            Prelude.<$> (x Data..:? "retentionPeriod")
            Prelude.<*> (x Data..:? "storageClass")
      )

instance Prelude.Hashable RetentionSetting where
  hashWithSalt _salt RetentionSetting' {..} =
    _salt
      `Prelude.hashWithSalt` retentionPeriod
      `Prelude.hashWithSalt` storageClass

instance Prelude.NFData RetentionSetting where
  rnf RetentionSetting' {..} =
    Prelude.rnf retentionPeriod
      `Prelude.seq` Prelude.rnf storageClass

instance Data.ToJSON RetentionSetting where
  toJSON RetentionSetting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("retentionPeriod" Data..=)
              Prelude.<$> retentionPeriod,
            ("storageClass" Data..=) Prelude.<$> storageClass
          ]
      )
