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
-- Module      : Amazonka.Personalize.Types.DataSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.DataSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the data source that contains the data to upload to a dataset.
--
-- /See:/ 'newDataSource' smart constructor.
data DataSource = DataSource'
  { -- | The path to the Amazon S3 bucket where the data that you want to upload
    -- to your dataset is stored. For example:
    --
    -- @s3:\/\/bucket-name\/folder-name\/@
    dataLocation :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataLocation', 'dataSource_dataLocation' - The path to the Amazon S3 bucket where the data that you want to upload
-- to your dataset is stored. For example:
--
-- @s3:\/\/bucket-name\/folder-name\/@
newDataSource ::
  DataSource
newDataSource =
  DataSource' {dataLocation = Prelude.Nothing}

-- | The path to the Amazon S3 bucket where the data that you want to upload
-- to your dataset is stored. For example:
--
-- @s3:\/\/bucket-name\/folder-name\/@
dataSource_dataLocation :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Text)
dataSource_dataLocation = Lens.lens (\DataSource' {dataLocation} -> dataLocation) (\s@DataSource' {} a -> s {dataLocation = a} :: DataSource)

instance Core.FromJSON DataSource where
  parseJSON =
    Core.withObject
      "DataSource"
      ( \x ->
          DataSource' Prelude.<$> (x Core..:? "dataLocation")
      )

instance Prelude.Hashable DataSource where
  hashWithSalt _salt DataSource' {..} =
    _salt `Prelude.hashWithSalt` dataLocation

instance Prelude.NFData DataSource where
  rnf DataSource' {..} = Prelude.rnf dataLocation

instance Core.ToJSON DataSource where
  toJSON DataSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [("dataLocation" Core..=) Prelude.<$> dataLocation]
      )
