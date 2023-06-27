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
-- Module      : Amazonka.TimeStreamWrite.Types.DataModelConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamWrite.Types.DataModelConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TimeStreamWrite.Types.DataModel
import Amazonka.TimeStreamWrite.Types.DataModelS3Configuration

-- |
--
-- /See:/ 'newDataModelConfiguration' smart constructor.
data DataModelConfiguration = DataModelConfiguration'
  { dataModel :: Prelude.Maybe DataModel,
    dataModelS3Configuration :: Prelude.Maybe DataModelS3Configuration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataModelConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataModel', 'dataModelConfiguration_dataModel' -
--
-- 'dataModelS3Configuration', 'dataModelConfiguration_dataModelS3Configuration' -
newDataModelConfiguration ::
  DataModelConfiguration
newDataModelConfiguration =
  DataModelConfiguration'
    { dataModel =
        Prelude.Nothing,
      dataModelS3Configuration = Prelude.Nothing
    }

dataModelConfiguration_dataModel :: Lens.Lens' DataModelConfiguration (Prelude.Maybe DataModel)
dataModelConfiguration_dataModel = Lens.lens (\DataModelConfiguration' {dataModel} -> dataModel) (\s@DataModelConfiguration' {} a -> s {dataModel = a} :: DataModelConfiguration)

dataModelConfiguration_dataModelS3Configuration :: Lens.Lens' DataModelConfiguration (Prelude.Maybe DataModelS3Configuration)
dataModelConfiguration_dataModelS3Configuration = Lens.lens (\DataModelConfiguration' {dataModelS3Configuration} -> dataModelS3Configuration) (\s@DataModelConfiguration' {} a -> s {dataModelS3Configuration = a} :: DataModelConfiguration)

instance Data.FromJSON DataModelConfiguration where
  parseJSON =
    Data.withObject
      "DataModelConfiguration"
      ( \x ->
          DataModelConfiguration'
            Prelude.<$> (x Data..:? "DataModel")
            Prelude.<*> (x Data..:? "DataModelS3Configuration")
      )

instance Prelude.Hashable DataModelConfiguration where
  hashWithSalt _salt DataModelConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` dataModel
      `Prelude.hashWithSalt` dataModelS3Configuration

instance Prelude.NFData DataModelConfiguration where
  rnf DataModelConfiguration' {..} =
    Prelude.rnf dataModel
      `Prelude.seq` Prelude.rnf dataModelS3Configuration

instance Data.ToJSON DataModelConfiguration where
  toJSON DataModelConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataModel" Data..=) Prelude.<$> dataModel,
            ("DataModelS3Configuration" Data..=)
              Prelude.<$> dataModelS3Configuration
          ]
      )
