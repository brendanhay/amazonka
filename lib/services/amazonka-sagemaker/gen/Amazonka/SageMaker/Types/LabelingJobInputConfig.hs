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
-- Module      : Amazonka.SageMaker.Types.LabelingJobInputConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.LabelingJobInputConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.LabelingJobDataAttributes
import Amazonka.SageMaker.Types.LabelingJobDataSource

-- | Input configuration information for a labeling job.
--
-- /See:/ 'newLabelingJobInputConfig' smart constructor.
data LabelingJobInputConfig = LabelingJobInputConfig'
  { -- | Attributes of the data specified by the customer.
    dataAttributes :: Prelude.Maybe LabelingJobDataAttributes,
    -- | The location of the input data.
    dataSource :: LabelingJobDataSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LabelingJobInputConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataAttributes', 'labelingJobInputConfig_dataAttributes' - Attributes of the data specified by the customer.
--
-- 'dataSource', 'labelingJobInputConfig_dataSource' - The location of the input data.
newLabelingJobInputConfig ::
  -- | 'dataSource'
  LabelingJobDataSource ->
  LabelingJobInputConfig
newLabelingJobInputConfig pDataSource_ =
  LabelingJobInputConfig'
    { dataAttributes =
        Prelude.Nothing,
      dataSource = pDataSource_
    }

-- | Attributes of the data specified by the customer.
labelingJobInputConfig_dataAttributes :: Lens.Lens' LabelingJobInputConfig (Prelude.Maybe LabelingJobDataAttributes)
labelingJobInputConfig_dataAttributes = Lens.lens (\LabelingJobInputConfig' {dataAttributes} -> dataAttributes) (\s@LabelingJobInputConfig' {} a -> s {dataAttributes = a} :: LabelingJobInputConfig)

-- | The location of the input data.
labelingJobInputConfig_dataSource :: Lens.Lens' LabelingJobInputConfig LabelingJobDataSource
labelingJobInputConfig_dataSource = Lens.lens (\LabelingJobInputConfig' {dataSource} -> dataSource) (\s@LabelingJobInputConfig' {} a -> s {dataSource = a} :: LabelingJobInputConfig)

instance Data.FromJSON LabelingJobInputConfig where
  parseJSON =
    Data.withObject
      "LabelingJobInputConfig"
      ( \x ->
          LabelingJobInputConfig'
            Prelude.<$> (x Data..:? "DataAttributes")
            Prelude.<*> (x Data..: "DataSource")
      )

instance Prelude.Hashable LabelingJobInputConfig where
  hashWithSalt _salt LabelingJobInputConfig' {..} =
    _salt
      `Prelude.hashWithSalt` dataAttributes
      `Prelude.hashWithSalt` dataSource

instance Prelude.NFData LabelingJobInputConfig where
  rnf LabelingJobInputConfig' {..} =
    Prelude.rnf dataAttributes
      `Prelude.seq` Prelude.rnf dataSource

instance Data.ToJSON LabelingJobInputConfig where
  toJSON LabelingJobInputConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataAttributes" Data..=)
              Prelude.<$> dataAttributes,
            Prelude.Just ("DataSource" Data..= dataSource)
          ]
      )
