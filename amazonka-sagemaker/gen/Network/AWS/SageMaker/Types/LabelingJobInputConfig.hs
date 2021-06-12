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
-- Module      : Network.AWS.SageMaker.Types.LabelingJobInputConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelingJobInputConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.LabelingJobDataAttributes
import Network.AWS.SageMaker.Types.LabelingJobDataSource

-- | Input configuration information for a labeling job.
--
-- /See:/ 'newLabelingJobInputConfig' smart constructor.
data LabelingJobInputConfig = LabelingJobInputConfig'
  { -- | Attributes of the data specified by the customer.
    dataAttributes :: Core.Maybe LabelingJobDataAttributes,
    -- | The location of the input data.
    dataSource :: LabelingJobDataSource
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      dataSource = pDataSource_
    }

-- | Attributes of the data specified by the customer.
labelingJobInputConfig_dataAttributes :: Lens.Lens' LabelingJobInputConfig (Core.Maybe LabelingJobDataAttributes)
labelingJobInputConfig_dataAttributes = Lens.lens (\LabelingJobInputConfig' {dataAttributes} -> dataAttributes) (\s@LabelingJobInputConfig' {} a -> s {dataAttributes = a} :: LabelingJobInputConfig)

-- | The location of the input data.
labelingJobInputConfig_dataSource :: Lens.Lens' LabelingJobInputConfig LabelingJobDataSource
labelingJobInputConfig_dataSource = Lens.lens (\LabelingJobInputConfig' {dataSource} -> dataSource) (\s@LabelingJobInputConfig' {} a -> s {dataSource = a} :: LabelingJobInputConfig)

instance Core.FromJSON LabelingJobInputConfig where
  parseJSON =
    Core.withObject
      "LabelingJobInputConfig"
      ( \x ->
          LabelingJobInputConfig'
            Core.<$> (x Core..:? "DataAttributes")
            Core.<*> (x Core..: "DataSource")
      )

instance Core.Hashable LabelingJobInputConfig

instance Core.NFData LabelingJobInputConfig

instance Core.ToJSON LabelingJobInputConfig where
  toJSON LabelingJobInputConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DataAttributes" Core..=) Core.<$> dataAttributes,
            Core.Just ("DataSource" Core..= dataSource)
          ]
      )
