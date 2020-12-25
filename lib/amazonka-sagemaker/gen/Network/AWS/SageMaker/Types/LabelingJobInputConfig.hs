{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.LabelingJobInputConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelingJobInputConfig
  ( LabelingJobInputConfig (..),

    -- * Smart constructor
    mkLabelingJobInputConfig,

    -- * Lenses
    ljicDataSource,
    ljicDataAttributes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.LabelingJobDataAttributes as Types
import qualified Network.AWS.SageMaker.Types.LabelingJobDataSource as Types

-- | Input configuration information for a labeling job.
--
-- /See:/ 'mkLabelingJobInputConfig' smart constructor.
data LabelingJobInputConfig = LabelingJobInputConfig'
  { -- | The location of the input data.
    dataSource :: Types.LabelingJobDataSource,
    -- | Attributes of the data specified by the customer.
    dataAttributes :: Core.Maybe Types.LabelingJobDataAttributes
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LabelingJobInputConfig' value with any optional fields omitted.
mkLabelingJobInputConfig ::
  -- | 'dataSource'
  Types.LabelingJobDataSource ->
  LabelingJobInputConfig
mkLabelingJobInputConfig dataSource =
  LabelingJobInputConfig'
    { dataSource,
      dataAttributes = Core.Nothing
    }

-- | The location of the input data.
--
-- /Note:/ Consider using 'dataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljicDataSource :: Lens.Lens' LabelingJobInputConfig Types.LabelingJobDataSource
ljicDataSource = Lens.field @"dataSource"
{-# DEPRECATED ljicDataSource "Use generic-lens or generic-optics with 'dataSource' instead." #-}

-- | Attributes of the data specified by the customer.
--
-- /Note:/ Consider using 'dataAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljicDataAttributes :: Lens.Lens' LabelingJobInputConfig (Core.Maybe Types.LabelingJobDataAttributes)
ljicDataAttributes = Lens.field @"dataAttributes"
{-# DEPRECATED ljicDataAttributes "Use generic-lens or generic-optics with 'dataAttributes' instead." #-}

instance Core.FromJSON LabelingJobInputConfig where
  toJSON LabelingJobInputConfig {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DataSource" Core..= dataSource),
            ("DataAttributes" Core..=) Core.<$> dataAttributes
          ]
      )

instance Core.FromJSON LabelingJobInputConfig where
  parseJSON =
    Core.withObject "LabelingJobInputConfig" Core.$
      \x ->
        LabelingJobInputConfig'
          Core.<$> (x Core..: "DataSource") Core.<*> (x Core..:? "DataAttributes")
