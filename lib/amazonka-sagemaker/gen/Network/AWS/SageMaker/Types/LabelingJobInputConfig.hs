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
    ljicDataAttributes,
    ljicDataSource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.LabelingJobDataAttributes
import Network.AWS.SageMaker.Types.LabelingJobDataSource

-- | Input configuration information for a labeling job.
--
-- /See:/ 'mkLabelingJobInputConfig' smart constructor.
data LabelingJobInputConfig = LabelingJobInputConfig'
  { -- | Attributes of the data specified by the customer.
    dataAttributes :: Lude.Maybe LabelingJobDataAttributes,
    -- | The location of the input data.
    dataSource :: LabelingJobDataSource
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LabelingJobInputConfig' with the minimum fields required to make a request.
--
-- * 'dataAttributes' - Attributes of the data specified by the customer.
-- * 'dataSource' - The location of the input data.
mkLabelingJobInputConfig ::
  -- | 'dataSource'
  LabelingJobDataSource ->
  LabelingJobInputConfig
mkLabelingJobInputConfig pDataSource_ =
  LabelingJobInputConfig'
    { dataAttributes = Lude.Nothing,
      dataSource = pDataSource_
    }

-- | Attributes of the data specified by the customer.
--
-- /Note:/ Consider using 'dataAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljicDataAttributes :: Lens.Lens' LabelingJobInputConfig (Lude.Maybe LabelingJobDataAttributes)
ljicDataAttributes = Lens.lens (dataAttributes :: LabelingJobInputConfig -> Lude.Maybe LabelingJobDataAttributes) (\s a -> s {dataAttributes = a} :: LabelingJobInputConfig)
{-# DEPRECATED ljicDataAttributes "Use generic-lens or generic-optics with 'dataAttributes' instead." #-}

-- | The location of the input data.
--
-- /Note:/ Consider using 'dataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljicDataSource :: Lens.Lens' LabelingJobInputConfig LabelingJobDataSource
ljicDataSource = Lens.lens (dataSource :: LabelingJobInputConfig -> LabelingJobDataSource) (\s a -> s {dataSource = a} :: LabelingJobInputConfig)
{-# DEPRECATED ljicDataSource "Use generic-lens or generic-optics with 'dataSource' instead." #-}

instance Lude.FromJSON LabelingJobInputConfig where
  parseJSON =
    Lude.withObject
      "LabelingJobInputConfig"
      ( \x ->
          LabelingJobInputConfig'
            Lude.<$> (x Lude..:? "DataAttributes") Lude.<*> (x Lude..: "DataSource")
      )

instance Lude.ToJSON LabelingJobInputConfig where
  toJSON LabelingJobInputConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DataAttributes" Lude..=) Lude.<$> dataAttributes,
            Lude.Just ("DataSource" Lude..= dataSource)
          ]
      )
