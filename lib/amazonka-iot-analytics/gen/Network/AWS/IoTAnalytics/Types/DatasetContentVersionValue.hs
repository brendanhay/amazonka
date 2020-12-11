-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetContentVersionValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatasetContentVersionValue
  ( DatasetContentVersionValue (..),

    -- * Smart constructor
    mkDatasetContentVersionValue,

    -- * Lenses
    dcvvDatasetName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The dataset whose latest contents are used as input to the notebook or application.
--
-- /See:/ 'mkDatasetContentVersionValue' smart constructor.
newtype DatasetContentVersionValue = DatasetContentVersionValue'
  { datasetName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DatasetContentVersionValue' with the minimum fields required to make a request.
--
-- * 'datasetName' - The name of the dataset whose latest contents are used as input to the notebook or application.
mkDatasetContentVersionValue ::
  -- | 'datasetName'
  Lude.Text ->
  DatasetContentVersionValue
mkDatasetContentVersionValue pDatasetName_ =
  DatasetContentVersionValue' {datasetName = pDatasetName_}

-- | The name of the dataset whose latest contents are used as input to the notebook or application.
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvvDatasetName :: Lens.Lens' DatasetContentVersionValue Lude.Text
dcvvDatasetName = Lens.lens (datasetName :: DatasetContentVersionValue -> Lude.Text) (\s a -> s {datasetName = a} :: DatasetContentVersionValue)
{-# DEPRECATED dcvvDatasetName "Use generic-lens or generic-optics with 'datasetName' instead." #-}

instance Lude.FromJSON DatasetContentVersionValue where
  parseJSON =
    Lude.withObject
      "DatasetContentVersionValue"
      ( \x ->
          DatasetContentVersionValue' Lude.<$> (x Lude..: "datasetName")
      )

instance Lude.ToJSON DatasetContentVersionValue where
  toJSON DatasetContentVersionValue' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("datasetName" Lude..= datasetName)])
