{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetContentVersionValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.DatasetContentVersionValue
  ( DatasetContentVersionValue (..)
  -- * Smart constructor
  , mkDatasetContentVersionValue
  -- * Lenses
  , dcvvDatasetName
  ) where

import qualified Network.AWS.IoTAnalytics.Types.DatasetName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The dataset whose latest contents are used as input to the notebook or application.
--
-- /See:/ 'mkDatasetContentVersionValue' smart constructor.
newtype DatasetContentVersionValue = DatasetContentVersionValue'
  { datasetName :: Types.DatasetName
    -- ^ The name of the dataset whose latest contents are used as input to the notebook or application.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DatasetContentVersionValue' value with any optional fields omitted.
mkDatasetContentVersionValue
    :: Types.DatasetName -- ^ 'datasetName'
    -> DatasetContentVersionValue
mkDatasetContentVersionValue datasetName
  = DatasetContentVersionValue'{datasetName}

-- | The name of the dataset whose latest contents are used as input to the notebook or application.
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvvDatasetName :: Lens.Lens' DatasetContentVersionValue Types.DatasetName
dcvvDatasetName = Lens.field @"datasetName"
{-# INLINEABLE dcvvDatasetName #-}
{-# DEPRECATED datasetName "Use generic-lens or generic-optics with 'datasetName' instead"  #-}

instance Core.FromJSON DatasetContentVersionValue where
        toJSON DatasetContentVersionValue{..}
          = Core.object
              (Core.catMaybes [Core.Just ("datasetName" Core..= datasetName)])

instance Core.FromJSON DatasetContentVersionValue where
        parseJSON
          = Core.withObject "DatasetContentVersionValue" Core.$
              \ x ->
                DatasetContentVersionValue' Core.<$> (x Core..: "datasetName")
