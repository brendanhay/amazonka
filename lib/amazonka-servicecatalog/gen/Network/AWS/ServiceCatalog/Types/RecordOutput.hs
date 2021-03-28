{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.RecordOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.RecordOutput
  ( RecordOutput (..)
  -- * Smart constructor
  , mkRecordOutput
  -- * Lenses
  , roDescription
  , roOutputKey
  , roOutputValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.Description as Types
import qualified Network.AWS.ServiceCatalog.Types.OutputKey as Types
import qualified Network.AWS.ServiceCatalog.Types.OutputValue as Types

-- | The output for the product created as the result of a request. For example, the output for a CloudFormation-backed product that creates an S3 bucket would include the S3 bucket URL.
--
-- /See:/ 'mkRecordOutput' smart constructor.
data RecordOutput = RecordOutput'
  { description :: Core.Maybe Types.Description
    -- ^ The description of the output.
  , outputKey :: Core.Maybe Types.OutputKey
    -- ^ The output key.
  , outputValue :: Core.Maybe Types.OutputValue
    -- ^ The output value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RecordOutput' value with any optional fields omitted.
mkRecordOutput
    :: RecordOutput
mkRecordOutput
  = RecordOutput'{description = Core.Nothing,
                  outputKey = Core.Nothing, outputValue = Core.Nothing}

-- | The description of the output.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
roDescription :: Lens.Lens' RecordOutput (Core.Maybe Types.Description)
roDescription = Lens.field @"description"
{-# INLINEABLE roDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The output key.
--
-- /Note:/ Consider using 'outputKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
roOutputKey :: Lens.Lens' RecordOutput (Core.Maybe Types.OutputKey)
roOutputKey = Lens.field @"outputKey"
{-# INLINEABLE roOutputKey #-}
{-# DEPRECATED outputKey "Use generic-lens or generic-optics with 'outputKey' instead"  #-}

-- | The output value.
--
-- /Note:/ Consider using 'outputValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
roOutputValue :: Lens.Lens' RecordOutput (Core.Maybe Types.OutputValue)
roOutputValue = Lens.field @"outputValue"
{-# INLINEABLE roOutputValue #-}
{-# DEPRECATED outputValue "Use generic-lens or generic-optics with 'outputValue' instead"  #-}

instance Core.FromJSON RecordOutput where
        parseJSON
          = Core.withObject "RecordOutput" Core.$
              \ x ->
                RecordOutput' Core.<$>
                  (x Core..:? "Description") Core.<*> x Core..:? "OutputKey" Core.<*>
                    x Core..:? "OutputValue"
