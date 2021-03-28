{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.Variable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.Variable
  ( Variable (..)
  -- * Smart constructor
  , mkVariable
  -- * Lenses
  , vName
  , vDatasetContentVersionValue
  , vDoubleValue
  , vOutputFileUriValue
  , vStringValue
  ) where

import qualified Network.AWS.IoTAnalytics.Types.DatasetContentVersionValue as Types
import qualified Network.AWS.IoTAnalytics.Types.Name as Types
import qualified Network.AWS.IoTAnalytics.Types.OutputFileUriValue as Types
import qualified Network.AWS.IoTAnalytics.Types.StringValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An instance of a variable to be passed to the @containerAction@ execution. Each variable must have a name and a value given by one of @stringValue@ , @datasetContentVersionValue@ , or @outputFileUriValue@ .
--
-- /See:/ 'mkVariable' smart constructor.
data Variable = Variable'
  { name :: Types.Name
    -- ^ The name of the variable.
  , datasetContentVersionValue :: Core.Maybe Types.DatasetContentVersionValue
    -- ^ The value of the variable as a structure that specifies a dataset content version.
  , doubleValue :: Core.Maybe Core.Double
    -- ^ The value of the variable as a double (numeric).
  , outputFileUriValue :: Core.Maybe Types.OutputFileUriValue
    -- ^ The value of the variable as a structure that specifies an output file URI.
  , stringValue :: Core.Maybe Types.StringValue
    -- ^ The value of the variable as a string.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Variable' value with any optional fields omitted.
mkVariable
    :: Types.Name -- ^ 'name'
    -> Variable
mkVariable name
  = Variable'{name, datasetContentVersionValue = Core.Nothing,
              doubleValue = Core.Nothing, outputFileUriValue = Core.Nothing,
              stringValue = Core.Nothing}

-- | The name of the variable.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vName :: Lens.Lens' Variable Types.Name
vName = Lens.field @"name"
{-# INLINEABLE vName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The value of the variable as a structure that specifies a dataset content version.
--
-- /Note:/ Consider using 'datasetContentVersionValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vDatasetContentVersionValue :: Lens.Lens' Variable (Core.Maybe Types.DatasetContentVersionValue)
vDatasetContentVersionValue = Lens.field @"datasetContentVersionValue"
{-# INLINEABLE vDatasetContentVersionValue #-}
{-# DEPRECATED datasetContentVersionValue "Use generic-lens or generic-optics with 'datasetContentVersionValue' instead"  #-}

-- | The value of the variable as a double (numeric).
--
-- /Note:/ Consider using 'doubleValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vDoubleValue :: Lens.Lens' Variable (Core.Maybe Core.Double)
vDoubleValue = Lens.field @"doubleValue"
{-# INLINEABLE vDoubleValue #-}
{-# DEPRECATED doubleValue "Use generic-lens or generic-optics with 'doubleValue' instead"  #-}

-- | The value of the variable as a structure that specifies an output file URI.
--
-- /Note:/ Consider using 'outputFileUriValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vOutputFileUriValue :: Lens.Lens' Variable (Core.Maybe Types.OutputFileUriValue)
vOutputFileUriValue = Lens.field @"outputFileUriValue"
{-# INLINEABLE vOutputFileUriValue #-}
{-# DEPRECATED outputFileUriValue "Use generic-lens or generic-optics with 'outputFileUriValue' instead"  #-}

-- | The value of the variable as a string.
--
-- /Note:/ Consider using 'stringValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vStringValue :: Lens.Lens' Variable (Core.Maybe Types.StringValue)
vStringValue = Lens.field @"stringValue"
{-# INLINEABLE vStringValue #-}
{-# DEPRECATED stringValue "Use generic-lens or generic-optics with 'stringValue' instead"  #-}

instance Core.FromJSON Variable where
        toJSON Variable{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name),
                  ("datasetContentVersionValue" Core..=) Core.<$>
                    datasetContentVersionValue,
                  ("doubleValue" Core..=) Core.<$> doubleValue,
                  ("outputFileUriValue" Core..=) Core.<$> outputFileUriValue,
                  ("stringValue" Core..=) Core.<$> stringValue])

instance Core.FromJSON Variable where
        parseJSON
          = Core.withObject "Variable" Core.$
              \ x ->
                Variable' Core.<$>
                  (x Core..: "name") Core.<*> x Core..:? "datasetContentVersionValue"
                    Core.<*> x Core..:? "doubleValue"
                    Core.<*> x Core..:? "outputFileUriValue"
                    Core.<*> x Core..:? "stringValue"
