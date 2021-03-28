{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ProcessorParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.ProcessorParameter
  ( ProcessorParameter (..)
  -- * Smart constructor
  , mkProcessorParameter
  -- * Lenses
  , ppParameterName
  , ppParameterValue
  ) where

import qualified Network.AWS.Firehose.Types.ParameterValue as Types
import qualified Network.AWS.Firehose.Types.ProcessorParameterName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the processor parameter.
--
-- /See:/ 'mkProcessorParameter' smart constructor.
data ProcessorParameter = ProcessorParameter'
  { parameterName :: Types.ProcessorParameterName
    -- ^ The name of the parameter.
  , parameterValue :: Types.ParameterValue
    -- ^ The parameter value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProcessorParameter' value with any optional fields omitted.
mkProcessorParameter
    :: Types.ProcessorParameterName -- ^ 'parameterName'
    -> Types.ParameterValue -- ^ 'parameterValue'
    -> ProcessorParameter
mkProcessorParameter parameterName parameterValue
  = ProcessorParameter'{parameterName, parameterValue}

-- | The name of the parameter.
--
-- /Note:/ Consider using 'parameterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppParameterName :: Lens.Lens' ProcessorParameter Types.ProcessorParameterName
ppParameterName = Lens.field @"parameterName"
{-# INLINEABLE ppParameterName #-}
{-# DEPRECATED parameterName "Use generic-lens or generic-optics with 'parameterName' instead"  #-}

-- | The parameter value.
--
-- /Note:/ Consider using 'parameterValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppParameterValue :: Lens.Lens' ProcessorParameter Types.ParameterValue
ppParameterValue = Lens.field @"parameterValue"
{-# INLINEABLE ppParameterValue #-}
{-# DEPRECATED parameterValue "Use generic-lens or generic-optics with 'parameterValue' instead"  #-}

instance Core.FromJSON ProcessorParameter where
        toJSON ProcessorParameter{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ParameterName" Core..= parameterName),
                  Core.Just ("ParameterValue" Core..= parameterValue)])

instance Core.FromJSON ProcessorParameter where
        parseJSON
          = Core.withObject "ProcessorParameter" Core.$
              \ x ->
                ProcessorParameter' Core.<$>
                  (x Core..: "ParameterName") Core.<*> x Core..: "ParameterValue"
