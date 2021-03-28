{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrialComponentParameterValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.TrialComponentParameterValue
  ( TrialComponentParameterValue (..)
  -- * Smart constructor
  , mkTrialComponentParameterValue
  -- * Lenses
  , tcpvNumberValue
  , tcpvStringValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.StringValue as Types

-- | The value of a hyperparameter. Only one of @NumberValue@ or @StringValue@ can be specified.
--
-- This object is specified in the 'CreateTrialComponent' request.
--
-- /See:/ 'mkTrialComponentParameterValue' smart constructor.
data TrialComponentParameterValue = TrialComponentParameterValue'
  { numberValue :: Core.Maybe Core.Double
    -- ^ The numeric value of a numeric hyperparameter. If you specify a value for this parameter, you can't specify the @StringValue@ parameter.
  , stringValue :: Core.Maybe Types.StringValue
    -- ^ The string value of a categorical hyperparameter. If you specify a value for this parameter, you can't specify the @NumberValue@ parameter.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TrialComponentParameterValue' value with any optional fields omitted.
mkTrialComponentParameterValue
    :: TrialComponentParameterValue
mkTrialComponentParameterValue
  = TrialComponentParameterValue'{numberValue = Core.Nothing,
                                  stringValue = Core.Nothing}

-- | The numeric value of a numeric hyperparameter. If you specify a value for this parameter, you can't specify the @StringValue@ parameter.
--
-- /Note:/ Consider using 'numberValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcpvNumberValue :: Lens.Lens' TrialComponentParameterValue (Core.Maybe Core.Double)
tcpvNumberValue = Lens.field @"numberValue"
{-# INLINEABLE tcpvNumberValue #-}
{-# DEPRECATED numberValue "Use generic-lens or generic-optics with 'numberValue' instead"  #-}

-- | The string value of a categorical hyperparameter. If you specify a value for this parameter, you can't specify the @NumberValue@ parameter.
--
-- /Note:/ Consider using 'stringValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcpvStringValue :: Lens.Lens' TrialComponentParameterValue (Core.Maybe Types.StringValue)
tcpvStringValue = Lens.field @"stringValue"
{-# INLINEABLE tcpvStringValue #-}
{-# DEPRECATED stringValue "Use generic-lens or generic-optics with 'stringValue' instead"  #-}

instance Core.FromJSON TrialComponentParameterValue where
        toJSON TrialComponentParameterValue{..}
          = Core.object
              (Core.catMaybes
                 [("NumberValue" Core..=) Core.<$> numberValue,
                  ("StringValue" Core..=) Core.<$> stringValue])

instance Core.FromJSON TrialComponentParameterValue where
        parseJSON
          = Core.withObject "TrialComponentParameterValue" Core.$
              \ x ->
                TrialComponentParameterValue' Core.<$>
                  (x Core..:? "NumberValue") Core.<*> x Core..:? "StringValue"
