{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.ParameterNameValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DAX.Types.ParameterNameValue
  ( ParameterNameValue (..)
  -- * Smart constructor
  , mkParameterNameValue
  -- * Lenses
  , pnvParameterName
  , pnvParameterValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An individual DAX parameter.
--
-- /See:/ 'mkParameterNameValue' smart constructor.
data ParameterNameValue = ParameterNameValue'
  { parameterName :: Core.Maybe Core.Text
    -- ^ The name of the parameter.
  , parameterValue :: Core.Maybe Core.Text
    -- ^ The value of the parameter.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ParameterNameValue' value with any optional fields omitted.
mkParameterNameValue
    :: ParameterNameValue
mkParameterNameValue
  = ParameterNameValue'{parameterName = Core.Nothing,
                        parameterValue = Core.Nothing}

-- | The name of the parameter.
--
-- /Note:/ Consider using 'parameterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pnvParameterName :: Lens.Lens' ParameterNameValue (Core.Maybe Core.Text)
pnvParameterName = Lens.field @"parameterName"
{-# INLINEABLE pnvParameterName #-}
{-# DEPRECATED parameterName "Use generic-lens or generic-optics with 'parameterName' instead"  #-}

-- | The value of the parameter.
--
-- /Note:/ Consider using 'parameterValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pnvParameterValue :: Lens.Lens' ParameterNameValue (Core.Maybe Core.Text)
pnvParameterValue = Lens.field @"parameterValue"
{-# INLINEABLE pnvParameterValue #-}
{-# DEPRECATED parameterValue "Use generic-lens or generic-optics with 'parameterValue' instead"  #-}

instance Core.FromJSON ParameterNameValue where
        toJSON ParameterNameValue{..}
          = Core.object
              (Core.catMaybes
                 [("ParameterName" Core..=) Core.<$> parameterName,
                  ("ParameterValue" Core..=) Core.<$> parameterValue])
