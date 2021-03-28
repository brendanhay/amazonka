{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ParameterConstraints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.ParameterConstraints
  ( ParameterConstraints (..)
  -- * Smart constructor
  , mkParameterConstraints
  -- * Lenses
  , pcAllowedValues
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.AllowedValue as Types

-- | The constraints that the administrator has put on the parameter.
--
-- /See:/ 'mkParameterConstraints' smart constructor.
newtype ParameterConstraints = ParameterConstraints'
  { allowedValues :: Core.Maybe [Types.AllowedValue]
    -- ^ The values that the administrator has allowed for the parameter.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ParameterConstraints' value with any optional fields omitted.
mkParameterConstraints
    :: ParameterConstraints
mkParameterConstraints
  = ParameterConstraints'{allowedValues = Core.Nothing}

-- | The values that the administrator has allowed for the parameter.
--
-- /Note:/ Consider using 'allowedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcAllowedValues :: Lens.Lens' ParameterConstraints (Core.Maybe [Types.AllowedValue])
pcAllowedValues = Lens.field @"allowedValues"
{-# INLINEABLE pcAllowedValues #-}
{-# DEPRECATED allowedValues "Use generic-lens or generic-optics with 'allowedValues' instead"  #-}

instance Core.FromJSON ParameterConstraints where
        parseJSON
          = Core.withObject "ParameterConstraints" Core.$
              \ x -> ParameterConstraints' Core.<$> (x Core..:? "AllowedValues")
