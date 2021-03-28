{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.ProvisioningParameter
  ( ProvisioningParameter (..)
  -- * Smart constructor
  , mkProvisioningParameter
  -- * Lenses
  , ppKey
  , ppValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.Key as Types
import qualified Network.AWS.ServiceCatalog.Types.ParameterValue as Types

-- | Information about a parameter used to provision a product.
--
-- /See:/ 'mkProvisioningParameter' smart constructor.
data ProvisioningParameter = ProvisioningParameter'
  { key :: Core.Maybe Types.Key
    -- ^ The parameter key.
  , value :: Core.Maybe Types.ParameterValue
    -- ^ The parameter value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProvisioningParameter' value with any optional fields omitted.
mkProvisioningParameter
    :: ProvisioningParameter
mkProvisioningParameter
  = ProvisioningParameter'{key = Core.Nothing, value = Core.Nothing}

-- | The parameter key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppKey :: Lens.Lens' ProvisioningParameter (Core.Maybe Types.Key)
ppKey = Lens.field @"key"
{-# INLINEABLE ppKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The parameter value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppValue :: Lens.Lens' ProvisioningParameter (Core.Maybe Types.ParameterValue)
ppValue = Lens.field @"value"
{-# INLINEABLE ppValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON ProvisioningParameter where
        toJSON ProvisioningParameter{..}
          = Core.object
              (Core.catMaybes
                 [("Key" Core..=) Core.<$> key, ("Value" Core..=) Core.<$> value])
