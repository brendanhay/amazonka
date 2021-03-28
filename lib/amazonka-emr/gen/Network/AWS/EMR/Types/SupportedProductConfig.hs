{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.SupportedProductConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.SupportedProductConfig
  ( SupportedProductConfig (..)
  -- * Smart constructor
  , mkSupportedProductConfig
  -- * Lenses
  , spcArgs
  , spcName
  ) where

import qualified Network.AWS.EMR.Types.Name as Types
import qualified Network.AWS.EMR.Types.XmlString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The list of supported product configurations which allow user-supplied arguments. EMR accepts these arguments and forwards them to the corresponding installation script as bootstrap action arguments.
--
-- /See:/ 'mkSupportedProductConfig' smart constructor.
data SupportedProductConfig = SupportedProductConfig'
  { args :: Core.Maybe [Types.XmlString]
    -- ^ The list of user-supplied arguments.
  , name :: Core.Maybe Types.Name
    -- ^ The name of the product configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SupportedProductConfig' value with any optional fields omitted.
mkSupportedProductConfig
    :: SupportedProductConfig
mkSupportedProductConfig
  = SupportedProductConfig'{args = Core.Nothing, name = Core.Nothing}

-- | The list of user-supplied arguments.
--
-- /Note:/ Consider using 'args' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spcArgs :: Lens.Lens' SupportedProductConfig (Core.Maybe [Types.XmlString])
spcArgs = Lens.field @"args"
{-# INLINEABLE spcArgs #-}
{-# DEPRECATED args "Use generic-lens or generic-optics with 'args' instead"  #-}

-- | The name of the product configuration.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spcName :: Lens.Lens' SupportedProductConfig (Core.Maybe Types.Name)
spcName = Lens.field @"name"
{-# INLINEABLE spcName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON SupportedProductConfig where
        toJSON SupportedProductConfig{..}
          = Core.object
              (Core.catMaybes
                 [("Args" Core..=) Core.<$> args, ("Name" Core..=) Core.<$> name])
