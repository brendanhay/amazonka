{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.HttpUrlDestinationProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.HttpUrlDestinationProperties
  ( HttpUrlDestinationProperties (..)
  -- * Smart constructor
  , mkHttpUrlDestinationProperties
  -- * Lenses
  , hudpConfirmationUrl
  ) where

import qualified Network.AWS.IoT.Types.Url as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | HTTP URL destination properties.
--
-- /See:/ 'mkHttpUrlDestinationProperties' smart constructor.
newtype HttpUrlDestinationProperties = HttpUrlDestinationProperties'
  { confirmationUrl :: Core.Maybe Types.Url
    -- ^ The URL used to confirm the HTTP topic rule destination URL.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'HttpUrlDestinationProperties' value with any optional fields omitted.
mkHttpUrlDestinationProperties
    :: HttpUrlDestinationProperties
mkHttpUrlDestinationProperties
  = HttpUrlDestinationProperties'{confirmationUrl = Core.Nothing}

-- | The URL used to confirm the HTTP topic rule destination URL.
--
-- /Note:/ Consider using 'confirmationUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hudpConfirmationUrl :: Lens.Lens' HttpUrlDestinationProperties (Core.Maybe Types.Url)
hudpConfirmationUrl = Lens.field @"confirmationUrl"
{-# INLINEABLE hudpConfirmationUrl #-}
{-# DEPRECATED confirmationUrl "Use generic-lens or generic-optics with 'confirmationUrl' instead"  #-}

instance Core.FromJSON HttpUrlDestinationProperties where
        parseJSON
          = Core.withObject "HttpUrlDestinationProperties" Core.$
              \ x ->
                HttpUrlDestinationProperties' Core.<$>
                  (x Core..:? "confirmationUrl")
