{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.HttpUrlDestinationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.HttpUrlDestinationSummary
  ( HttpUrlDestinationSummary (..)
  -- * Smart constructor
  , mkHttpUrlDestinationSummary
  -- * Lenses
  , hudsConfirmationUrl
  ) where

import qualified Network.AWS.IoT.Types.Url as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about an HTTP URL destination.
--
-- /See:/ 'mkHttpUrlDestinationSummary' smart constructor.
newtype HttpUrlDestinationSummary = HttpUrlDestinationSummary'
  { confirmationUrl :: Core.Maybe Types.Url
    -- ^ The URL used to confirm ownership of or access to the HTTP topic rule destination URL.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'HttpUrlDestinationSummary' value with any optional fields omitted.
mkHttpUrlDestinationSummary
    :: HttpUrlDestinationSummary
mkHttpUrlDestinationSummary
  = HttpUrlDestinationSummary'{confirmationUrl = Core.Nothing}

-- | The URL used to confirm ownership of or access to the HTTP topic rule destination URL.
--
-- /Note:/ Consider using 'confirmationUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hudsConfirmationUrl :: Lens.Lens' HttpUrlDestinationSummary (Core.Maybe Types.Url)
hudsConfirmationUrl = Lens.field @"confirmationUrl"
{-# INLINEABLE hudsConfirmationUrl #-}
{-# DEPRECATED confirmationUrl "Use generic-lens or generic-optics with 'confirmationUrl' instead"  #-}

instance Core.FromJSON HttpUrlDestinationSummary where
        parseJSON
          = Core.withObject "HttpUrlDestinationSummary" Core.$
              \ x ->
                HttpUrlDestinationSummary' Core.<$> (x Core..:? "confirmationUrl")
