{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.HttpUrlDestinationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.HttpUrlDestinationConfiguration
  ( HttpUrlDestinationConfiguration (..)
  -- * Smart constructor
  , mkHttpUrlDestinationConfiguration
  -- * Lenses
  , hudcConfirmationUrl
  ) where

import qualified Network.AWS.IoT.Types.ConfirmationUrl as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | HTTP URL destination configuration used by the topic rule's HTTP action.
--
-- /See:/ 'mkHttpUrlDestinationConfiguration' smart constructor.
newtype HttpUrlDestinationConfiguration = HttpUrlDestinationConfiguration'
  { confirmationUrl :: Types.ConfirmationUrl
    -- ^ The URL AWS IoT uses to confirm ownership of or access to the topic rule destination URL.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'HttpUrlDestinationConfiguration' value with any optional fields omitted.
mkHttpUrlDestinationConfiguration
    :: Types.ConfirmationUrl -- ^ 'confirmationUrl'
    -> HttpUrlDestinationConfiguration
mkHttpUrlDestinationConfiguration confirmationUrl
  = HttpUrlDestinationConfiguration'{confirmationUrl}

-- | The URL AWS IoT uses to confirm ownership of or access to the topic rule destination URL.
--
-- /Note:/ Consider using 'confirmationUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hudcConfirmationUrl :: Lens.Lens' HttpUrlDestinationConfiguration Types.ConfirmationUrl
hudcConfirmationUrl = Lens.field @"confirmationUrl"
{-# INLINEABLE hudcConfirmationUrl #-}
{-# DEPRECATED confirmationUrl "Use generic-lens or generic-optics with 'confirmationUrl' instead"  #-}

instance Core.FromJSON HttpUrlDestinationConfiguration where
        toJSON HttpUrlDestinationConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("confirmationUrl" Core..= confirmationUrl)])
