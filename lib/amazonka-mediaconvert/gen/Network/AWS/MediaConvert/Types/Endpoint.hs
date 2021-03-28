{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Endpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Endpoint
  ( Endpoint (..)
  -- * Smart constructor
  , mkEndpoint
  -- * Lenses
  , eUrl
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an account-specific API endpoint.
--
-- /See:/ 'mkEndpoint' smart constructor.
newtype Endpoint = Endpoint'
  { url :: Core.Maybe Core.Text
    -- ^ URL of endpoint
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Endpoint' value with any optional fields omitted.
mkEndpoint
    :: Endpoint
mkEndpoint = Endpoint'{url = Core.Nothing}

-- | URL of endpoint
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eUrl :: Lens.Lens' Endpoint (Core.Maybe Core.Text)
eUrl = Lens.field @"url"
{-# INLINEABLE eUrl #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

instance Core.FromJSON Endpoint where
        parseJSON
          = Core.withObject "Endpoint" Core.$
              \ x -> Endpoint' Core.<$> (x Core..:? "url")
