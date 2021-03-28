{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.HttpEndpointDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.HttpEndpointDescription
  ( HttpEndpointDescription (..)
  -- * Smart constructor
  , mkHttpEndpointDescription
  -- * Lenses
  , hedName
  , hedUrl
  ) where

import qualified Network.AWS.Firehose.Types.HttpEndpointName as Types
import qualified Network.AWS.Firehose.Types.Url as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the HTTP endpoint selected as the destination. 
--
-- /See:/ 'mkHttpEndpointDescription' smart constructor.
data HttpEndpointDescription = HttpEndpointDescription'
  { name :: Core.Maybe Types.HttpEndpointName
    -- ^ The name of the HTTP endpoint selected as the destination.
  , url :: Core.Maybe Types.Url
    -- ^ The URL of the HTTP endpoint selected as the destination.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HttpEndpointDescription' value with any optional fields omitted.
mkHttpEndpointDescription
    :: HttpEndpointDescription
mkHttpEndpointDescription
  = HttpEndpointDescription'{name = Core.Nothing, url = Core.Nothing}

-- | The name of the HTTP endpoint selected as the destination.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hedName :: Lens.Lens' HttpEndpointDescription (Core.Maybe Types.HttpEndpointName)
hedName = Lens.field @"name"
{-# INLINEABLE hedName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The URL of the HTTP endpoint selected as the destination.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hedUrl :: Lens.Lens' HttpEndpointDescription (Core.Maybe Types.Url)
hedUrl = Lens.field @"url"
{-# INLINEABLE hedUrl #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

instance Core.FromJSON HttpEndpointDescription where
        parseJSON
          = Core.withObject "HttpEndpointDescription" Core.$
              \ x ->
                HttpEndpointDescription' Core.<$>
                  (x Core..:? "Name") Core.<*> x Core..:? "Url"
