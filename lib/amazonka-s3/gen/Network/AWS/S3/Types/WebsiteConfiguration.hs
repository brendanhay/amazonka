{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.WebsiteConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.WebsiteConfiguration
  ( WebsiteConfiguration (..),

    -- * Smart constructor
    mkWebsiteConfiguration,

    -- * Lenses
    wcErrorDocument,
    wcIndexDocument,
    wcRedirectAllRequestsTo,
    wcRoutingRules,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.ErrorDocument as Types
import qualified Network.AWS.S3.Types.IndexDocument as Types
import qualified Network.AWS.S3.Types.RedirectAllRequestsTo as Types
import qualified Network.AWS.S3.Types.RoutingRule as Types

-- | Specifies website configuration parameters for an Amazon S3 bucket.
--
-- /See:/ 'mkWebsiteConfiguration' smart constructor.
data WebsiteConfiguration = WebsiteConfiguration'
  { -- | The name of the error document for the website.
    errorDocument :: Core.Maybe Types.ErrorDocument,
    -- | The name of the index document for the website.
    indexDocument :: Core.Maybe Types.IndexDocument,
    -- | The redirect behavior for every request to this bucket's website endpoint.
    --
    -- /Important:/ If you specify this property, you can't specify any other property.
    redirectAllRequestsTo :: Core.Maybe Types.RedirectAllRequestsTo,
    -- | Rules that define when a redirect is applied and the redirect behavior.
    routingRules :: Core.Maybe [Types.RoutingRule]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WebsiteConfiguration' value with any optional fields omitted.
mkWebsiteConfiguration ::
  WebsiteConfiguration
mkWebsiteConfiguration =
  WebsiteConfiguration'
    { errorDocument = Core.Nothing,
      indexDocument = Core.Nothing,
      redirectAllRequestsTo = Core.Nothing,
      routingRules = Core.Nothing
    }

-- | The name of the error document for the website.
--
-- /Note:/ Consider using 'errorDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcErrorDocument :: Lens.Lens' WebsiteConfiguration (Core.Maybe Types.ErrorDocument)
wcErrorDocument = Lens.field @"errorDocument"
{-# DEPRECATED wcErrorDocument "Use generic-lens or generic-optics with 'errorDocument' instead." #-}

-- | The name of the index document for the website.
--
-- /Note:/ Consider using 'indexDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcIndexDocument :: Lens.Lens' WebsiteConfiguration (Core.Maybe Types.IndexDocument)
wcIndexDocument = Lens.field @"indexDocument"
{-# DEPRECATED wcIndexDocument "Use generic-lens or generic-optics with 'indexDocument' instead." #-}

-- | The redirect behavior for every request to this bucket's website endpoint.
--
-- /Important:/ If you specify this property, you can't specify any other property.
--
-- /Note:/ Consider using 'redirectAllRequestsTo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcRedirectAllRequestsTo :: Lens.Lens' WebsiteConfiguration (Core.Maybe Types.RedirectAllRequestsTo)
wcRedirectAllRequestsTo = Lens.field @"redirectAllRequestsTo"
{-# DEPRECATED wcRedirectAllRequestsTo "Use generic-lens or generic-optics with 'redirectAllRequestsTo' instead." #-}

-- | Rules that define when a redirect is applied and the redirect behavior.
--
-- /Note:/ Consider using 'routingRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcRoutingRules :: Lens.Lens' WebsiteConfiguration (Core.Maybe [Types.RoutingRule])
wcRoutingRules = Lens.field @"routingRules"
{-# DEPRECATED wcRoutingRules "Use generic-lens or generic-optics with 'routingRules' instead." #-}

instance Core.ToXML WebsiteConfiguration where
  toXML WebsiteConfiguration {..} =
    Core.toXMLNode "ErrorDocument" Core.<$> errorDocument
      Core.<> Core.toXMLNode "IndexDocument"
      Core.<$> indexDocument
      Core.<> Core.toXMLNode "RedirectAllRequestsTo"
      Core.<$> redirectAllRequestsTo
      Core.<> Core.toXMLNode
        "RoutingRules"
        (Core.toXMLList "RoutingRule" Core.<$> routingRules)
