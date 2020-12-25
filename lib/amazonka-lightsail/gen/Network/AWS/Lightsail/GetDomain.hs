{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific domain recordset.
module Network.AWS.Lightsail.GetDomain
  ( -- * Creating a request
    GetDomain (..),
    mkGetDomain,

    -- ** Request lenses
    gdDomainName,

    -- * Destructuring the response
    GetDomainResponse (..),
    mkGetDomainResponse,

    -- ** Response lenses
    gdrgrsDomain,
    gdrgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDomain' smart constructor.
newtype GetDomain = GetDomain'
  { -- | The domain name for which your want to return information about.
    domainName :: Types.DomainName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetDomain' value with any optional fields omitted.
mkGetDomain ::
  -- | 'domainName'
  Types.DomainName ->
  GetDomain
mkGetDomain domainName = GetDomain' {domainName}

-- | The domain name for which your want to return information about.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdDomainName :: Lens.Lens' GetDomain Types.DomainName
gdDomainName = Lens.field @"domainName"
{-# DEPRECATED gdDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Core.FromJSON GetDomain where
  toJSON GetDomain {..} =
    Core.object
      (Core.catMaybes [Core.Just ("domainName" Core..= domainName)])

instance Core.AWSRequest GetDomain where
  type Rs GetDomain = GetDomainResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Lightsail_20161128.GetDomain")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDomainResponse'
            Core.<$> (x Core..:? "domain") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetDomainResponse' smart constructor.
data GetDomainResponse = GetDomainResponse'
  { -- | An array of key-value pairs containing information about your get domain request.
    domain :: Core.Maybe Types.Domain,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetDomainResponse' value with any optional fields omitted.
mkGetDomainResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetDomainResponse
mkGetDomainResponse responseStatus =
  GetDomainResponse' {domain = Core.Nothing, responseStatus}

-- | An array of key-value pairs containing information about your get domain request.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrgrsDomain :: Lens.Lens' GetDomainResponse (Core.Maybe Types.Domain)
gdrgrsDomain = Lens.field @"domain"
{-# DEPRECATED gdrgrsDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrgrsResponseStatus :: Lens.Lens' GetDomainResponse Core.Int
gdrgrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gdrgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
