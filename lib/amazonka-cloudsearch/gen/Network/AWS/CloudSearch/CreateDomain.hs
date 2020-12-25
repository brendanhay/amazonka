{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.CreateDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new search domain. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/creating-domains.html Creating a Search Domain> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.CreateDomain
  ( -- * Creating a request
    CreateDomain (..),
    mkCreateDomain,

    -- ** Request lenses
    cdDomainName,

    -- * Destructuring the response
    CreateDomainResponse (..),
    mkCreateDomainResponse,

    -- ** Response lenses
    cdrrsDomainStatus,
    cdrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'CreateDomain' @ operation. Specifies a name for the new search domain.
--
-- /See:/ 'mkCreateDomain' smart constructor.
newtype CreateDomain = CreateDomain'
  { -- | A name for the domain you are creating. Allowed characters are a-z (lower-case letters), 0-9, and hyphen (-). Domain names must start with a letter or number and be at least 3 and no more than 28 characters long.
    domainName :: Types.DomainName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDomain' value with any optional fields omitted.
mkCreateDomain ::
  -- | 'domainName'
  Types.DomainName ->
  CreateDomain
mkCreateDomain domainName = CreateDomain' {domainName}

-- | A name for the domain you are creating. Allowed characters are a-z (lower-case letters), 0-9, and hyphen (-). Domain names must start with a letter or number and be at least 3 and no more than 28 characters long.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDomainName :: Lens.Lens' CreateDomain Types.DomainName
cdDomainName = Lens.field @"domainName"
{-# DEPRECATED cdDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Core.AWSRequest CreateDomain where
  type Rs CreateDomain = CreateDomainResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "CreateDomain")
                Core.<> (Core.pure ("Version", "2013-01-01"))
                Core.<> (Core.toQueryValue "DomainName" domainName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreateDomainResult"
      ( \s h x ->
          CreateDomainResponse'
            Core.<$> (x Core..@? "DomainStatus") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The result of a @CreateDomainRequest@ . Contains the status of a newly created domain.
--
-- /See:/ 'mkCreateDomainResponse' smart constructor.
data CreateDomainResponse = CreateDomainResponse'
  { domainStatus :: Core.Maybe Types.DomainStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDomainResponse' value with any optional fields omitted.
mkCreateDomainResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateDomainResponse
mkCreateDomainResponse responseStatus =
  CreateDomainResponse'
    { domainStatus = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsDomainStatus :: Lens.Lens' CreateDomainResponse (Core.Maybe Types.DomainStatus)
cdrrsDomainStatus = Lens.field @"domainStatus"
{-# DEPRECATED cdrrsDomainStatus "Use generic-lens or generic-optics with 'domainStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsResponseStatus :: Lens.Lens' CreateDomainResponse Core.Int
cdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
