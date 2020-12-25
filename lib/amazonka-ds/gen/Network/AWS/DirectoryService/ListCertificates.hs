{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.ListCertificates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For the specified directory, lists all the certificates registered for a secured LDAP connection.
module Network.AWS.DirectoryService.ListCertificates
  ( -- * Creating a request
    ListCertificates (..),
    mkListCertificates,

    -- ** Request lenses
    lcDirectoryId,
    lcLimit,
    lcNextToken,

    -- * Destructuring the response
    ListCertificatesResponse (..),
    mkListCertificatesResponse,

    -- ** Response lenses
    lcrrsCertificatesInfo,
    lcrrsNextToken,
    lcrrsResponseStatus,
  )
where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListCertificates' smart constructor.
data ListCertificates = ListCertificates'
  { -- | The identifier of the directory.
    directoryId :: Types.DirectoryId,
    -- | The number of items that should show up on one page
    limit :: Core.Maybe Core.Natural,
    -- | A token for requesting another page of certificates if the @NextToken@ response element indicates that more certificates are available. Use the value of the returned @NextToken@ element in your request until the token comes back as @null@ . Pass @null@ if this is the first call.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListCertificates' value with any optional fields omitted.
mkListCertificates ::
  -- | 'directoryId'
  Types.DirectoryId ->
  ListCertificates
mkListCertificates directoryId =
  ListCertificates'
    { directoryId,
      limit = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The identifier of the directory.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcDirectoryId :: Lens.Lens' ListCertificates Types.DirectoryId
lcDirectoryId = Lens.field @"directoryId"
{-# DEPRECATED lcDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The number of items that should show up on one page
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcLimit :: Lens.Lens' ListCertificates (Core.Maybe Core.Natural)
lcLimit = Lens.field @"limit"
{-# DEPRECATED lcLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | A token for requesting another page of certificates if the @NextToken@ response element indicates that more certificates are available. Use the value of the returned @NextToken@ element in your request until the token comes back as @null@ . Pass @null@ if this is the first call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcNextToken :: Lens.Lens' ListCertificates (Core.Maybe Types.NextToken)
lcNextToken = Lens.field @"nextToken"
{-# DEPRECATED lcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListCertificates where
  toJSON ListCertificates {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DirectoryId" Core..= directoryId),
            ("Limit" Core..=) Core.<$> limit,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListCertificates where
  type Rs ListCertificates = ListCertificatesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DirectoryService_20150416.ListCertificates")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCertificatesResponse'
            Core.<$> (x Core..:? "CertificatesInfo")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListCertificatesResponse' smart constructor.
data ListCertificatesResponse = ListCertificatesResponse'
  { -- | A list of certificates with basic details including certificate ID, certificate common name, certificate state.
    certificatesInfo :: Core.Maybe [Types.CertificateInfo],
    -- | Indicates whether another page of certificates is available when the number of available certificates exceeds the page limit.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListCertificatesResponse' value with any optional fields omitted.
mkListCertificatesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListCertificatesResponse
mkListCertificatesResponse responseStatus =
  ListCertificatesResponse'
    { certificatesInfo = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of certificates with basic details including certificate ID, certificate common name, certificate state.
--
-- /Note:/ Consider using 'certificatesInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsCertificatesInfo :: Lens.Lens' ListCertificatesResponse (Core.Maybe [Types.CertificateInfo])
lcrrsCertificatesInfo = Lens.field @"certificatesInfo"
{-# DEPRECATED lcrrsCertificatesInfo "Use generic-lens or generic-optics with 'certificatesInfo' instead." #-}

-- | Indicates whether another page of certificates is available when the number of available certificates exceeds the page limit.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsNextToken :: Lens.Lens' ListCertificatesResponse (Core.Maybe Types.NextToken)
lcrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lcrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsResponseStatus :: Lens.Lens' ListCertificatesResponse Core.Int
lcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
