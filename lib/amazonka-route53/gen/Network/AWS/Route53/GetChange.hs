{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetChange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current status of a change batch request. The status is one of the following values:
--
--
--     * @PENDING@ indicates that the changes in this request have not propagated to all Amazon Route 53 DNS servers. This is the initial status of all change batch requests.
--
--
--     * @INSYNC@ indicates that the changes have propagated to all Route 53 DNS servers.
module Network.AWS.Route53.GetChange
  ( -- * Creating a request
    GetChange (..),
    mkGetChange,

    -- ** Request lenses
    gcId,

    -- * Destructuring the response
    GetChangeResponse (..),
    mkGetChangeResponse,

    -- ** Response lenses
    gcrrsChangeInfo,
    gcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | The input for a GetChange request.
--
-- /See:/ 'mkGetChange' smart constructor.
newtype GetChange = GetChange'
  { -- | The ID of the change batch request. The value that you specify here is the value that @ChangeResourceRecordSets@ returned in the @Id@ element when you submitted the request.
    id :: Types.ResourceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetChange' value with any optional fields omitted.
mkGetChange ::
  -- | 'id'
  Types.ResourceId ->
  GetChange
mkGetChange id = GetChange' {id}

-- | The ID of the change batch request. The value that you specify here is the value that @ChangeResourceRecordSets@ returned in the @Id@ element when you submitted the request.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcId :: Lens.Lens' GetChange Types.ResourceId
gcId = Lens.field @"id"
{-# DEPRECATED gcId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.AWSRequest GetChange where
  type Rs GetChange = GetChangeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath ("/2013-04-01/change/" Core.<> (Core.toText id)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetChangeResponse'
            Core.<$> (x Core..@ "ChangeInfo") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | A complex type that contains the @ChangeInfo@ element.
--
-- /See:/ 'mkGetChangeResponse' smart constructor.
data GetChangeResponse = GetChangeResponse'
  { -- | A complex type that contains information about the specified change batch.
    changeInfo :: Types.ChangeInfo,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetChangeResponse' value with any optional fields omitted.
mkGetChangeResponse ::
  -- | 'changeInfo'
  Types.ChangeInfo ->
  -- | 'responseStatus'
  Core.Int ->
  GetChangeResponse
mkGetChangeResponse changeInfo responseStatus =
  GetChangeResponse' {changeInfo, responseStatus}

-- | A complex type that contains information about the specified change batch.
--
-- /Note:/ Consider using 'changeInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsChangeInfo :: Lens.Lens' GetChangeResponse Types.ChangeInfo
gcrrsChangeInfo = Lens.field @"changeInfo"
{-# DEPRECATED gcrrsChangeInfo "Use generic-lens or generic-optics with 'changeInfo' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsResponseStatus :: Lens.Lens' GetChangeResponse Core.Int
gcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
