{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.DeleteNamedQuery
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the named query if you have access to the workgroup in which the query was saved.
--
-- For code samples using the AWS SDK for Java, see <http://docs.aws.amazon.com/athena/latest/ug/code-samples.html Examples and Code Samples> in the /Amazon Athena User Guide/ .
module Network.AWS.Athena.DeleteNamedQuery
  ( -- * Creating a request
    DeleteNamedQuery (..),
    mkDeleteNamedQuery,

    -- ** Request lenses
    dnqNamedQueryId,

    -- * Destructuring the response
    DeleteNamedQueryResponse (..),
    mkDeleteNamedQueryResponse,

    -- ** Response lenses
    dnqrrsResponseStatus,
  )
where

import qualified Network.AWS.Athena.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteNamedQuery' smart constructor.
newtype DeleteNamedQuery = DeleteNamedQuery'
  { -- | The unique ID of the query to delete.
    namedQueryId :: Types.NamedQueryId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNamedQuery' value with any optional fields omitted.
mkDeleteNamedQuery ::
  -- | 'namedQueryId'
  Types.NamedQueryId ->
  DeleteNamedQuery
mkDeleteNamedQuery namedQueryId = DeleteNamedQuery' {namedQueryId}

-- | The unique ID of the query to delete.
--
-- /Note:/ Consider using 'namedQueryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnqNamedQueryId :: Lens.Lens' DeleteNamedQuery Types.NamedQueryId
dnqNamedQueryId = Lens.field @"namedQueryId"
{-# DEPRECATED dnqNamedQueryId "Use generic-lens or generic-optics with 'namedQueryId' instead." #-}

instance Core.FromJSON DeleteNamedQuery where
  toJSON DeleteNamedQuery {..} =
    Core.object
      (Core.catMaybes [Core.Just ("NamedQueryId" Core..= namedQueryId)])

instance Core.AWSRequest DeleteNamedQuery where
  type Rs DeleteNamedQuery = DeleteNamedQueryResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonAthena.DeleteNamedQuery")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteNamedQueryResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteNamedQueryResponse' smart constructor.
newtype DeleteNamedQueryResponse = DeleteNamedQueryResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNamedQueryResponse' value with any optional fields omitted.
mkDeleteNamedQueryResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteNamedQueryResponse
mkDeleteNamedQueryResponse responseStatus =
  DeleteNamedQueryResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnqrrsResponseStatus :: Lens.Lens' DeleteNamedQueryResponse Core.Int
dnqrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dnqrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
