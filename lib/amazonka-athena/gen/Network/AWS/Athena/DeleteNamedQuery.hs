{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeleteNamedQuery (..)
    , mkDeleteNamedQuery
    -- ** Request lenses
    , dnqNamedQueryId

    -- * Destructuring the response
    , DeleteNamedQueryResponse (..)
    , mkDeleteNamedQueryResponse
    -- ** Response lenses
    , dnqrrsResponseStatus
    ) where

import qualified Network.AWS.Athena.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteNamedQuery' smart constructor.
newtype DeleteNamedQuery = DeleteNamedQuery'
  { namedQueryId :: Types.NamedQueryId
    -- ^ The unique ID of the query to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNamedQuery' value with any optional fields omitted.
mkDeleteNamedQuery
    :: Types.NamedQueryId -- ^ 'namedQueryId'
    -> DeleteNamedQuery
mkDeleteNamedQuery namedQueryId = DeleteNamedQuery'{namedQueryId}

-- | The unique ID of the query to delete.
--
-- /Note:/ Consider using 'namedQueryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnqNamedQueryId :: Lens.Lens' DeleteNamedQuery Types.NamedQueryId
dnqNamedQueryId = Lens.field @"namedQueryId"
{-# INLINEABLE dnqNamedQueryId #-}
{-# DEPRECATED namedQueryId "Use generic-lens or generic-optics with 'namedQueryId' instead"  #-}

instance Core.ToQuery DeleteNamedQuery where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteNamedQuery where
        toHeaders DeleteNamedQuery{..}
          = Core.pure ("X-Amz-Target", "AmazonAthena.DeleteNamedQuery")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteNamedQuery where
        toJSON DeleteNamedQuery{..}
          = Core.object
              (Core.catMaybes [Core.Just ("NamedQueryId" Core..= namedQueryId)])

instance Core.AWSRequest DeleteNamedQuery where
        type Rs DeleteNamedQuery = DeleteNamedQueryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteNamedQueryResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteNamedQueryResponse' smart constructor.
newtype DeleteNamedQueryResponse = DeleteNamedQueryResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNamedQueryResponse' value with any optional fields omitted.
mkDeleteNamedQueryResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteNamedQueryResponse
mkDeleteNamedQueryResponse responseStatus
  = DeleteNamedQueryResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnqrrsResponseStatus :: Lens.Lens' DeleteNamedQueryResponse Core.Int
dnqrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dnqrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
