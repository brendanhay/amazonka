{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeValidDBInstanceModifications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You can call @DescribeValidDBInstanceModifications@ to learn what modifications you can make to your DB instance. You can use this information when you call @ModifyDBInstance@ . 
module Network.AWS.RDS.DescribeValidDBInstanceModifications
    (
    -- * Creating a request
      DescribeValidDBInstanceModifications (..)
    , mkDescribeValidDBInstanceModifications
    -- ** Request lenses
    , dvdbimDBInstanceIdentifier

    -- * Destructuring the response
    , DescribeValidDBInstanceModificationsResponse (..)
    , mkDescribeValidDBInstanceModificationsResponse
    -- ** Response lenses
    , dvdbimrrsValidDBInstanceModificationsMessage
    , dvdbimrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDescribeValidDBInstanceModifications' smart constructor.
newtype DescribeValidDBInstanceModifications = DescribeValidDBInstanceModifications'
  { dBInstanceIdentifier :: Core.Text
    -- ^ The customer identifier or the ARN of your DB instance. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeValidDBInstanceModifications' value with any optional fields omitted.
mkDescribeValidDBInstanceModifications
    :: Core.Text -- ^ 'dBInstanceIdentifier'
    -> DescribeValidDBInstanceModifications
mkDescribeValidDBInstanceModifications dBInstanceIdentifier
  = DescribeValidDBInstanceModifications'{dBInstanceIdentifier}

-- | The customer identifier or the ARN of your DB instance. 
--
-- /Note:/ Consider using 'dBInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvdbimDBInstanceIdentifier :: Lens.Lens' DescribeValidDBInstanceModifications Core.Text
dvdbimDBInstanceIdentifier = Lens.field @"dBInstanceIdentifier"
{-# INLINEABLE dvdbimDBInstanceIdentifier #-}
{-# DEPRECATED dBInstanceIdentifier "Use generic-lens or generic-optics with 'dBInstanceIdentifier' instead"  #-}

instance Core.ToQuery DescribeValidDBInstanceModifications where
        toQuery DescribeValidDBInstanceModifications{..}
          = Core.toQueryPair "Action"
              ("DescribeValidDBInstanceModifications" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "DBInstanceIdentifier" dBInstanceIdentifier

instance Core.ToHeaders DescribeValidDBInstanceModifications where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeValidDBInstanceModifications where
        type Rs DescribeValidDBInstanceModifications =
             DescribeValidDBInstanceModificationsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper
              "DescribeValidDBInstanceModificationsResult"
              (\ s h x ->
                 DescribeValidDBInstanceModificationsResponse' Core.<$>
                   (x Core..@? "ValidDBInstanceModificationsMessage") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeValidDBInstanceModificationsResponse' smart constructor.
data DescribeValidDBInstanceModificationsResponse = DescribeValidDBInstanceModificationsResponse'
  { validDBInstanceModificationsMessage :: Core.Maybe Types.ValidDBInstanceModificationsMessage
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeValidDBInstanceModificationsResponse' value with any optional fields omitted.
mkDescribeValidDBInstanceModificationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeValidDBInstanceModificationsResponse
mkDescribeValidDBInstanceModificationsResponse responseStatus
  = DescribeValidDBInstanceModificationsResponse'{validDBInstanceModificationsMessage
                                                    = Core.Nothing,
                                                  responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'validDBInstanceModificationsMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvdbimrrsValidDBInstanceModificationsMessage :: Lens.Lens' DescribeValidDBInstanceModificationsResponse (Core.Maybe Types.ValidDBInstanceModificationsMessage)
dvdbimrrsValidDBInstanceModificationsMessage = Lens.field @"validDBInstanceModificationsMessage"
{-# INLINEABLE dvdbimrrsValidDBInstanceModificationsMessage #-}
{-# DEPRECATED validDBInstanceModificationsMessage "Use generic-lens or generic-optics with 'validDBInstanceModificationsMessage' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvdbimrrsResponseStatus :: Lens.Lens' DescribeValidDBInstanceModificationsResponse Core.Int
dvdbimrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dvdbimrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
