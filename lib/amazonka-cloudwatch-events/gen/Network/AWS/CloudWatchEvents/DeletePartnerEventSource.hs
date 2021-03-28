{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.DeletePartnerEventSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation is used by SaaS partners to delete a partner event source. This operation is not used by AWS customers.
--
-- When you delete an event source, the status of the corresponding partner event bus in the AWS customer account becomes DELETED.
--
module Network.AWS.CloudWatchEvents.DeletePartnerEventSource
    (
    -- * Creating a request
      DeletePartnerEventSource (..)
    , mkDeletePartnerEventSource
    -- ** Request lenses
    , dpesName
    , dpesAccount

    -- * Destructuring the response
    , DeletePartnerEventSourceResponse (..)
    , mkDeletePartnerEventSourceResponse
    ) where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeletePartnerEventSource' smart constructor.
data DeletePartnerEventSource = DeletePartnerEventSource'
  { name :: Types.Name
    -- ^ The name of the event source to delete.
  , account :: Types.Account
    -- ^ The AWS account ID of the AWS customer that the event source was created for.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePartnerEventSource' value with any optional fields omitted.
mkDeletePartnerEventSource
    :: Types.Name -- ^ 'name'
    -> Types.Account -- ^ 'account'
    -> DeletePartnerEventSource
mkDeletePartnerEventSource name account
  = DeletePartnerEventSource'{name, account}

-- | The name of the event source to delete.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpesName :: Lens.Lens' DeletePartnerEventSource Types.Name
dpesName = Lens.field @"name"
{-# INLINEABLE dpesName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The AWS account ID of the AWS customer that the event source was created for.
--
-- /Note:/ Consider using 'account' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpesAccount :: Lens.Lens' DeletePartnerEventSource Types.Account
dpesAccount = Lens.field @"account"
{-# INLINEABLE dpesAccount #-}
{-# DEPRECATED account "Use generic-lens or generic-optics with 'account' instead"  #-}

instance Core.ToQuery DeletePartnerEventSource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeletePartnerEventSource where
        toHeaders DeletePartnerEventSource{..}
          = Core.pure ("X-Amz-Target", "AWSEvents.DeletePartnerEventSource")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeletePartnerEventSource where
        toJSON DeletePartnerEventSource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("Account" Core..= account)])

instance Core.AWSRequest DeletePartnerEventSource where
        type Rs DeletePartnerEventSource = DeletePartnerEventSourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DeletePartnerEventSourceResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeletePartnerEventSourceResponse' smart constructor.
data DeletePartnerEventSourceResponse = DeletePartnerEventSourceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePartnerEventSourceResponse' value with any optional fields omitted.
mkDeletePartnerEventSourceResponse
    :: DeletePartnerEventSourceResponse
mkDeletePartnerEventSourceResponse
  = DeletePartnerEventSourceResponse'
