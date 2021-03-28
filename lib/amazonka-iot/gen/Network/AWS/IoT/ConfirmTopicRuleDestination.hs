{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ConfirmTopicRuleDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Confirms a topic rule destination. When you create a rule requiring a destination, AWS IoT sends a confirmation message to the endpoint or base address you specify. The message includes a token which you pass back when calling @ConfirmTopicRuleDestination@ to confirm that you own or have access to the endpoint.
module Network.AWS.IoT.ConfirmTopicRuleDestination
    (
    -- * Creating a request
      ConfirmTopicRuleDestination (..)
    , mkConfirmTopicRuleDestination
    -- ** Request lenses
    , ctrdConfirmationToken

    -- * Destructuring the response
    , ConfirmTopicRuleDestinationResponse (..)
    , mkConfirmTopicRuleDestinationResponse
    -- ** Response lenses
    , ctrdrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkConfirmTopicRuleDestination' smart constructor.
newtype ConfirmTopicRuleDestination = ConfirmTopicRuleDestination'
  { confirmationToken :: Types.ConfirmationToken
    -- ^ The token used to confirm ownership or access to the topic rule confirmation URL.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ConfirmTopicRuleDestination' value with any optional fields omitted.
mkConfirmTopicRuleDestination
    :: Types.ConfirmationToken -- ^ 'confirmationToken'
    -> ConfirmTopicRuleDestination
mkConfirmTopicRuleDestination confirmationToken
  = ConfirmTopicRuleDestination'{confirmationToken}

-- | The token used to confirm ownership or access to the topic rule confirmation URL.
--
-- /Note:/ Consider using 'confirmationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrdConfirmationToken :: Lens.Lens' ConfirmTopicRuleDestination Types.ConfirmationToken
ctrdConfirmationToken = Lens.field @"confirmationToken"
{-# INLINEABLE ctrdConfirmationToken #-}
{-# DEPRECATED confirmationToken "Use generic-lens or generic-optics with 'confirmationToken' instead"  #-}

instance Core.ToQuery ConfirmTopicRuleDestination where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ConfirmTopicRuleDestination where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ConfirmTopicRuleDestination where
        type Rs ConfirmTopicRuleDestination =
             ConfirmTopicRuleDestinationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/confirmdestination/" Core.<> Core.toText confirmationToken,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 ConfirmTopicRuleDestinationResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkConfirmTopicRuleDestinationResponse' smart constructor.
newtype ConfirmTopicRuleDestinationResponse = ConfirmTopicRuleDestinationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ConfirmTopicRuleDestinationResponse' value with any optional fields omitted.
mkConfirmTopicRuleDestinationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ConfirmTopicRuleDestinationResponse
mkConfirmTopicRuleDestinationResponse responseStatus
  = ConfirmTopicRuleDestinationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrdrrsResponseStatus :: Lens.Lens' ConfirmTopicRuleDestinationResponse Core.Int
ctrdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ctrdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
