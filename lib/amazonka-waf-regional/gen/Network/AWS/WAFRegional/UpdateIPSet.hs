{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.UpdateIPSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts or deletes 'IPSetDescriptor' objects in an @IPSet@ . For each @IPSetDescriptor@ object, you specify the following values: 
--
--
--     * Whether to insert or delete the object from the array. If you want to change an @IPSetDescriptor@ object, you delete the existing object and add a new one.
--
--
--     * The IP address version, @IPv4@ or @IPv6@ . 
--
--
--     * The IP address in CIDR notation, for example, @192.0.2.0/24@ (for the range of IP addresses from @192.0.2.0@ to @192.0.2.255@ ) or @192.0.2.44/32@ (for the individual IP address @192.0.2.44@ ). 
--
--
-- AWS WAF supports IPv4 address ranges: /8 and any range between /16 through /32. AWS WAF supports IPv6 address ranges: /24, /32, /48, /56, /64, and /128. For more information about CIDR notation, see the Wikipedia entry <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Classless Inter-Domain Routing> .
-- IPv6 addresses can be represented using any of the following formats:
--
--     * 1111:0000:0000:0000:0000:0000:0000:0111/128
--
--
--     * 1111:0:0:0:0:0:0:0111/128
--
--
--     * 1111::0111/128
--
--
--     * 1111::111/128
--
--
-- You use an @IPSet@ to specify which web requests you want to allow or block based on the IP addresses that the requests originated from. For example, if you're receiving a lot of requests from one or a small number of IP addresses and you want to block the requests, you can create an @IPSet@ that specifies those IP addresses, and then configure AWS WAF to block the requests. 
-- To create and configure an @IPSet@ , perform the following steps:
--
--     * Submit a 'CreateIPSet' request.
--
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of an 'UpdateIPSet' request.
--
--
--     * Submit an @UpdateIPSet@ request to specify the IP addresses that you want AWS WAF to watch for.
--
--
-- When you update an @IPSet@ , you specify the IP addresses that you want to add and/or the IP addresses that you want to delete. If you want to change an IP address, you delete the existing IP address and add the new one.
-- You can insert a maximum of 1000 addresses in a single request.
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAFRegional.UpdateIPSet
    (
    -- * Creating a request
      UpdateIPSet (..)
    , mkUpdateIPSet
    -- ** Request lenses
    , uipsIPSetId
    , uipsChangeToken
    , uipsUpdates

    -- * Destructuring the response
    , UpdateIPSetResponse (..)
    , mkUpdateIPSetResponse
    -- ** Response lenses
    , uipsrrsChangeToken
    , uipsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkUpdateIPSet' smart constructor.
data UpdateIPSet = UpdateIPSet'
  { iPSetId :: Types.ResourceId
    -- ^ The @IPSetId@ of the 'IPSet' that you want to update. @IPSetId@ is returned by 'CreateIPSet' and by 'ListIPSets' .
  , changeToken :: Types.ChangeToken
    -- ^ The value returned by the most recent call to 'GetChangeToken' .
  , updates :: Core.NonEmpty Types.IPSetUpdate
    -- ^ An array of @IPSetUpdate@ objects that you want to insert into or delete from an 'IPSet' . For more information, see the applicable data types:
--
--
--     * 'IPSetUpdate' : Contains @Action@ and @IPSetDescriptor@ 
--
--
--     * 'IPSetDescriptor' : Contains @Type@ and @Value@ 
--
--
-- You can insert a maximum of 1000 addresses in a single request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateIPSet' value with any optional fields omitted.
mkUpdateIPSet
    :: Types.ResourceId -- ^ 'iPSetId'
    -> Types.ChangeToken -- ^ 'changeToken'
    -> Core.NonEmpty Types.IPSetUpdate -- ^ 'updates'
    -> UpdateIPSet
mkUpdateIPSet iPSetId changeToken updates
  = UpdateIPSet'{iPSetId, changeToken, updates}

-- | The @IPSetId@ of the 'IPSet' that you want to update. @IPSetId@ is returned by 'CreateIPSet' and by 'ListIPSets' .
--
-- /Note:/ Consider using 'iPSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipsIPSetId :: Lens.Lens' UpdateIPSet Types.ResourceId
uipsIPSetId = Lens.field @"iPSetId"
{-# INLINEABLE uipsIPSetId #-}
{-# DEPRECATED iPSetId "Use generic-lens or generic-optics with 'iPSetId' instead"  #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipsChangeToken :: Lens.Lens' UpdateIPSet Types.ChangeToken
uipsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE uipsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

-- | An array of @IPSetUpdate@ objects that you want to insert into or delete from an 'IPSet' . For more information, see the applicable data types:
--
--
--     * 'IPSetUpdate' : Contains @Action@ and @IPSetDescriptor@ 
--
--
--     * 'IPSetDescriptor' : Contains @Type@ and @Value@ 
--
--
-- You can insert a maximum of 1000 addresses in a single request.
--
-- /Note:/ Consider using 'updates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipsUpdates :: Lens.Lens' UpdateIPSet (Core.NonEmpty Types.IPSetUpdate)
uipsUpdates = Lens.field @"updates"
{-# INLINEABLE uipsUpdates #-}
{-# DEPRECATED updates "Use generic-lens or generic-optics with 'updates' instead"  #-}

instance Core.ToQuery UpdateIPSet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateIPSet where
        toHeaders UpdateIPSet{..}
          = Core.pure
              ("X-Amz-Target", "AWSWAF_Regional_20161128.UpdateIPSet")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateIPSet where
        toJSON UpdateIPSet{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("IPSetId" Core..= iPSetId),
                  Core.Just ("ChangeToken" Core..= changeToken),
                  Core.Just ("Updates" Core..= updates)])

instance Core.AWSRequest UpdateIPSet where
        type Rs UpdateIPSet = UpdateIPSetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateIPSetResponse' Core.<$>
                   (x Core..:? "ChangeToken") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateIPSetResponse' smart constructor.
data UpdateIPSetResponse = UpdateIPSetResponse'
  { changeToken :: Core.Maybe Types.ChangeToken
    -- ^ The @ChangeToken@ that you used to submit the @UpdateIPSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateIPSetResponse' value with any optional fields omitted.
mkUpdateIPSetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateIPSetResponse
mkUpdateIPSetResponse responseStatus
  = UpdateIPSetResponse'{changeToken = Core.Nothing, responseStatus}

-- | The @ChangeToken@ that you used to submit the @UpdateIPSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipsrrsChangeToken :: Lens.Lens' UpdateIPSetResponse (Core.Maybe Types.ChangeToken)
uipsrrsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE uipsrrsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipsrrsResponseStatus :: Lens.Lens' UpdateIPSetResponse Core.Int
uipsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uipsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
