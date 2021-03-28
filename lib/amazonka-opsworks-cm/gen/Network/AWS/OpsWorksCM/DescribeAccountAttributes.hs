{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.DescribeAccountAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes your OpsWorks-CM account attributes. 
--
-- This operation is synchronous. 
module Network.AWS.OpsWorksCM.DescribeAccountAttributes
    (
    -- * Creating a request
      DescribeAccountAttributes (..)
    , mkDescribeAccountAttributes

    -- * Destructuring the response
    , DescribeAccountAttributesResponse (..)
    , mkDescribeAccountAttributesResponse
    -- ** Response lenses
    , daarrsAttributes
    , daarrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorksCM.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeAccountAttributes' smart constructor.
data DescribeAccountAttributes = DescribeAccountAttributes'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAccountAttributes' value with any optional fields omitted.
mkDescribeAccountAttributes
    :: DescribeAccountAttributes
mkDescribeAccountAttributes = DescribeAccountAttributes'

instance Core.ToQuery DescribeAccountAttributes where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeAccountAttributes where
        toHeaders DescribeAccountAttributes{..}
          = Core.pure
              ("X-Amz-Target",
               "OpsWorksCM_V2016_11_01.DescribeAccountAttributes")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeAccountAttributes where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest DescribeAccountAttributes where
        type Rs DescribeAccountAttributes =
             DescribeAccountAttributesResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeAccountAttributesResponse' Core.<$>
                   (x Core..:? "Attributes") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeAccountAttributesResponse' smart constructor.
data DescribeAccountAttributesResponse = DescribeAccountAttributesResponse'
  { attributes :: Core.Maybe [Types.AccountAttribute]
    -- ^ The attributes that are currently set for the account. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAccountAttributesResponse' value with any optional fields omitted.
mkDescribeAccountAttributesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeAccountAttributesResponse
mkDescribeAccountAttributesResponse responseStatus
  = DescribeAccountAttributesResponse'{attributes = Core.Nothing,
                                       responseStatus}

-- | The attributes that are currently set for the account. 
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarrsAttributes :: Lens.Lens' DescribeAccountAttributesResponse (Core.Maybe [Types.AccountAttribute])
daarrsAttributes = Lens.field @"attributes"
{-# INLINEABLE daarrsAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarrsResponseStatus :: Lens.Lens' DescribeAccountAttributesResponse Core.Int
daarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE daarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
