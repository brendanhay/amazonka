{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeAccountAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of attributes attached to an account
module Network.AWS.Redshift.DescribeAccountAttributes
    (
    -- * Creating a request
      DescribeAccountAttributes (..)
    , mkDescribeAccountAttributes
    -- ** Request lenses
    , daaAttributeNames

    -- * Destructuring the response
    , DescribeAccountAttributesResponse (..)
    , mkDescribeAccountAttributesResponse
    -- ** Response lenses
    , daarrsAccountAttributes
    , daarrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeAccountAttributes' smart constructor.
newtype DescribeAccountAttributes = DescribeAccountAttributes'
  { attributeNames :: Core.Maybe [Core.Text]
    -- ^ A list of attribute names.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAccountAttributes' value with any optional fields omitted.
mkDescribeAccountAttributes
    :: DescribeAccountAttributes
mkDescribeAccountAttributes
  = DescribeAccountAttributes'{attributeNames = Core.Nothing}

-- | A list of attribute names.
--
-- /Note:/ Consider using 'attributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daaAttributeNames :: Lens.Lens' DescribeAccountAttributes (Core.Maybe [Core.Text])
daaAttributeNames = Lens.field @"attributeNames"
{-# INLINEABLE daaAttributeNames #-}
{-# DEPRECATED attributeNames "Use generic-lens or generic-optics with 'attributeNames' instead"  #-}

instance Core.ToQuery DescribeAccountAttributes where
        toQuery DescribeAccountAttributes{..}
          = Core.toQueryPair "Action"
              ("DescribeAccountAttributes" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "AttributeNames"
                (Core.maybe Core.mempty (Core.toQueryList "AttributeName")
                   attributeNames)

instance Core.ToHeaders DescribeAccountAttributes where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeAccountAttributes where
        type Rs DescribeAccountAttributes =
             DescribeAccountAttributesResponse
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
          = Response.receiveXMLWrapper "DescribeAccountAttributesResult"
              (\ s h x ->
                 DescribeAccountAttributesResponse' Core.<$>
                   (x Core..@? "AccountAttributes" Core..<@>
                      Core.parseXMLList "AccountAttribute")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeAccountAttributesResponse' smart constructor.
data DescribeAccountAttributesResponse = DescribeAccountAttributesResponse'
  { accountAttributes :: Core.Maybe [Types.AccountAttribute]
    -- ^ A list of attributes assigned to an account.
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
  = DescribeAccountAttributesResponse'{accountAttributes =
                                         Core.Nothing,
                                       responseStatus}

-- | A list of attributes assigned to an account.
--
-- /Note:/ Consider using 'accountAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarrsAccountAttributes :: Lens.Lens' DescribeAccountAttributesResponse (Core.Maybe [Types.AccountAttribute])
daarrsAccountAttributes = Lens.field @"accountAttributes"
{-# INLINEABLE daarrsAccountAttributes #-}
{-# DEPRECATED accountAttributes "Use generic-lens or generic-optics with 'accountAttributes' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarrsResponseStatus :: Lens.Lens' DescribeAccountAttributesResponse Core.Int
daarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE daarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
