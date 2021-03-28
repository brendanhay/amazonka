{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.GetSMSAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the settings for sending SMS messages from your account.
--
-- These settings are set with the @SetSMSAttributes@ action.
module Network.AWS.SNS.GetSMSAttributes
    (
    -- * Creating a request
      GetSMSAttributes (..)
    , mkGetSMSAttributes
    -- ** Request lenses
    , gsmsaAttributes

    -- * Destructuring the response
    , GetSMSAttributesResponse (..)
    , mkGetSMSAttributesResponse
    -- ** Response lenses
    , gsmsarrsAttributes
    , gsmsarrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SNS.Types as Types

-- | The input for the @GetSMSAttributes@ request.
--
-- /See:/ 'mkGetSMSAttributes' smart constructor.
newtype GetSMSAttributes = GetSMSAttributes'
  { attributes :: Core.Maybe [Core.Text]
    -- ^ A list of the individual attribute names, such as @MonthlySpendLimit@ , for which you want values.
--
-- For all attribute names, see <https://docs.aws.amazon.com/sns/latest/api/API_SetSMSAttributes.html SetSMSAttributes> .
-- If you don't use this parameter, Amazon SNS returns all SMS attributes.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetSMSAttributes' value with any optional fields omitted.
mkGetSMSAttributes
    :: GetSMSAttributes
mkGetSMSAttributes = GetSMSAttributes'{attributes = Core.Nothing}

-- | A list of the individual attribute names, such as @MonthlySpendLimit@ , for which you want values.
--
-- For all attribute names, see <https://docs.aws.amazon.com/sns/latest/api/API_SetSMSAttributes.html SetSMSAttributes> .
-- If you don't use this parameter, Amazon SNS returns all SMS attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsmsaAttributes :: Lens.Lens' GetSMSAttributes (Core.Maybe [Core.Text])
gsmsaAttributes = Lens.field @"attributes"
{-# INLINEABLE gsmsaAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

instance Core.ToQuery GetSMSAttributes where
        toQuery GetSMSAttributes{..}
          = Core.toQueryPair "Action" ("GetSMSAttributes" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-03-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "attributes"
                (Core.maybe Core.mempty (Core.toQueryList "member") attributes)

instance Core.ToHeaders GetSMSAttributes where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetSMSAttributes where
        type Rs GetSMSAttributes = GetSMSAttributesResponse
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
          = Response.receiveXMLWrapper "GetSMSAttributesResult"
              (\ s h x ->
                 GetSMSAttributesResponse' Core.<$>
                   (x Core..@? "attributes" Core..<@>
                      Core.parseXMLMap "entry" "key" "value")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The response from the @GetSMSAttributes@ request.
--
-- /See:/ 'mkGetSMSAttributesResponse' smart constructor.
data GetSMSAttributesResponse = GetSMSAttributesResponse'
  { attributes :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ The SMS attribute names and their values.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSMSAttributesResponse' value with any optional fields omitted.
mkGetSMSAttributesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetSMSAttributesResponse
mkGetSMSAttributesResponse responseStatus
  = GetSMSAttributesResponse'{attributes = Core.Nothing,
                              responseStatus}

-- | The SMS attribute names and their values.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsmsarrsAttributes :: Lens.Lens' GetSMSAttributesResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
gsmsarrsAttributes = Lens.field @"attributes"
{-# INLINEABLE gsmsarrsAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsmsarrsResponseStatus :: Lens.Lens' GetSMSAttributesResponse Core.Int
gsmsarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gsmsarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
