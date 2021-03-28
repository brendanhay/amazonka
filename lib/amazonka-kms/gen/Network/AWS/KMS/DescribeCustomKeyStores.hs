{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.DescribeCustomKeyStores
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key stores> in the account and region.
--
-- This operation is part of the <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html Custom Key Store feature> feature in AWS KMS, which combines the convenience and extensive integration of AWS KMS with the isolation and control of a single-tenant key store.
-- By default, this operation returns information about all custom key stores in the account and region. To get only information about a particular custom key store, use either the @CustomKeyStoreName@ or @CustomKeyStoreId@ parameter (but not both).
-- To determine whether the custom key store is connected to its AWS CloudHSM cluster, use the @ConnectionState@ element in the response. If an attempt to connect the custom key store failed, the @ConnectionState@ value is @FAILED@ and the @ConnectionErrorCode@ element in the response indicates the cause of the failure. For help interpreting the @ConnectionErrorCode@ , see 'CustomKeyStoresListEntry' .
-- Custom key stores have a @DISCONNECTED@ connection state if the key store has never been connected or you use the 'DisconnectCustomKeyStore' operation to disconnect it. If your custom key store state is @CONNECTED@ but you are having trouble using it, make sure that its associated AWS CloudHSM cluster is active and contains the minimum number of HSMs required for the operation, if any.
-- For help repairing your custom key store, see the <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html Troubleshooting Custom Key Stores> topic in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.DescribeCustomKeyStores
    (
    -- * Creating a request
      DescribeCustomKeyStores (..)
    , mkDescribeCustomKeyStores
    -- ** Request lenses
    , dckssCustomKeyStoreId
    , dckssCustomKeyStoreName
    , dckssLimit
    , dckssMarker

    -- * Destructuring the response
    , DescribeCustomKeyStoresResponse (..)
    , mkDescribeCustomKeyStoresResponse
    -- ** Response lenses
    , drsCustomKeyStores
    , drsNextMarker
    , drsTruncated
    , drsResponseStatus
    ) where

import qualified Network.AWS.KMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeCustomKeyStores' smart constructor.
data DescribeCustomKeyStores = DescribeCustomKeyStores'
  { customKeyStoreId :: Core.Maybe Types.CustomKeyStoreIdType
    -- ^ Gets only information about the specified custom key store. Enter the key store ID.
--
-- By default, this operation gets information about all custom key stores in the account and region. To limit the output to a particular custom key store, you can use either the @CustomKeyStoreId@ or @CustomKeyStoreName@ parameter, but not both.
  , customKeyStoreName :: Core.Maybe Types.CustomKeyStoreNameType
    -- ^ Gets only information about the specified custom key store. Enter the friendly name of the custom key store.
--
-- By default, this operation gets information about all custom key stores in the account and region. To limit the output to a particular custom key store, you can use either the @CustomKeyStoreId@ or @CustomKeyStoreName@ parameter, but not both.
  , limit :: Core.Maybe Core.Natural
    -- ^ Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer.
  , marker :: Core.Maybe Types.MarkerType
    -- ^ Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCustomKeyStores' value with any optional fields omitted.
mkDescribeCustomKeyStores
    :: DescribeCustomKeyStores
mkDescribeCustomKeyStores
  = DescribeCustomKeyStores'{customKeyStoreId = Core.Nothing,
                             customKeyStoreName = Core.Nothing, limit = Core.Nothing,
                             marker = Core.Nothing}

-- | Gets only information about the specified custom key store. Enter the key store ID.
--
-- By default, this operation gets information about all custom key stores in the account and region. To limit the output to a particular custom key store, you can use either the @CustomKeyStoreId@ or @CustomKeyStoreName@ parameter, but not both.
--
-- /Note:/ Consider using 'customKeyStoreId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dckssCustomKeyStoreId :: Lens.Lens' DescribeCustomKeyStores (Core.Maybe Types.CustomKeyStoreIdType)
dckssCustomKeyStoreId = Lens.field @"customKeyStoreId"
{-# INLINEABLE dckssCustomKeyStoreId #-}
{-# DEPRECATED customKeyStoreId "Use generic-lens or generic-optics with 'customKeyStoreId' instead"  #-}

-- | Gets only information about the specified custom key store. Enter the friendly name of the custom key store.
--
-- By default, this operation gets information about all custom key stores in the account and region. To limit the output to a particular custom key store, you can use either the @CustomKeyStoreId@ or @CustomKeyStoreName@ parameter, but not both.
--
-- /Note:/ Consider using 'customKeyStoreName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dckssCustomKeyStoreName :: Lens.Lens' DescribeCustomKeyStores (Core.Maybe Types.CustomKeyStoreNameType)
dckssCustomKeyStoreName = Lens.field @"customKeyStoreName"
{-# INLINEABLE dckssCustomKeyStoreName #-}
{-# DEPRECATED customKeyStoreName "Use generic-lens or generic-optics with 'customKeyStoreName' instead"  #-}

-- | Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dckssLimit :: Lens.Lens' DescribeCustomKeyStores (Core.Maybe Core.Natural)
dckssLimit = Lens.field @"limit"
{-# INLINEABLE dckssLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dckssMarker :: Lens.Lens' DescribeCustomKeyStores (Core.Maybe Types.MarkerType)
dckssMarker = Lens.field @"marker"
{-# INLINEABLE dckssMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

instance Core.ToQuery DescribeCustomKeyStores where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeCustomKeyStores where
        toHeaders DescribeCustomKeyStores{..}
          = Core.pure
              ("X-Amz-Target", "TrentService.DescribeCustomKeyStores")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeCustomKeyStores where
        toJSON DescribeCustomKeyStores{..}
          = Core.object
              (Core.catMaybes
                 [("CustomKeyStoreId" Core..=) Core.<$> customKeyStoreId,
                  ("CustomKeyStoreName" Core..=) Core.<$> customKeyStoreName,
                  ("Limit" Core..=) Core.<$> limit,
                  ("Marker" Core..=) Core.<$> marker])

instance Core.AWSRequest DescribeCustomKeyStores where
        type Rs DescribeCustomKeyStores = DescribeCustomKeyStoresResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeCustomKeyStoresResponse' Core.<$>
                   (x Core..:? "CustomKeyStores") Core.<*> x Core..:? "NextMarker"
                     Core.<*> x Core..:? "Truncated"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeCustomKeyStoresResponse' smart constructor.
data DescribeCustomKeyStoresResponse = DescribeCustomKeyStoresResponse'
  { customKeyStores :: Core.Maybe [Types.CustomKeyStoresListEntry]
    -- ^ Contains metadata about each custom key store.
  , nextMarker :: Core.Maybe Types.MarkerType
    -- ^ When @Truncated@ is true, this element is present and contains the value to use for the @Marker@ parameter in a subsequent request.
  , truncated :: Core.Maybe Core.Bool
    -- ^ A flag that indicates whether there are more items in the list. When this value is true, the list in this response is truncated. To get more items, pass the value of the @NextMarker@ element in thisresponse to the @Marker@ parameter in a subsequent request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeCustomKeyStoresResponse' value with any optional fields omitted.
mkDescribeCustomKeyStoresResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeCustomKeyStoresResponse
mkDescribeCustomKeyStoresResponse responseStatus
  = DescribeCustomKeyStoresResponse'{customKeyStores = Core.Nothing,
                                     nextMarker = Core.Nothing, truncated = Core.Nothing,
                                     responseStatus}

-- | Contains metadata about each custom key store.
--
-- /Note:/ Consider using 'customKeyStores' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsCustomKeyStores :: Lens.Lens' DescribeCustomKeyStoresResponse (Core.Maybe [Types.CustomKeyStoresListEntry])
drsCustomKeyStores = Lens.field @"customKeyStores"
{-# INLINEABLE drsCustomKeyStores #-}
{-# DEPRECATED customKeyStores "Use generic-lens or generic-optics with 'customKeyStores' instead"  #-}

-- | When @Truncated@ is true, this element is present and contains the value to use for the @Marker@ parameter in a subsequent request.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsNextMarker :: Lens.Lens' DescribeCustomKeyStoresResponse (Core.Maybe Types.MarkerType)
drsNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE drsNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

-- | A flag that indicates whether there are more items in the list. When this value is true, the list in this response is truncated. To get more items, pass the value of the @NextMarker@ element in thisresponse to the @Marker@ parameter in a subsequent request.
--
-- /Note:/ Consider using 'truncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsTruncated :: Lens.Lens' DescribeCustomKeyStoresResponse (Core.Maybe Core.Bool)
drsTruncated = Lens.field @"truncated"
{-# INLINEABLE drsTruncated #-}
{-# DEPRECATED truncated "Use generic-lens or generic-optics with 'truncated' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeCustomKeyStoresResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
