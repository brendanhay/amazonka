{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateManagedPrefixList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a managed prefix list. You can specify one or more entries for the prefix list. Each entry consists of a CIDR block and an optional description.
--
-- You must specify the maximum number of entries for the prefix list. The maximum number of entries cannot be changed later.
module Network.AWS.EC2.CreateManagedPrefixList
    (
    -- * Creating a request
      CreateManagedPrefixList (..)
    , mkCreateManagedPrefixList
    -- ** Request lenses
    , cmplPrefixListName
    , cmplMaxEntries
    , cmplAddressFamily
    , cmplClientToken
    , cmplDryRun
    , cmplEntries
    , cmplTagSpecifications

    -- * Destructuring the response
    , CreateManagedPrefixListResponse (..)
    , mkCreateManagedPrefixListResponse
    -- ** Response lenses
    , cmplrrsPrefixList
    , cmplrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateManagedPrefixList' smart constructor.
data CreateManagedPrefixList = CreateManagedPrefixList'
  { prefixListName :: Core.Text
    -- ^ A name for the prefix list.
--
-- Constraints: Up to 255 characters in length. The name cannot start with @com.amazonaws@ .
  , maxEntries :: Core.Int
    -- ^ The maximum number of entries for the prefix list.
  , addressFamily :: Core.Text
    -- ^ The IP address type.
--
-- Valid Values: @IPv4@ | @IPv6@ 
  , clientToken :: Core.Maybe Core.Text
    -- ^ Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- Constraints: Up to 255 UTF-8 characters in length.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , entries :: Core.Maybe [Types.AddPrefixListEntry]
    -- ^ One or more entries for the prefix list.
  , tagSpecifications :: Core.Maybe [Types.TagSpecification]
    -- ^ The tags to apply to the prefix list during creation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateManagedPrefixList' value with any optional fields omitted.
mkCreateManagedPrefixList
    :: Core.Text -- ^ 'prefixListName'
    -> Core.Int -- ^ 'maxEntries'
    -> Core.Text -- ^ 'addressFamily'
    -> CreateManagedPrefixList
mkCreateManagedPrefixList prefixListName maxEntries addressFamily
  = CreateManagedPrefixList'{prefixListName, maxEntries,
                             addressFamily, clientToken = Core.Nothing, dryRun = Core.Nothing,
                             entries = Core.Nothing, tagSpecifications = Core.Nothing}

-- | A name for the prefix list.
--
-- Constraints: Up to 255 characters in length. The name cannot start with @com.amazonaws@ .
--
-- /Note:/ Consider using 'prefixListName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmplPrefixListName :: Lens.Lens' CreateManagedPrefixList Core.Text
cmplPrefixListName = Lens.field @"prefixListName"
{-# INLINEABLE cmplPrefixListName #-}
{-# DEPRECATED prefixListName "Use generic-lens or generic-optics with 'prefixListName' instead"  #-}

-- | The maximum number of entries for the prefix list.
--
-- /Note:/ Consider using 'maxEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmplMaxEntries :: Lens.Lens' CreateManagedPrefixList Core.Int
cmplMaxEntries = Lens.field @"maxEntries"
{-# INLINEABLE cmplMaxEntries #-}
{-# DEPRECATED maxEntries "Use generic-lens or generic-optics with 'maxEntries' instead"  #-}

-- | The IP address type.
--
-- Valid Values: @IPv4@ | @IPv6@ 
--
-- /Note:/ Consider using 'addressFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmplAddressFamily :: Lens.Lens' CreateManagedPrefixList Core.Text
cmplAddressFamily = Lens.field @"addressFamily"
{-# INLINEABLE cmplAddressFamily #-}
{-# DEPRECATED addressFamily "Use generic-lens or generic-optics with 'addressFamily' instead"  #-}

-- | Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- Constraints: Up to 255 UTF-8 characters in length.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmplClientToken :: Lens.Lens' CreateManagedPrefixList (Core.Maybe Core.Text)
cmplClientToken = Lens.field @"clientToken"
{-# INLINEABLE cmplClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmplDryRun :: Lens.Lens' CreateManagedPrefixList (Core.Maybe Core.Bool)
cmplDryRun = Lens.field @"dryRun"
{-# INLINEABLE cmplDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | One or more entries for the prefix list.
--
-- /Note:/ Consider using 'entries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmplEntries :: Lens.Lens' CreateManagedPrefixList (Core.Maybe [Types.AddPrefixListEntry])
cmplEntries = Lens.field @"entries"
{-# INLINEABLE cmplEntries #-}
{-# DEPRECATED entries "Use generic-lens or generic-optics with 'entries' instead"  #-}

-- | The tags to apply to the prefix list during creation.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmplTagSpecifications :: Lens.Lens' CreateManagedPrefixList (Core.Maybe [Types.TagSpecification])
cmplTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE cmplTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

instance Core.ToQuery CreateManagedPrefixList where
        toQuery CreateManagedPrefixList{..}
          = Core.toQueryPair "Action"
              ("CreateManagedPrefixList" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "PrefixListName" prefixListName
              Core.<> Core.toQueryPair "MaxEntries" maxEntries
              Core.<> Core.toQueryPair "AddressFamily" addressFamily
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClientToken") clientToken
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Entry") entries
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TagSpecification")
                tagSpecifications

instance Core.ToHeaders CreateManagedPrefixList where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateManagedPrefixList where
        type Rs CreateManagedPrefixList = CreateManagedPrefixListResponse
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
          = Response.receiveXML
              (\ s h x ->
                 CreateManagedPrefixListResponse' Core.<$>
                   (x Core..@? "prefixList") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateManagedPrefixListResponse' smart constructor.
data CreateManagedPrefixListResponse = CreateManagedPrefixListResponse'
  { prefixList :: Core.Maybe Types.ManagedPrefixList
    -- ^ Information about the prefix list.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateManagedPrefixListResponse' value with any optional fields omitted.
mkCreateManagedPrefixListResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateManagedPrefixListResponse
mkCreateManagedPrefixListResponse responseStatus
  = CreateManagedPrefixListResponse'{prefixList = Core.Nothing,
                                     responseStatus}

-- | Information about the prefix list.
--
-- /Note:/ Consider using 'prefixList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmplrrsPrefixList :: Lens.Lens' CreateManagedPrefixListResponse (Core.Maybe Types.ManagedPrefixList)
cmplrrsPrefixList = Lens.field @"prefixList"
{-# INLINEABLE cmplrrsPrefixList #-}
{-# DEPRECATED prefixList "Use generic-lens or generic-optics with 'prefixList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmplrrsResponseStatus :: Lens.Lens' CreateManagedPrefixListResponse Core.Int
cmplrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cmplrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
