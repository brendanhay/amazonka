{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.ListVaults
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation lists all vaults owned by the calling user's account. The list returned in the response is ASCII-sorted by vault name.
--
-- By default, this operation returns up to 10 items. If there are more vaults to list, the response @marker@ field contains the vault Amazon Resource Name (ARN) at which to continue the list with a new List Vaults request; otherwise, the @marker@ field is @null@ . To return a list of vaults that begins at a specific vault, set the @marker@ request parameter to the vault ARN you obtained from a previous List Vaults request. You can also limit the number of vaults returned in the response by specifying the @limit@ parameter in the request. 
-- An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)> .
-- For conceptual information and underlying REST API, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/retrieving-vault-info.html Retrieving Vault Metadata in Amazon S3 Glacier> and <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-vaults-get.html List Vaults > in the /Amazon Glacier Developer Guide/ . 
--
-- This operation returns paginated results.
module Network.AWS.Glacier.ListVaults
    (
    -- * Creating a request
      ListVaults (..)
    , mkListVaults
    -- ** Request lenses
    , lvAccountId
    , lvLimit
    , lvMarker

    -- * Destructuring the response
    , ListVaultsResponse (..)
    , mkListVaultsResponse
    -- ** Response lenses
    , lvrrsMarker
    , lvrrsVaultList
    , lvrrsResponseStatus
    ) where

import qualified Network.AWS.Glacier.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Provides options to retrieve the vault list owned by the calling user's account. The list provides metadata information for each vault.
--
-- /See:/ 'mkListVaults' smart constructor.
data ListVaults = ListVaults'
  { accountId :: Core.Text
    -- ^ The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
  , limit :: Core.Maybe Core.Text
    -- ^ The maximum number of vaults to be returned. The default limit is 10. The number of vaults returned might be fewer than the specified limit, but the number of returned vaults never exceeds the limit.
  , marker :: Core.Maybe Core.Text
    -- ^ A string used for pagination. The marker specifies the vault ARN after which the listing of vaults should begin.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListVaults' value with any optional fields omitted.
mkListVaults
    :: Core.Text -- ^ 'accountId'
    -> ListVaults
mkListVaults accountId
  = ListVaults'{accountId, limit = Core.Nothing,
                marker = Core.Nothing}

-- | The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvAccountId :: Lens.Lens' ListVaults Core.Text
lvAccountId = Lens.field @"accountId"
{-# INLINEABLE lvAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | The maximum number of vaults to be returned. The default limit is 10. The number of vaults returned might be fewer than the specified limit, but the number of returned vaults never exceeds the limit.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvLimit :: Lens.Lens' ListVaults (Core.Maybe Core.Text)
lvLimit = Lens.field @"limit"
{-# INLINEABLE lvLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | A string used for pagination. The marker specifies the vault ARN after which the listing of vaults should begin.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvMarker :: Lens.Lens' ListVaults (Core.Maybe Core.Text)
lvMarker = Lens.field @"marker"
{-# INLINEABLE lvMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

instance Core.ToQuery ListVaults where
        toQuery ListVaults{..}
          = Core.maybe Core.mempty (Core.toQueryPair "limit") limit Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "marker") marker

instance Core.ToHeaders ListVaults where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListVaults where
        type Rs ListVaults = ListVaultsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/" Core.<> Core.toText accountId Core.<> "/vaults",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListVaultsResponse' Core.<$>
                   (x Core..:? "Marker") Core.<*> x Core..:? "VaultList" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListVaults where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"vaultList" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | Contains the Amazon S3 Glacier response to your request.
--
-- /See:/ 'mkListVaultsResponse' smart constructor.
data ListVaultsResponse = ListVaultsResponse'
  { marker :: Core.Maybe Core.Text
    -- ^ The vault ARN at which to continue pagination of the results. You use the marker in another List Vaults request to obtain more vaults in the list.
  , vaultList :: Core.Maybe [Types.DescribeVaultOutput]
    -- ^ List of vaults.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListVaultsResponse' value with any optional fields omitted.
mkListVaultsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListVaultsResponse
mkListVaultsResponse responseStatus
  = ListVaultsResponse'{marker = Core.Nothing,
                        vaultList = Core.Nothing, responseStatus}

-- | The vault ARN at which to continue pagination of the results. You use the marker in another List Vaults request to obtain more vaults in the list.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrrsMarker :: Lens.Lens' ListVaultsResponse (Core.Maybe Core.Text)
lvrrsMarker = Lens.field @"marker"
{-# INLINEABLE lvrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | List of vaults.
--
-- /Note:/ Consider using 'vaultList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrrsVaultList :: Lens.Lens' ListVaultsResponse (Core.Maybe [Types.DescribeVaultOutput])
lvrrsVaultList = Lens.field @"vaultList"
{-# INLINEABLE lvrrsVaultList #-}
{-# DEPRECATED vaultList "Use generic-lens or generic-optics with 'vaultList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrrsResponseStatus :: Lens.Lens' ListVaultsResponse Core.Int
lvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
