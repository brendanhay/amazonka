{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.ListRetirableGrants
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all grants for which the grant's @RetiringPrincipal@ matches the one specified.
--
-- A typical use is to list all grants that you are able to retire. To retire a grant, use 'RetireGrant' .
module Network.AWS.KMS.ListRetirableGrants
    (
    -- * Creating a request
      ListRetirableGrants (..)
    , mkListRetirableGrants
    -- ** Request lenses
    , lrgRetiringPrincipal
    , lrgLimit
    , lrgMarker

     -- * Destructuring the response
    , Types.ListGrantsResponse (..)
    , Types.mkListGrantsResponse
    -- ** Response lenses
    , Types.lgrGrants
    , Types.lgrNextMarker
    , Types.lgrTruncated
    ) where

import qualified Network.AWS.KMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListRetirableGrants' smart constructor.
data ListRetirableGrants = ListRetirableGrants'
  { retiringPrincipal :: Types.RetiringPrincipal
    -- ^ The retiring principal for which to list grants.
--
-- To specify the retiring principal, use the <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of an AWS principal. Valid AWS principals include AWS accounts (root), IAM users, federated users, and assumed role users. For examples of the ARN syntax for specifying a principal, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM)> in the Example ARNs section of the /Amazon Web Services General Reference/ .
  , limit :: Core.Maybe Core.Natural
    -- ^ Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer.
--
-- This value is optional. If you include a value, it must be between 1 and 100, inclusive. If you do not include a value, it defaults to 50.
  , marker :: Core.Maybe Types.Marker
    -- ^ Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListRetirableGrants' value with any optional fields omitted.
mkListRetirableGrants
    :: Types.RetiringPrincipal -- ^ 'retiringPrincipal'
    -> ListRetirableGrants
mkListRetirableGrants retiringPrincipal
  = ListRetirableGrants'{retiringPrincipal, limit = Core.Nothing,
                         marker = Core.Nothing}

-- | The retiring principal for which to list grants.
--
-- To specify the retiring principal, use the <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of an AWS principal. Valid AWS principals include AWS accounts (root), IAM users, federated users, and assumed role users. For examples of the ARN syntax for specifying a principal, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM)> in the Example ARNs section of the /Amazon Web Services General Reference/ .
--
-- /Note:/ Consider using 'retiringPrincipal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrgRetiringPrincipal :: Lens.Lens' ListRetirableGrants Types.RetiringPrincipal
lrgRetiringPrincipal = Lens.field @"retiringPrincipal"
{-# INLINEABLE lrgRetiringPrincipal #-}
{-# DEPRECATED retiringPrincipal "Use generic-lens or generic-optics with 'retiringPrincipal' instead"  #-}

-- | Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer.
--
-- This value is optional. If you include a value, it must be between 1 and 100, inclusive. If you do not include a value, it defaults to 50.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrgLimit :: Lens.Lens' ListRetirableGrants (Core.Maybe Core.Natural)
lrgLimit = Lens.field @"limit"
{-# INLINEABLE lrgLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrgMarker :: Lens.Lens' ListRetirableGrants (Core.Maybe Types.Marker)
lrgMarker = Lens.field @"marker"
{-# INLINEABLE lrgMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

instance Core.ToQuery ListRetirableGrants where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListRetirableGrants where
        toHeaders ListRetirableGrants{..}
          = Core.pure ("X-Amz-Target", "TrentService.ListRetirableGrants")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListRetirableGrants where
        toJSON ListRetirableGrants{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("RetiringPrincipal" Core..= retiringPrincipal),
                  ("Limit" Core..=) Core.<$> limit,
                  ("Marker" Core..=) Core.<$> marker])

instance Core.AWSRequest ListRetirableGrants where
        type Rs ListRetirableGrants = Types.ListGrantsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
