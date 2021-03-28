{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.VpcLink
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.VpcLink
  ( VpcLink (..)
  -- * Smart constructor
  , mkVpcLink
  -- * Lenses
  , vlDescription
  , vlId
  , vlName
  , vlStatus
  , vlStatusMessage
  , vlTags
  , vlTargetArns
  ) where

import qualified Network.AWS.ApiGateway.Types.VpcLinkStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An API Gateway VPC link for a 'RestApi' to access resources in an Amazon Virtual Private Cloud (VPC).
--
-- To enable access to a resource in an Amazon Virtual Private Cloud through Amazon API Gateway, you, as an API developer, create a 'VpcLink' resource targeted for one or more network load balancers of the VPC and then integrate an API method with a private integration that uses the 'VpcLink' . The private integration has an integration type of @HTTP@ or @HTTP_PROXY@ and has a connection type of @VPC_LINK@ . The integration uses the @connectionId@ property to identify the 'VpcLink' used.
--
--
-- /See:/ 'mkVpcLink' smart constructor.
data VpcLink = VpcLink'
  { description :: Core.Maybe Core.Text
    -- ^ The description of the VPC link.
  , id :: Core.Maybe Core.Text
    -- ^ The identifier of the 'VpcLink' . It is used in an 'Integration' to reference this 'VpcLink' .
  , name :: Core.Maybe Core.Text
    -- ^ The name used to label and identify the VPC link.
  , status :: Core.Maybe Types.VpcLinkStatus
    -- ^ The status of the VPC link. The valid values are @AVAILABLE@ , @PENDING@ , @DELETING@ , or @FAILED@ . Deploying an API will wait if the status is @PENDING@ and will fail if the status is @DELETING@ .
  , statusMessage :: Core.Maybe Core.Text
    -- ^ A description about the VPC link status.
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ The collection of tags. Each tag element is associated with a given resource.
  , targetArns :: Core.Maybe [Core.Text]
    -- ^ The ARN of the network load balancer of the VPC targeted by the VPC link. The network load balancer must be owned by the same AWS account of the API owner.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VpcLink' value with any optional fields omitted.
mkVpcLink
    :: VpcLink
mkVpcLink
  = VpcLink'{description = Core.Nothing, id = Core.Nothing,
             name = Core.Nothing, status = Core.Nothing,
             statusMessage = Core.Nothing, tags = Core.Nothing,
             targetArns = Core.Nothing}

-- | The description of the VPC link.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vlDescription :: Lens.Lens' VpcLink (Core.Maybe Core.Text)
vlDescription = Lens.field @"description"
{-# INLINEABLE vlDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The identifier of the 'VpcLink' . It is used in an 'Integration' to reference this 'VpcLink' .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vlId :: Lens.Lens' VpcLink (Core.Maybe Core.Text)
vlId = Lens.field @"id"
{-# INLINEABLE vlId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The name used to label and identify the VPC link.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vlName :: Lens.Lens' VpcLink (Core.Maybe Core.Text)
vlName = Lens.field @"name"
{-# INLINEABLE vlName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The status of the VPC link. The valid values are @AVAILABLE@ , @PENDING@ , @DELETING@ , or @FAILED@ . Deploying an API will wait if the status is @PENDING@ and will fail if the status is @DELETING@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vlStatus :: Lens.Lens' VpcLink (Core.Maybe Types.VpcLinkStatus)
vlStatus = Lens.field @"status"
{-# INLINEABLE vlStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | A description about the VPC link status.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vlStatusMessage :: Lens.Lens' VpcLink (Core.Maybe Core.Text)
vlStatusMessage = Lens.field @"statusMessage"
{-# INLINEABLE vlStatusMessage #-}
{-# DEPRECATED statusMessage "Use generic-lens or generic-optics with 'statusMessage' instead"  #-}

-- | The collection of tags. Each tag element is associated with a given resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vlTags :: Lens.Lens' VpcLink (Core.Maybe (Core.HashMap Core.Text Core.Text))
vlTags = Lens.field @"tags"
{-# INLINEABLE vlTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The ARN of the network load balancer of the VPC targeted by the VPC link. The network load balancer must be owned by the same AWS account of the API owner.
--
-- /Note:/ Consider using 'targetArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vlTargetArns :: Lens.Lens' VpcLink (Core.Maybe [Core.Text])
vlTargetArns = Lens.field @"targetArns"
{-# INLINEABLE vlTargetArns #-}
{-# DEPRECATED targetArns "Use generic-lens or generic-optics with 'targetArns' instead"  #-}

instance Core.FromJSON VpcLink where
        parseJSON
          = Core.withObject "VpcLink" Core.$
              \ x ->
                VpcLink' Core.<$>
                  (x Core..:? "description") Core.<*> x Core..:? "id" Core.<*>
                    x Core..:? "name"
                    Core.<*> x Core..:? "status"
                    Core.<*> x Core..:? "statusMessage"
                    Core.<*> x Core..:? "tags"
                    Core.<*> x Core..:? "targetArns"
