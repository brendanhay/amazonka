{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.CreateVpcLink
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a VPC link, under the caller's account in a selected region, in an asynchronous operation that typically takes 2-4 minutes to complete and become operational. The caller must have permissions to create and update VPC Endpoint services.
module Network.AWS.ApiGateway.CreateVpcLink
    (
    -- * Creating a request
      CreateVpcLink (..)
    , mkCreateVpcLink
    -- ** Request lenses
    , cvlName
    , cvlTargetArns
    , cvlDescription
    , cvlTags

     -- * Destructuring the response
    , Types.VpcLink (..)
    , Types.mkVpcLink
    -- ** Response lenses
    , Types.vlDescription
    , Types.vlId
    , Types.vlName
    , Types.vlStatus
    , Types.vlStatusMessage
    , Types.vlTags
    , Types.vlTargetArns
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Creates a VPC link, under the caller's account in a selected region, in an asynchronous operation that typically takes 2-4 minutes to complete and become operational. The caller must have permissions to create and update VPC Endpoint services.
--
-- /See:/ 'mkCreateVpcLink' smart constructor.
data CreateVpcLink = CreateVpcLink'
  { name :: Core.Text
    -- ^ [Required] The name used to label and identify the VPC link.
  , targetArns :: [Core.Text]
    -- ^ [Required] The ARN of the network load balancer of the VPC targeted by the VPC link. The network load balancer must be owned by the same AWS account of the API owner.
  , description :: Core.Maybe Core.Text
    -- ^ The description of the VPC link.
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ The key-value map of strings. The valid character set is [a-zA-Z+-=._:/]. The tag key can be up to 128 characters and must not start with @aws:@ . The tag value can be up to 256 characters.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateVpcLink' value with any optional fields omitted.
mkCreateVpcLink
    :: Core.Text -- ^ 'name'
    -> CreateVpcLink
mkCreateVpcLink name
  = CreateVpcLink'{name, targetArns = Core.mempty,
                   description = Core.Nothing, tags = Core.Nothing}

-- | [Required] The name used to label and identify the VPC link.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvlName :: Lens.Lens' CreateVpcLink Core.Text
cvlName = Lens.field @"name"
{-# INLINEABLE cvlName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | [Required] The ARN of the network load balancer of the VPC targeted by the VPC link. The network load balancer must be owned by the same AWS account of the API owner.
--
-- /Note:/ Consider using 'targetArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvlTargetArns :: Lens.Lens' CreateVpcLink [Core.Text]
cvlTargetArns = Lens.field @"targetArns"
{-# INLINEABLE cvlTargetArns #-}
{-# DEPRECATED targetArns "Use generic-lens or generic-optics with 'targetArns' instead"  #-}

-- | The description of the VPC link.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvlDescription :: Lens.Lens' CreateVpcLink (Core.Maybe Core.Text)
cvlDescription = Lens.field @"description"
{-# INLINEABLE cvlDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The key-value map of strings. The valid character set is [a-zA-Z+-=._:/]. The tag key can be up to 128 characters and must not start with @aws:@ . The tag value can be up to 256 characters.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvlTags :: Lens.Lens' CreateVpcLink (Core.Maybe (Core.HashMap Core.Text Core.Text))
cvlTags = Lens.field @"tags"
{-# INLINEABLE cvlTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateVpcLink where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateVpcLink where
        toHeaders CreateVpcLink{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON CreateVpcLink where
        toJSON CreateVpcLink{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name),
                  Core.Just ("targetArns" Core..= targetArns),
                  ("description" Core..=) Core.<$> description,
                  ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateVpcLink where
        type Rs CreateVpcLink = Types.VpcLink
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/vpclinks",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
