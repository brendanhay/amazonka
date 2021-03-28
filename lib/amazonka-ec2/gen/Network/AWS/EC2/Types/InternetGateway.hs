{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InternetGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.InternetGateway
  ( InternetGateway (..)
  -- * Smart constructor
  , mkInternetGateway
  -- * Lenses
  , igAttachments
  , igInternetGatewayId
  , igOwnerId
  , igTags
  ) where

import qualified Network.AWS.EC2.Types.InternetGatewayAttachment as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an internet gateway.
--
-- /See:/ 'mkInternetGateway' smart constructor.
data InternetGateway = InternetGateway'
  { attachments :: Core.Maybe [Types.InternetGatewayAttachment]
    -- ^ Any VPCs attached to the internet gateway.
  , internetGatewayId :: Core.Text
    -- ^ The ID of the internet gateway.
  , ownerId :: Core.Maybe Core.Text
    -- ^ The ID of the AWS account that owns the internet gateway.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Any tags assigned to the internet gateway.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InternetGateway' value with any optional fields omitted.
mkInternetGateway
    :: Core.Text -- ^ 'internetGatewayId'
    -> InternetGateway
mkInternetGateway internetGatewayId
  = InternetGateway'{attachments = Core.Nothing, internetGatewayId,
                     ownerId = Core.Nothing, tags = Core.Nothing}

-- | Any VPCs attached to the internet gateway.
--
-- /Note:/ Consider using 'attachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igAttachments :: Lens.Lens' InternetGateway (Core.Maybe [Types.InternetGatewayAttachment])
igAttachments = Lens.field @"attachments"
{-# INLINEABLE igAttachments #-}
{-# DEPRECATED attachments "Use generic-lens or generic-optics with 'attachments' instead"  #-}

-- | The ID of the internet gateway.
--
-- /Note:/ Consider using 'internetGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igInternetGatewayId :: Lens.Lens' InternetGateway Core.Text
igInternetGatewayId = Lens.field @"internetGatewayId"
{-# INLINEABLE igInternetGatewayId #-}
{-# DEPRECATED internetGatewayId "Use generic-lens or generic-optics with 'internetGatewayId' instead"  #-}

-- | The ID of the AWS account that owns the internet gateway.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igOwnerId :: Lens.Lens' InternetGateway (Core.Maybe Core.Text)
igOwnerId = Lens.field @"ownerId"
{-# INLINEABLE igOwnerId #-}
{-# DEPRECATED ownerId "Use generic-lens or generic-optics with 'ownerId' instead"  #-}

-- | Any tags assigned to the internet gateway.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igTags :: Lens.Lens' InternetGateway (Core.Maybe [Types.Tag])
igTags = Lens.field @"tags"
{-# INLINEABLE igTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromXML InternetGateway where
        parseXML x
          = InternetGateway' Core.<$>
              (x Core..@? "attachmentSet" Core..<@> Core.parseXMLList "item")
                Core.<*> x Core..@ "internetGatewayId"
                Core.<*> x Core..@? "ownerId"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
