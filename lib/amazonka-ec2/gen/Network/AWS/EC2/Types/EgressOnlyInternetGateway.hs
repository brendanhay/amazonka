{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EgressOnlyInternetGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EgressOnlyInternetGateway
  ( EgressOnlyInternetGateway (..),

    -- * Smart constructor
    mkEgressOnlyInternetGateway,

    -- * Lenses
    eoigAttachments,
    eoigEgressOnlyInternetGatewayId,
    eoigTags,
  )
where

import qualified Network.AWS.EC2.Types.EgressOnlyInternetGatewayId as Types
import qualified Network.AWS.EC2.Types.InternetGatewayAttachment as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an egress-only internet gateway.
--
-- /See:/ 'mkEgressOnlyInternetGateway' smart constructor.
data EgressOnlyInternetGateway = EgressOnlyInternetGateway'
  { -- | Information about the attachment of the egress-only internet gateway.
    attachments :: Core.Maybe [Types.InternetGatewayAttachment],
    -- | The ID of the egress-only internet gateway.
    egressOnlyInternetGatewayId :: Core.Maybe Types.EgressOnlyInternetGatewayId,
    -- | The tags assigned to the egress-only internet gateway.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EgressOnlyInternetGateway' value with any optional fields omitted.
mkEgressOnlyInternetGateway ::
  EgressOnlyInternetGateway
mkEgressOnlyInternetGateway =
  EgressOnlyInternetGateway'
    { attachments = Core.Nothing,
      egressOnlyInternetGatewayId = Core.Nothing,
      tags = Core.Nothing
    }

-- | Information about the attachment of the egress-only internet gateway.
--
-- /Note:/ Consider using 'attachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoigAttachments :: Lens.Lens' EgressOnlyInternetGateway (Core.Maybe [Types.InternetGatewayAttachment])
eoigAttachments = Lens.field @"attachments"
{-# DEPRECATED eoigAttachments "Use generic-lens or generic-optics with 'attachments' instead." #-}

-- | The ID of the egress-only internet gateway.
--
-- /Note:/ Consider using 'egressOnlyInternetGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoigEgressOnlyInternetGatewayId :: Lens.Lens' EgressOnlyInternetGateway (Core.Maybe Types.EgressOnlyInternetGatewayId)
eoigEgressOnlyInternetGatewayId = Lens.field @"egressOnlyInternetGatewayId"
{-# DEPRECATED eoigEgressOnlyInternetGatewayId "Use generic-lens or generic-optics with 'egressOnlyInternetGatewayId' instead." #-}

-- | The tags assigned to the egress-only internet gateway.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoigTags :: Lens.Lens' EgressOnlyInternetGateway (Core.Maybe [Types.Tag])
eoigTags = Lens.field @"tags"
{-# DEPRECATED eoigTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromXML EgressOnlyInternetGateway where
  parseXML x =
    EgressOnlyInternetGateway'
      Core.<$> (x Core..@? "attachmentSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "egressOnlyInternetGatewayId")
      Core.<*> (x Core..@? "tagSet" Core..<@> Core.parseXMLList "item")
