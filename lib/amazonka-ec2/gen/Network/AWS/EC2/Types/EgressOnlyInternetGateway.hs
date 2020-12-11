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
    eoigEgressOnlyInternetGatewayId,
    eoigAttachments,
    eoigTags,
  )
where

import Network.AWS.EC2.Types.InternetGatewayAttachment
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an egress-only internet gateway.
--
-- /See:/ 'mkEgressOnlyInternetGateway' smart constructor.
data EgressOnlyInternetGateway = EgressOnlyInternetGateway'
  { egressOnlyInternetGatewayId ::
      Lude.Maybe Lude.Text,
    attachments ::
      Lude.Maybe [InternetGatewayAttachment],
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EgressOnlyInternetGateway' with the minimum fields required to make a request.
--
-- * 'attachments' - Information about the attachment of the egress-only internet gateway.
-- * 'egressOnlyInternetGatewayId' - The ID of the egress-only internet gateway.
-- * 'tags' - The tags assigned to the egress-only internet gateway.
mkEgressOnlyInternetGateway ::
  EgressOnlyInternetGateway
mkEgressOnlyInternetGateway =
  EgressOnlyInternetGateway'
    { egressOnlyInternetGatewayId =
        Lude.Nothing,
      attachments = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The ID of the egress-only internet gateway.
--
-- /Note:/ Consider using 'egressOnlyInternetGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoigEgressOnlyInternetGatewayId :: Lens.Lens' EgressOnlyInternetGateway (Lude.Maybe Lude.Text)
eoigEgressOnlyInternetGatewayId = Lens.lens (egressOnlyInternetGatewayId :: EgressOnlyInternetGateway -> Lude.Maybe Lude.Text) (\s a -> s {egressOnlyInternetGatewayId = a} :: EgressOnlyInternetGateway)
{-# DEPRECATED eoigEgressOnlyInternetGatewayId "Use generic-lens or generic-optics with 'egressOnlyInternetGatewayId' instead." #-}

-- | Information about the attachment of the egress-only internet gateway.
--
-- /Note:/ Consider using 'attachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoigAttachments :: Lens.Lens' EgressOnlyInternetGateway (Lude.Maybe [InternetGatewayAttachment])
eoigAttachments = Lens.lens (attachments :: EgressOnlyInternetGateway -> Lude.Maybe [InternetGatewayAttachment]) (\s a -> s {attachments = a} :: EgressOnlyInternetGateway)
{-# DEPRECATED eoigAttachments "Use generic-lens or generic-optics with 'attachments' instead." #-}

-- | The tags assigned to the egress-only internet gateway.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoigTags :: Lens.Lens' EgressOnlyInternetGateway (Lude.Maybe [Tag])
eoigTags = Lens.lens (tags :: EgressOnlyInternetGateway -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: EgressOnlyInternetGateway)
{-# DEPRECATED eoigTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML EgressOnlyInternetGateway where
  parseXML x =
    EgressOnlyInternetGateway'
      Lude.<$> (x Lude..@? "egressOnlyInternetGatewayId")
      Lude.<*> ( x Lude..@? "attachmentSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
