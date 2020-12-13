{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InternetGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InternetGateway
  ( InternetGateway (..),

    -- * Smart constructor
    mkInternetGateway,

    -- * Lenses
    igAttachments,
    igOwnerId,
    igInternetGatewayId,
    igTags,
  )
where

import Network.AWS.EC2.Types.InternetGatewayAttachment
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an internet gateway.
--
-- /See:/ 'mkInternetGateway' smart constructor.
data InternetGateway = InternetGateway'
  { -- | Any VPCs attached to the internet gateway.
    attachments :: Lude.Maybe [InternetGatewayAttachment],
    -- | The ID of the AWS account that owns the internet gateway.
    ownerId :: Lude.Maybe Lude.Text,
    -- | The ID of the internet gateway.
    internetGatewayId :: Lude.Text,
    -- | Any tags assigned to the internet gateway.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InternetGateway' with the minimum fields required to make a request.
--
-- * 'attachments' - Any VPCs attached to the internet gateway.
-- * 'ownerId' - The ID of the AWS account that owns the internet gateway.
-- * 'internetGatewayId' - The ID of the internet gateway.
-- * 'tags' - Any tags assigned to the internet gateway.
mkInternetGateway ::
  -- | 'internetGatewayId'
  Lude.Text ->
  InternetGateway
mkInternetGateway pInternetGatewayId_ =
  InternetGateway'
    { attachments = Lude.Nothing,
      ownerId = Lude.Nothing,
      internetGatewayId = pInternetGatewayId_,
      tags = Lude.Nothing
    }

-- | Any VPCs attached to the internet gateway.
--
-- /Note:/ Consider using 'attachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igAttachments :: Lens.Lens' InternetGateway (Lude.Maybe [InternetGatewayAttachment])
igAttachments = Lens.lens (attachments :: InternetGateway -> Lude.Maybe [InternetGatewayAttachment]) (\s a -> s {attachments = a} :: InternetGateway)
{-# DEPRECATED igAttachments "Use generic-lens or generic-optics with 'attachments' instead." #-}

-- | The ID of the AWS account that owns the internet gateway.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igOwnerId :: Lens.Lens' InternetGateway (Lude.Maybe Lude.Text)
igOwnerId = Lens.lens (ownerId :: InternetGateway -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: InternetGateway)
{-# DEPRECATED igOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The ID of the internet gateway.
--
-- /Note:/ Consider using 'internetGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igInternetGatewayId :: Lens.Lens' InternetGateway Lude.Text
igInternetGatewayId = Lens.lens (internetGatewayId :: InternetGateway -> Lude.Text) (\s a -> s {internetGatewayId = a} :: InternetGateway)
{-# DEPRECATED igInternetGatewayId "Use generic-lens or generic-optics with 'internetGatewayId' instead." #-}

-- | Any tags assigned to the internet gateway.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igTags :: Lens.Lens' InternetGateway (Lude.Maybe [Tag])
igTags = Lens.lens (tags :: InternetGateway -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: InternetGateway)
{-# DEPRECATED igTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML InternetGateway where
  parseXML x =
    InternetGateway'
      Lude.<$> ( x Lude..@? "attachmentSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "ownerId")
      Lude.<*> (x Lude..@ "internetGatewayId")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
