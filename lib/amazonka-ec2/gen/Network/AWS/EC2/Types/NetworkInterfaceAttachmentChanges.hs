-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInterfaceAttachmentChanges
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInterfaceAttachmentChanges
  ( NetworkInterfaceAttachmentChanges (..),

    -- * Smart constructor
    mkNetworkInterfaceAttachmentChanges,

    -- * Lenses
    niacDeleteOnTermination,
    niacAttachmentId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an attachment change.
--
-- /See:/ 'mkNetworkInterfaceAttachmentChanges' smart constructor.
data NetworkInterfaceAttachmentChanges = NetworkInterfaceAttachmentChanges'
  { deleteOnTermination ::
      Lude.Maybe Lude.Bool,
    attachmentId ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NetworkInterfaceAttachmentChanges' with the minimum fields required to make a request.
--
-- * 'attachmentId' - The ID of the network interface attachment.
-- * 'deleteOnTermination' - Indicates whether the network interface is deleted when the instance is terminated.
mkNetworkInterfaceAttachmentChanges ::
  NetworkInterfaceAttachmentChanges
mkNetworkInterfaceAttachmentChanges =
  NetworkInterfaceAttachmentChanges'
    { deleteOnTermination =
        Lude.Nothing,
      attachmentId = Lude.Nothing
    }

-- | Indicates whether the network interface is deleted when the instance is terminated.
--
-- /Note:/ Consider using 'deleteOnTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niacDeleteOnTermination :: Lens.Lens' NetworkInterfaceAttachmentChanges (Lude.Maybe Lude.Bool)
niacDeleteOnTermination = Lens.lens (deleteOnTermination :: NetworkInterfaceAttachmentChanges -> Lude.Maybe Lude.Bool) (\s a -> s {deleteOnTermination = a} :: NetworkInterfaceAttachmentChanges)
{-# DEPRECATED niacDeleteOnTermination "Use generic-lens or generic-optics with 'deleteOnTermination' instead." #-}

-- | The ID of the network interface attachment.
--
-- /Note:/ Consider using 'attachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niacAttachmentId :: Lens.Lens' NetworkInterfaceAttachmentChanges (Lude.Maybe Lude.Text)
niacAttachmentId = Lens.lens (attachmentId :: NetworkInterfaceAttachmentChanges -> Lude.Maybe Lude.Text) (\s a -> s {attachmentId = a} :: NetworkInterfaceAttachmentChanges)
{-# DEPRECATED niacAttachmentId "Use generic-lens or generic-optics with 'attachmentId' instead." #-}

instance Lude.ToQuery NetworkInterfaceAttachmentChanges where
  toQuery NetworkInterfaceAttachmentChanges' {..} =
    Lude.mconcat
      [ "DeleteOnTermination" Lude.=: deleteOnTermination,
        "AttachmentId" Lude.=: attachmentId
      ]
