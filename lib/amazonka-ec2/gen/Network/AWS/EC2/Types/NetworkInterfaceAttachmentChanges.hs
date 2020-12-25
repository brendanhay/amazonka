{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    niacAttachmentId,
    niacDeleteOnTermination,
  )
where

import qualified Network.AWS.EC2.Types.AttachmentId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an attachment change.
--
-- /See:/ 'mkNetworkInterfaceAttachmentChanges' smart constructor.
data NetworkInterfaceAttachmentChanges = NetworkInterfaceAttachmentChanges'
  { -- | The ID of the network interface attachment.
    attachmentId :: Core.Maybe Types.AttachmentId,
    -- | Indicates whether the network interface is deleted when the instance is terminated.
    deleteOnTermination :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NetworkInterfaceAttachmentChanges' value with any optional fields omitted.
mkNetworkInterfaceAttachmentChanges ::
  NetworkInterfaceAttachmentChanges
mkNetworkInterfaceAttachmentChanges =
  NetworkInterfaceAttachmentChanges'
    { attachmentId = Core.Nothing,
      deleteOnTermination = Core.Nothing
    }

-- | The ID of the network interface attachment.
--
-- /Note:/ Consider using 'attachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niacAttachmentId :: Lens.Lens' NetworkInterfaceAttachmentChanges (Core.Maybe Types.AttachmentId)
niacAttachmentId = Lens.field @"attachmentId"
{-# DEPRECATED niacAttachmentId "Use generic-lens or generic-optics with 'attachmentId' instead." #-}

-- | Indicates whether the network interface is deleted when the instance is terminated.
--
-- /Note:/ Consider using 'deleteOnTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niacDeleteOnTermination :: Lens.Lens' NetworkInterfaceAttachmentChanges (Core.Maybe Core.Bool)
niacDeleteOnTermination = Lens.field @"deleteOnTermination"
{-# DEPRECATED niacDeleteOnTermination "Use generic-lens or generic-optics with 'deleteOnTermination' instead." #-}
