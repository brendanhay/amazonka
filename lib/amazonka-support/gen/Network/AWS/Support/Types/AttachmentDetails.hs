{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.AttachmentDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.AttachmentDetails
  ( AttachmentDetails (..),

    -- * Smart constructor
    mkAttachmentDetails,

    -- * Lenses
    adAttachmentId,
    adFileName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Support.Types.AttachmentId as Types
import qualified Network.AWS.Support.Types.FileName as Types

-- | The file name and ID of an attachment to a case communication. You can use the ID to retrieve the attachment with the 'DescribeAttachment' operation.
--
-- /See:/ 'mkAttachmentDetails' smart constructor.
data AttachmentDetails = AttachmentDetails'
  { -- | The ID of the attachment.
    attachmentId :: Core.Maybe Types.AttachmentId,
    -- | The file name of the attachment.
    fileName :: Core.Maybe Types.FileName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachmentDetails' value with any optional fields omitted.
mkAttachmentDetails ::
  AttachmentDetails
mkAttachmentDetails =
  AttachmentDetails'
    { attachmentId = Core.Nothing,
      fileName = Core.Nothing
    }

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'attachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAttachmentId :: Lens.Lens' AttachmentDetails (Core.Maybe Types.AttachmentId)
adAttachmentId = Lens.field @"attachmentId"
{-# DEPRECATED adAttachmentId "Use generic-lens or generic-optics with 'attachmentId' instead." #-}

-- | The file name of the attachment.
--
-- /Note:/ Consider using 'fileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adFileName :: Lens.Lens' AttachmentDetails (Core.Maybe Types.FileName)
adFileName = Lens.field @"fileName"
{-# DEPRECATED adFileName "Use generic-lens or generic-optics with 'fileName' instead." #-}

instance Core.FromJSON AttachmentDetails where
  parseJSON =
    Core.withObject "AttachmentDetails" Core.$
      \x ->
        AttachmentDetails'
          Core.<$> (x Core..:? "attachmentId") Core.<*> (x Core..:? "fileName")
