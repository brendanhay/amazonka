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
import qualified Network.AWS.Prelude as Lude

-- | The file name and ID of an attachment to a case communication. You can use the ID to retrieve the attachment with the 'DescribeAttachment' operation.
--
-- /See:/ 'mkAttachmentDetails' smart constructor.
data AttachmentDetails = AttachmentDetails'
  { -- | The ID of the attachment.
    attachmentId :: Lude.Maybe Lude.Text,
    -- | The file name of the attachment.
    fileName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachmentDetails' with the minimum fields required to make a request.
--
-- * 'attachmentId' - The ID of the attachment.
-- * 'fileName' - The file name of the attachment.
mkAttachmentDetails ::
  AttachmentDetails
mkAttachmentDetails =
  AttachmentDetails'
    { attachmentId = Lude.Nothing,
      fileName = Lude.Nothing
    }

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'attachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAttachmentId :: Lens.Lens' AttachmentDetails (Lude.Maybe Lude.Text)
adAttachmentId = Lens.lens (attachmentId :: AttachmentDetails -> Lude.Maybe Lude.Text) (\s a -> s {attachmentId = a} :: AttachmentDetails)
{-# DEPRECATED adAttachmentId "Use generic-lens or generic-optics with 'attachmentId' instead." #-}

-- | The file name of the attachment.
--
-- /Note:/ Consider using 'fileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adFileName :: Lens.Lens' AttachmentDetails (Lude.Maybe Lude.Text)
adFileName = Lens.lens (fileName :: AttachmentDetails -> Lude.Maybe Lude.Text) (\s a -> s {fileName = a} :: AttachmentDetails)
{-# DEPRECATED adFileName "Use generic-lens or generic-optics with 'fileName' instead." #-}

instance Lude.FromJSON AttachmentDetails where
  parseJSON =
    Lude.withObject
      "AttachmentDetails"
      ( \x ->
          AttachmentDetails'
            Lude.<$> (x Lude..:? "attachmentId") Lude.<*> (x Lude..:? "fileName")
      )
