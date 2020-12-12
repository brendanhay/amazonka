{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AttachmentInformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AttachmentInformation
  ( AttachmentInformation (..),

    -- * Smart constructor
    mkAttachmentInformation,

    -- * Lenses
    aiName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An attribute of an attachment, such as the attachment name.
--
-- /See:/ 'mkAttachmentInformation' smart constructor.
newtype AttachmentInformation = AttachmentInformation'
  { name ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachmentInformation' with the minimum fields required to make a request.
--
-- * 'name' - The name of the attachment.
mkAttachmentInformation ::
  AttachmentInformation
mkAttachmentInformation =
  AttachmentInformation' {name = Lude.Nothing}

-- | The name of the attachment.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiName :: Lens.Lens' AttachmentInformation (Lude.Maybe Lude.Text)
aiName = Lens.lens (name :: AttachmentInformation -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: AttachmentInformation)
{-# DEPRECATED aiName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON AttachmentInformation where
  parseJSON =
    Lude.withObject
      "AttachmentInformation"
      (\x -> AttachmentInformation' Lude.<$> (x Lude..:? "Name"))
