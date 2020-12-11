-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AttachmentContent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AttachmentContent
  ( AttachmentContent (..),

    -- * Smart constructor
    mkAttachmentContent,

    -- * Lenses
    acHash,
    acSize,
    acURL,
    acName,
    acHashType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.AttachmentHashType

-- | A structure that includes attributes that describe a document attachment.
--
-- /See:/ 'mkAttachmentContent' smart constructor.
data AttachmentContent = AttachmentContent'
  { hash ::
      Lude.Maybe Lude.Text,
    size :: Lude.Maybe Lude.Integer,
    url :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    hashType :: Lude.Maybe AttachmentHashType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachmentContent' with the minimum fields required to make a request.
--
-- * 'hash' - The cryptographic hash value of the document content.
-- * 'hashType' - The hash algorithm used to calculate the hash value.
-- * 'name' - The name of an attachment.
-- * 'size' - The size of an attachment in bytes.
-- * 'url' - The URL location of the attachment content.
mkAttachmentContent ::
  AttachmentContent
mkAttachmentContent =
  AttachmentContent'
    { hash = Lude.Nothing,
      size = Lude.Nothing,
      url = Lude.Nothing,
      name = Lude.Nothing,
      hashType = Lude.Nothing
    }

-- | The cryptographic hash value of the document content.
--
-- /Note:/ Consider using 'hash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acHash :: Lens.Lens' AttachmentContent (Lude.Maybe Lude.Text)
acHash = Lens.lens (hash :: AttachmentContent -> Lude.Maybe Lude.Text) (\s a -> s {hash = a} :: AttachmentContent)
{-# DEPRECATED acHash "Use generic-lens or generic-optics with 'hash' instead." #-}

-- | The size of an attachment in bytes.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acSize :: Lens.Lens' AttachmentContent (Lude.Maybe Lude.Integer)
acSize = Lens.lens (size :: AttachmentContent -> Lude.Maybe Lude.Integer) (\s a -> s {size = a} :: AttachmentContent)
{-# DEPRECATED acSize "Use generic-lens or generic-optics with 'size' instead." #-}

-- | The URL location of the attachment content.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acURL :: Lens.Lens' AttachmentContent (Lude.Maybe Lude.Text)
acURL = Lens.lens (url :: AttachmentContent -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: AttachmentContent)
{-# DEPRECATED acURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The name of an attachment.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acName :: Lens.Lens' AttachmentContent (Lude.Maybe Lude.Text)
acName = Lens.lens (name :: AttachmentContent -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: AttachmentContent)
{-# DEPRECATED acName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The hash algorithm used to calculate the hash value.
--
-- /Note:/ Consider using 'hashType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acHashType :: Lens.Lens' AttachmentContent (Lude.Maybe AttachmentHashType)
acHashType = Lens.lens (hashType :: AttachmentContent -> Lude.Maybe AttachmentHashType) (\s a -> s {hashType = a} :: AttachmentContent)
{-# DEPRECATED acHashType "Use generic-lens or generic-optics with 'hashType' instead." #-}

instance Lude.FromJSON AttachmentContent where
  parseJSON =
    Lude.withObject
      "AttachmentContent"
      ( \x ->
          AttachmentContent'
            Lude.<$> (x Lude..:? "Hash")
            Lude.<*> (x Lude..:? "Size")
            Lude.<*> (x Lude..:? "Url")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "HashType")
      )
