{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    acHashType,
    acName,
    acSize,
    acUrl,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.AttachmentHash as Types
import qualified Network.AWS.SSM.Types.AttachmentHashType as Types
import qualified Network.AWS.SSM.Types.AttachmentName as Types
import qualified Network.AWS.SSM.Types.AttachmentUrl as Types

-- | A structure that includes attributes that describe a document attachment.
--
-- /See:/ 'mkAttachmentContent' smart constructor.
data AttachmentContent = AttachmentContent'
  { -- | The cryptographic hash value of the document content.
    hash :: Core.Maybe Types.AttachmentHash,
    -- | The hash algorithm used to calculate the hash value.
    hashType :: Core.Maybe Types.AttachmentHashType,
    -- | The name of an attachment.
    name :: Core.Maybe Types.AttachmentName,
    -- | The size of an attachment in bytes.
    size :: Core.Maybe Core.Integer,
    -- | The URL location of the attachment content.
    url :: Core.Maybe Types.AttachmentUrl
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachmentContent' value with any optional fields omitted.
mkAttachmentContent ::
  AttachmentContent
mkAttachmentContent =
  AttachmentContent'
    { hash = Core.Nothing,
      hashType = Core.Nothing,
      name = Core.Nothing,
      size = Core.Nothing,
      url = Core.Nothing
    }

-- | The cryptographic hash value of the document content.
--
-- /Note:/ Consider using 'hash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acHash :: Lens.Lens' AttachmentContent (Core.Maybe Types.AttachmentHash)
acHash = Lens.field @"hash"
{-# DEPRECATED acHash "Use generic-lens or generic-optics with 'hash' instead." #-}

-- | The hash algorithm used to calculate the hash value.
--
-- /Note:/ Consider using 'hashType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acHashType :: Lens.Lens' AttachmentContent (Core.Maybe Types.AttachmentHashType)
acHashType = Lens.field @"hashType"
{-# DEPRECATED acHashType "Use generic-lens or generic-optics with 'hashType' instead." #-}

-- | The name of an attachment.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acName :: Lens.Lens' AttachmentContent (Core.Maybe Types.AttachmentName)
acName = Lens.field @"name"
{-# DEPRECATED acName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The size of an attachment in bytes.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acSize :: Lens.Lens' AttachmentContent (Core.Maybe Core.Integer)
acSize = Lens.field @"size"
{-# DEPRECATED acSize "Use generic-lens or generic-optics with 'size' instead." #-}

-- | The URL location of the attachment content.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acUrl :: Lens.Lens' AttachmentContent (Core.Maybe Types.AttachmentUrl)
acUrl = Lens.field @"url"
{-# DEPRECATED acUrl "Use generic-lens or generic-optics with 'url' instead." #-}

instance Core.FromJSON AttachmentContent where
  parseJSON =
    Core.withObject "AttachmentContent" Core.$
      \x ->
        AttachmentContent'
          Core.<$> (x Core..:? "Hash")
          Core.<*> (x Core..:? "HashType")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "Size")
          Core.<*> (x Core..:? "Url")
