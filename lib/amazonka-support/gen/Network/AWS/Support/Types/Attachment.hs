{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.Attachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.Attachment
  ( Attachment (..),

    -- * Smart constructor
    mkAttachment,

    -- * Lenses
    aData,
    aFileName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Support.Types.FileName as Types

-- | An attachment to a case communication. The attachment consists of the file name and the content of the file.
--
-- /See:/ 'mkAttachment' smart constructor.
data Attachment = Attachment'
  { -- | The content of the attachment file.
    data' :: Core.Maybe Core.Base64,
    -- | The name of the attachment file.
    fileName :: Core.Maybe Types.FileName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Attachment' value with any optional fields omitted.
mkAttachment ::
  Attachment
mkAttachment =
  Attachment' {data' = Core.Nothing, fileName = Core.Nothing}

-- | The content of the attachment file.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aData :: Lens.Lens' Attachment (Core.Maybe Core.Base64)
aData = Lens.field @"data'"
{-# DEPRECATED aData "Use generic-lens or generic-optics with 'data'' instead." #-}

-- | The name of the attachment file.
--
-- /Note:/ Consider using 'fileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aFileName :: Lens.Lens' Attachment (Core.Maybe Types.FileName)
aFileName = Lens.field @"fileName"
{-# DEPRECATED aFileName "Use generic-lens or generic-optics with 'fileName' instead." #-}

instance Core.FromJSON Attachment where
  toJSON Attachment {..} =
    Core.object
      ( Core.catMaybes
          [ ("data" Core..=) Core.<$> data',
            ("fileName" Core..=) Core.<$> fileName
          ]
      )

instance Core.FromJSON Attachment where
  parseJSON =
    Core.withObject "Attachment" Core.$
      \x ->
        Attachment'
          Core.<$> (x Core..:? "data") Core.<*> (x Core..:? "fileName")
