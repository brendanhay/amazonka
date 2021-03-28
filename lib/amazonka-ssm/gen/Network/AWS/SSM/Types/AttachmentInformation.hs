{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AttachmentInformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.AttachmentInformation
  ( AttachmentInformation (..)
  -- * Smart constructor
  , mkAttachmentInformation
  -- * Lenses
  , aiName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.Name as Types

-- | An attribute of an attachment, such as the attachment name.
--
-- /See:/ 'mkAttachmentInformation' smart constructor.
newtype AttachmentInformation = AttachmentInformation'
  { name :: Core.Maybe Types.Name
    -- ^ The name of the attachment.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AttachmentInformation' value with any optional fields omitted.
mkAttachmentInformation
    :: AttachmentInformation
mkAttachmentInformation
  = AttachmentInformation'{name = Core.Nothing}

-- | The name of the attachment.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiName :: Lens.Lens' AttachmentInformation (Core.Maybe Types.Name)
aiName = Lens.field @"name"
{-# INLINEABLE aiName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON AttachmentInformation where
        parseJSON
          = Core.withObject "AttachmentInformation" Core.$
              \ x -> AttachmentInformation' Core.<$> (x Core..:? "Name")
