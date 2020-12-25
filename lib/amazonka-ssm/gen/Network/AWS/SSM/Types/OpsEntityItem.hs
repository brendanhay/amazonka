{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsEntityItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsEntityItem
  ( OpsEntityItem (..),

    -- * Smart constructor
    mkOpsEntityItem,

    -- * Lenses
    oeiCaptureTime,
    oeiContent,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.AttributeName as Types
import qualified Network.AWS.SSM.Types.AttributeValue as Types
import qualified Network.AWS.SSM.Types.CaptureTime as Types

-- | The OpsItem summaries result item.
--
-- /See:/ 'mkOpsEntityItem' smart constructor.
data OpsEntityItem = OpsEntityItem'
  { -- | The time OpsItem data was captured.
    captureTime :: Core.Maybe Types.CaptureTime,
    -- | The detailed data content for an OpsItem summaries result item.
    content :: Core.Maybe [Core.HashMap Types.AttributeName Types.AttributeValue]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OpsEntityItem' value with any optional fields omitted.
mkOpsEntityItem ::
  OpsEntityItem
mkOpsEntityItem =
  OpsEntityItem'
    { captureTime = Core.Nothing,
      content = Core.Nothing
    }

-- | The time OpsItem data was captured.
--
-- /Note:/ Consider using 'captureTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeiCaptureTime :: Lens.Lens' OpsEntityItem (Core.Maybe Types.CaptureTime)
oeiCaptureTime = Lens.field @"captureTime"
{-# DEPRECATED oeiCaptureTime "Use generic-lens or generic-optics with 'captureTime' instead." #-}

-- | The detailed data content for an OpsItem summaries result item.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeiContent :: Lens.Lens' OpsEntityItem (Core.Maybe [Core.HashMap Types.AttributeName Types.AttributeValue])
oeiContent = Lens.field @"content"
{-# DEPRECATED oeiContent "Use generic-lens or generic-optics with 'content' instead." #-}

instance Core.FromJSON OpsEntityItem where
  parseJSON =
    Core.withObject "OpsEntityItem" Core.$
      \x ->
        OpsEntityItem'
          Core.<$> (x Core..:? "CaptureTime") Core.<*> (x Core..:? "Content")
