{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MediaConnectFlow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.MediaConnectFlow
  ( MediaConnectFlow (..)
  -- * Smart constructor
  , mkMediaConnectFlow
  -- * Lenses
  , mcfFlowArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The settings for a MediaConnect Flow.
--
-- /See:/ 'mkMediaConnectFlow' smart constructor.
newtype MediaConnectFlow = MediaConnectFlow'
  { flowArn :: Core.Maybe Core.Text
    -- ^ The unique ARN of the MediaConnect Flow being used as a source.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'MediaConnectFlow' value with any optional fields omitted.
mkMediaConnectFlow
    :: MediaConnectFlow
mkMediaConnectFlow = MediaConnectFlow'{flowArn = Core.Nothing}

-- | The unique ARN of the MediaConnect Flow being used as a source.
--
-- /Note:/ Consider using 'flowArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcfFlowArn :: Lens.Lens' MediaConnectFlow (Core.Maybe Core.Text)
mcfFlowArn = Lens.field @"flowArn"
{-# INLINEABLE mcfFlowArn #-}
{-# DEPRECATED flowArn "Use generic-lens or generic-optics with 'flowArn' instead"  #-}

instance Core.FromJSON MediaConnectFlow where
        parseJSON
          = Core.withObject "MediaConnectFlow" Core.$
              \ x -> MediaConnectFlow' Core.<$> (x Core..:? "flowArn")
