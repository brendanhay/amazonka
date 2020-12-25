{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MediaConnectFlowRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MediaConnectFlowRequest
  ( MediaConnectFlowRequest (..),

    -- * Smart constructor
    mkMediaConnectFlowRequest,

    -- * Lenses
    mcfrFlowArn,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The settings for a MediaConnect Flow.
--
-- /See:/ 'mkMediaConnectFlowRequest' smart constructor.
newtype MediaConnectFlowRequest = MediaConnectFlowRequest'
  { -- | The ARN of the MediaConnect Flow that you want to use as a source.
    flowArn :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'MediaConnectFlowRequest' value with any optional fields omitted.
mkMediaConnectFlowRequest ::
  MediaConnectFlowRequest
mkMediaConnectFlowRequest =
  MediaConnectFlowRequest' {flowArn = Core.Nothing}

-- | The ARN of the MediaConnect Flow that you want to use as a source.
--
-- /Note:/ Consider using 'flowArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcfrFlowArn :: Lens.Lens' MediaConnectFlowRequest (Core.Maybe Core.Text)
mcfrFlowArn = Lens.field @"flowArn"
{-# DEPRECATED mcfrFlowArn "Use generic-lens or generic-optics with 'flowArn' instead." #-}

instance Core.FromJSON MediaConnectFlowRequest where
  toJSON MediaConnectFlowRequest {..} =
    Core.object
      (Core.catMaybes [("flowArn" Core..=) Core.<$> flowArn])
