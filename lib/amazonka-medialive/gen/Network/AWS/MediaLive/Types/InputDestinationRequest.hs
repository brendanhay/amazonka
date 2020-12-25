{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputDestinationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDestinationRequest
  ( InputDestinationRequest (..),

    -- * Smart constructor
    mkInputDestinationRequest,

    -- * Lenses
    idrStreamName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Endpoint settings for a PUSH type input.
--
-- /See:/ 'mkInputDestinationRequest' smart constructor.
newtype InputDestinationRequest = InputDestinationRequest'
  { -- | A unique name for the location the RTMP stream is being pushed
    --
    -- to.
    streamName :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'InputDestinationRequest' value with any optional fields omitted.
mkInputDestinationRequest ::
  InputDestinationRequest
mkInputDestinationRequest =
  InputDestinationRequest' {streamName = Core.Nothing}

-- | A unique name for the location the RTMP stream is being pushed
--
-- to.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idrStreamName :: Lens.Lens' InputDestinationRequest (Core.Maybe Core.Text)
idrStreamName = Lens.field @"streamName"
{-# DEPRECATED idrStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

instance Core.FromJSON InputDestinationRequest where
  toJSON InputDestinationRequest {..} =
    Core.object
      (Core.catMaybes [("streamName" Core..=) Core.<$> streamName])
