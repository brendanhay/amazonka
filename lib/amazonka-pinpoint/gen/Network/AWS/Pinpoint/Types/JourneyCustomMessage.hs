{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.JourneyCustomMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.JourneyCustomMessage
  ( JourneyCustomMessage (..)
  -- * Smart constructor
  , mkJourneyCustomMessage
  -- * Lenses
  , jcmData
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the message content for a custom channel message that's sent to participants in a journey.
--
-- /See:/ 'mkJourneyCustomMessage' smart constructor.
newtype JourneyCustomMessage = JourneyCustomMessage'
  { data' :: Core.Maybe Core.Text
    -- ^ The message content that's passed to an AWS Lambda function or to a web hook.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'JourneyCustomMessage' value with any optional fields omitted.
mkJourneyCustomMessage
    :: JourneyCustomMessage
mkJourneyCustomMessage
  = JourneyCustomMessage'{data' = Core.Nothing}

-- | The message content that's passed to an AWS Lambda function or to a web hook.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jcmData :: Lens.Lens' JourneyCustomMessage (Core.Maybe Core.Text)
jcmData = Lens.field @"data'"
{-# INLINEABLE jcmData #-}
{-# DEPRECATED data' "Use generic-lens or generic-optics with 'data'' instead"  #-}

instance Core.FromJSON JourneyCustomMessage where
        toJSON JourneyCustomMessage{..}
          = Core.object (Core.catMaybes [("Data" Core..=) Core.<$> data'])

instance Core.FromJSON JourneyCustomMessage where
        parseJSON
          = Core.withObject "JourneyCustomMessage" Core.$
              \ x -> JourneyCustomMessage' Core.<$> (x Core..:? "Data")
