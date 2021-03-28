{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.EventDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AWSHealth.Types.EventDescription
  ( EventDescription (..)
  -- * Smart constructor
  , mkEventDescription
  -- * Lenses
  , edLatestDescription
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The detailed description of the event. Included in the information returned by the <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetails.html DescribeEventDetails> operation.
--
-- /See:/ 'mkEventDescription' smart constructor.
newtype EventDescription = EventDescription'
  { latestDescription :: Core.Maybe EventDescription
    -- ^ The most recent description of the event.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EventDescription' value with any optional fields omitted.
mkEventDescription
    :: EventDescription
mkEventDescription
  = EventDescription'{latestDescription = Core.Nothing}

-- | The most recent description of the event.
--
-- /Note:/ Consider using 'latestDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edLatestDescription :: Lens.Lens' EventDescription (Core.Maybe EventDescription)
edLatestDescription = Lens.field @"latestDescription"
{-# INLINEABLE edLatestDescription #-}
{-# DEPRECATED latestDescription "Use generic-lens or generic-optics with 'latestDescription' instead"  #-}

instance Core.FromJSON EventDescription where
        parseJSON
          = Core.withObject "EventDescription" Core.$
              \ x -> EventDescription' Core.<$> (x Core..:? "latestDescription")
