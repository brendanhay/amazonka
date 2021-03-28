{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.SourceDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.SourceDescription
  ( SourceDescription (..)
  -- * Smart constructor
  , mkSourceDescription
  -- * Lenses
  , sdKinesisStreamSourceDescription
  ) where

import qualified Network.AWS.Firehose.Types.KinesisStreamSourceDescription as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details about a Kinesis data stream used as the source for a Kinesis Data Firehose delivery stream.
--
-- /See:/ 'mkSourceDescription' smart constructor.
newtype SourceDescription = SourceDescription'
  { kinesisStreamSourceDescription :: Core.Maybe Types.KinesisStreamSourceDescription
    -- ^ The 'KinesisStreamSourceDescription' value for the source Kinesis data stream.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype Core.NFData

-- | Creates a 'SourceDescription' value with any optional fields omitted.
mkSourceDescription
    :: SourceDescription
mkSourceDescription
  = SourceDescription'{kinesisStreamSourceDescription = Core.Nothing}

-- | The 'KinesisStreamSourceDescription' value for the source Kinesis data stream.
--
-- /Note:/ Consider using 'kinesisStreamSourceDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdKinesisStreamSourceDescription :: Lens.Lens' SourceDescription (Core.Maybe Types.KinesisStreamSourceDescription)
sdKinesisStreamSourceDescription = Lens.field @"kinesisStreamSourceDescription"
{-# INLINEABLE sdKinesisStreamSourceDescription #-}
{-# DEPRECATED kinesisStreamSourceDescription "Use generic-lens or generic-optics with 'kinesisStreamSourceDescription' instead"  #-}

instance Core.FromJSON SourceDescription where
        parseJSON
          = Core.withObject "SourceDescription" Core.$
              \ x ->
                SourceDescription' Core.<$>
                  (x Core..:? "KinesisStreamSourceDescription")
