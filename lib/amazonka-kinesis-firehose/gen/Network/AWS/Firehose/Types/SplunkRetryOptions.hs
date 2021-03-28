{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.SplunkRetryOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.SplunkRetryOptions
  ( SplunkRetryOptions (..)
  -- * Smart constructor
  , mkSplunkRetryOptions
  -- * Lenses
  , sroDurationInSeconds
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configures retry behavior in case Kinesis Data Firehose is unable to deliver documents to Splunk, or if it doesn't receive an acknowledgment from Splunk.
--
-- /See:/ 'mkSplunkRetryOptions' smart constructor.
newtype SplunkRetryOptions = SplunkRetryOptions'
  { durationInSeconds :: Core.Maybe Core.Natural
    -- ^ The total amount of time that Kinesis Data Firehose spends on retries. This duration starts after the initial attempt to send data to Splunk fails. It doesn't include the periods during which Kinesis Data Firehose waits for acknowledgment from Splunk after each attempt.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SplunkRetryOptions' value with any optional fields omitted.
mkSplunkRetryOptions
    :: SplunkRetryOptions
mkSplunkRetryOptions
  = SplunkRetryOptions'{durationInSeconds = Core.Nothing}

-- | The total amount of time that Kinesis Data Firehose spends on retries. This duration starts after the initial attempt to send data to Splunk fails. It doesn't include the periods during which Kinesis Data Firehose waits for acknowledgment from Splunk after each attempt.
--
-- /Note:/ Consider using 'durationInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sroDurationInSeconds :: Lens.Lens' SplunkRetryOptions (Core.Maybe Core.Natural)
sroDurationInSeconds = Lens.field @"durationInSeconds"
{-# INLINEABLE sroDurationInSeconds #-}
{-# DEPRECATED durationInSeconds "Use generic-lens or generic-optics with 'durationInSeconds' instead"  #-}

instance Core.FromJSON SplunkRetryOptions where
        toJSON SplunkRetryOptions{..}
          = Core.object
              (Core.catMaybes
                 [("DurationInSeconds" Core..=) Core.<$> durationInSeconds])

instance Core.FromJSON SplunkRetryOptions where
        parseJSON
          = Core.withObject "SplunkRetryOptions" Core.$
              \ x ->
                SplunkRetryOptions' Core.<$> (x Core..:? "DurationInSeconds")
