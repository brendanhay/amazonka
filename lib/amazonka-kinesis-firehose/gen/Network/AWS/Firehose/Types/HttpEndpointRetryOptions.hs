{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.HttpEndpointRetryOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.HttpEndpointRetryOptions
  ( HttpEndpointRetryOptions (..)
  -- * Smart constructor
  , mkHttpEndpointRetryOptions
  -- * Lenses
  , heroDurationInSeconds
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the retry behavior in case Kinesis Data Firehose is unable to deliver data to the specified HTTP endpoint destination, or if it doesn't receive a valid acknowledgment of receipt from the specified HTTP endpoint destination.
--
-- /See:/ 'mkHttpEndpointRetryOptions' smart constructor.
newtype HttpEndpointRetryOptions = HttpEndpointRetryOptions'
  { durationInSeconds :: Core.Maybe Core.Natural
    -- ^ The total amount of time that Kinesis Data Firehose spends on retries. This duration starts after the initial attempt to send data to the custom destination via HTTPS endpoint fails. It doesn't include the periods during which Kinesis Data Firehose waits for acknowledgment from the specified destination after each attempt. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'HttpEndpointRetryOptions' value with any optional fields omitted.
mkHttpEndpointRetryOptions
    :: HttpEndpointRetryOptions
mkHttpEndpointRetryOptions
  = HttpEndpointRetryOptions'{durationInSeconds = Core.Nothing}

-- | The total amount of time that Kinesis Data Firehose spends on retries. This duration starts after the initial attempt to send data to the custom destination via HTTPS endpoint fails. It doesn't include the periods during which Kinesis Data Firehose waits for acknowledgment from the specified destination after each attempt. 
--
-- /Note:/ Consider using 'durationInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heroDurationInSeconds :: Lens.Lens' HttpEndpointRetryOptions (Core.Maybe Core.Natural)
heroDurationInSeconds = Lens.field @"durationInSeconds"
{-# INLINEABLE heroDurationInSeconds #-}
{-# DEPRECATED durationInSeconds "Use generic-lens or generic-optics with 'durationInSeconds' instead"  #-}

instance Core.FromJSON HttpEndpointRetryOptions where
        toJSON HttpEndpointRetryOptions{..}
          = Core.object
              (Core.catMaybes
                 [("DurationInSeconds" Core..=) Core.<$> durationInSeconds])

instance Core.FromJSON HttpEndpointRetryOptions where
        parseJSON
          = Core.withObject "HttpEndpointRetryOptions" Core.$
              \ x ->
                HttpEndpointRetryOptions' Core.<$> (x Core..:? "DurationInSeconds")
