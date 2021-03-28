{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.RedshiftRetryOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.RedshiftRetryOptions
  ( RedshiftRetryOptions (..)
  -- * Smart constructor
  , mkRedshiftRetryOptions
  -- * Lenses
  , rroDurationInSeconds
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configures retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon Redshift.
--
-- /See:/ 'mkRedshiftRetryOptions' smart constructor.
newtype RedshiftRetryOptions = RedshiftRetryOptions'
  { durationInSeconds :: Core.Maybe Core.Natural
    -- ^ The length of time during which Kinesis Data Firehose retries delivery after a failure, starting from the initial request and including the first attempt. The default value is 3600 seconds (60 minutes). Kinesis Data Firehose does not retry if the value of @DurationInSeconds@ is 0 (zero) or if the first delivery attempt takes longer than the current value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RedshiftRetryOptions' value with any optional fields omitted.
mkRedshiftRetryOptions
    :: RedshiftRetryOptions
mkRedshiftRetryOptions
  = RedshiftRetryOptions'{durationInSeconds = Core.Nothing}

-- | The length of time during which Kinesis Data Firehose retries delivery after a failure, starting from the initial request and including the first attempt. The default value is 3600 seconds (60 minutes). Kinesis Data Firehose does not retry if the value of @DurationInSeconds@ is 0 (zero) or if the first delivery attempt takes longer than the current value.
--
-- /Note:/ Consider using 'durationInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rroDurationInSeconds :: Lens.Lens' RedshiftRetryOptions (Core.Maybe Core.Natural)
rroDurationInSeconds = Lens.field @"durationInSeconds"
{-# INLINEABLE rroDurationInSeconds #-}
{-# DEPRECATED durationInSeconds "Use generic-lens or generic-optics with 'durationInSeconds' instead"  #-}

instance Core.FromJSON RedshiftRetryOptions where
        toJSON RedshiftRetryOptions{..}
          = Core.object
              (Core.catMaybes
                 [("DurationInSeconds" Core..=) Core.<$> durationInSeconds])

instance Core.FromJSON RedshiftRetryOptions where
        parseJSON
          = Core.withObject "RedshiftRetryOptions" Core.$
              \ x ->
                RedshiftRetryOptions' Core.<$> (x Core..:? "DurationInSeconds")
