{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ElasticsearchRetryOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.ElasticsearchRetryOptions
  ( ElasticsearchRetryOptions (..)
  -- * Smart constructor
  , mkElasticsearchRetryOptions
  -- * Lenses
  , eroDurationInSeconds
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configures retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon ES.
--
-- /See:/ 'mkElasticsearchRetryOptions' smart constructor.
newtype ElasticsearchRetryOptions = ElasticsearchRetryOptions'
  { durationInSeconds :: Core.Maybe Core.Natural
    -- ^ After an initial failure to deliver to Amazon ES, the total amount of time during which Kinesis Data Firehose retries delivery (including the first attempt). After this time has elapsed, the failed documents are written to Amazon S3. Default value is 300 seconds (5 minutes). A value of 0 (zero) results in no retries.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ElasticsearchRetryOptions' value with any optional fields omitted.
mkElasticsearchRetryOptions
    :: ElasticsearchRetryOptions
mkElasticsearchRetryOptions
  = ElasticsearchRetryOptions'{durationInSeconds = Core.Nothing}

-- | After an initial failure to deliver to Amazon ES, the total amount of time during which Kinesis Data Firehose retries delivery (including the first attempt). After this time has elapsed, the failed documents are written to Amazon S3. Default value is 300 seconds (5 minutes). A value of 0 (zero) results in no retries.
--
-- /Note:/ Consider using 'durationInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eroDurationInSeconds :: Lens.Lens' ElasticsearchRetryOptions (Core.Maybe Core.Natural)
eroDurationInSeconds = Lens.field @"durationInSeconds"
{-# INLINEABLE eroDurationInSeconds #-}
{-# DEPRECATED durationInSeconds "Use generic-lens or generic-optics with 'durationInSeconds' instead"  #-}

instance Core.FromJSON ElasticsearchRetryOptions where
        toJSON ElasticsearchRetryOptions{..}
          = Core.object
              (Core.catMaybes
                 [("DurationInSeconds" Core..=) Core.<$> durationInSeconds])

instance Core.FromJSON ElasticsearchRetryOptions where
        parseJSON
          = Core.withObject "ElasticsearchRetryOptions" Core.$
              \ x ->
                ElasticsearchRetryOptions' Core.<$>
                  (x Core..:? "DurationInSeconds")
