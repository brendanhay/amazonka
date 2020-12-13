{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ElasticsearchRetryOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ElasticsearchRetryOptions
  ( ElasticsearchRetryOptions (..),

    -- * Smart constructor
    mkElasticsearchRetryOptions,

    -- * Lenses
    eroDurationInSeconds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configures retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon ES.
--
-- /See:/ 'mkElasticsearchRetryOptions' smart constructor.
newtype ElasticsearchRetryOptions = ElasticsearchRetryOptions'
  { -- | After an initial failure to deliver to Amazon ES, the total amount of time during which Kinesis Data Firehose retries delivery (including the first attempt). After this time has elapsed, the failed documents are written to Amazon S3. Default value is 300 seconds (5 minutes). A value of 0 (zero) results in no retries.
    durationInSeconds :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ElasticsearchRetryOptions' with the minimum fields required to make a request.
--
-- * 'durationInSeconds' - After an initial failure to deliver to Amazon ES, the total amount of time during which Kinesis Data Firehose retries delivery (including the first attempt). After this time has elapsed, the failed documents are written to Amazon S3. Default value is 300 seconds (5 minutes). A value of 0 (zero) results in no retries.
mkElasticsearchRetryOptions ::
  ElasticsearchRetryOptions
mkElasticsearchRetryOptions =
  ElasticsearchRetryOptions' {durationInSeconds = Lude.Nothing}

-- | After an initial failure to deliver to Amazon ES, the total amount of time during which Kinesis Data Firehose retries delivery (including the first attempt). After this time has elapsed, the failed documents are written to Amazon S3. Default value is 300 seconds (5 minutes). A value of 0 (zero) results in no retries.
--
-- /Note:/ Consider using 'durationInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eroDurationInSeconds :: Lens.Lens' ElasticsearchRetryOptions (Lude.Maybe Lude.Natural)
eroDurationInSeconds = Lens.lens (durationInSeconds :: ElasticsearchRetryOptions -> Lude.Maybe Lude.Natural) (\s a -> s {durationInSeconds = a} :: ElasticsearchRetryOptions)
{-# DEPRECATED eroDurationInSeconds "Use generic-lens or generic-optics with 'durationInSeconds' instead." #-}

instance Lude.FromJSON ElasticsearchRetryOptions where
  parseJSON =
    Lude.withObject
      "ElasticsearchRetryOptions"
      ( \x ->
          ElasticsearchRetryOptions'
            Lude.<$> (x Lude..:? "DurationInSeconds")
      )

instance Lude.ToJSON ElasticsearchRetryOptions where
  toJSON ElasticsearchRetryOptions' {..} =
    Lude.object
      ( Lude.catMaybes
          [("DurationInSeconds" Lude..=) Lude.<$> durationInSeconds]
      )
