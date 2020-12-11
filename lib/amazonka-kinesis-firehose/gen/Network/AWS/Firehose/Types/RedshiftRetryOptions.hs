-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.RedshiftRetryOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.RedshiftRetryOptions
  ( RedshiftRetryOptions (..),

    -- * Smart constructor
    mkRedshiftRetryOptions,

    -- * Lenses
    rroDurationInSeconds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configures retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon Redshift.
--
-- /See:/ 'mkRedshiftRetryOptions' smart constructor.
newtype RedshiftRetryOptions = RedshiftRetryOptions'
  { durationInSeconds ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RedshiftRetryOptions' with the minimum fields required to make a request.
--
-- * 'durationInSeconds' - The length of time during which Kinesis Data Firehose retries delivery after a failure, starting from the initial request and including the first attempt. The default value is 3600 seconds (60 minutes). Kinesis Data Firehose does not retry if the value of @DurationInSeconds@ is 0 (zero) or if the first delivery attempt takes longer than the current value.
mkRedshiftRetryOptions ::
  RedshiftRetryOptions
mkRedshiftRetryOptions =
  RedshiftRetryOptions' {durationInSeconds = Lude.Nothing}

-- | The length of time during which Kinesis Data Firehose retries delivery after a failure, starting from the initial request and including the first attempt. The default value is 3600 seconds (60 minutes). Kinesis Data Firehose does not retry if the value of @DurationInSeconds@ is 0 (zero) or if the first delivery attempt takes longer than the current value.
--
-- /Note:/ Consider using 'durationInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rroDurationInSeconds :: Lens.Lens' RedshiftRetryOptions (Lude.Maybe Lude.Natural)
rroDurationInSeconds = Lens.lens (durationInSeconds :: RedshiftRetryOptions -> Lude.Maybe Lude.Natural) (\s a -> s {durationInSeconds = a} :: RedshiftRetryOptions)
{-# DEPRECATED rroDurationInSeconds "Use generic-lens or generic-optics with 'durationInSeconds' instead." #-}

instance Lude.FromJSON RedshiftRetryOptions where
  parseJSON =
    Lude.withObject
      "RedshiftRetryOptions"
      ( \x ->
          RedshiftRetryOptions' Lude.<$> (x Lude..:? "DurationInSeconds")
      )

instance Lude.ToJSON RedshiftRetryOptions where
  toJSON RedshiftRetryOptions' {..} =
    Lude.object
      ( Lude.catMaybes
          [("DurationInSeconds" Lude..=) Lude.<$> durationInSeconds]
      )
