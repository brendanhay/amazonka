-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.SplunkRetryOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.SplunkRetryOptions
  ( SplunkRetryOptions (..),

    -- * Smart constructor
    mkSplunkRetryOptions,

    -- * Lenses
    sroDurationInSeconds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configures retry behavior in case Kinesis Data Firehose is unable to deliver documents to Splunk, or if it doesn't receive an acknowledgment from Splunk.
--
-- /See:/ 'mkSplunkRetryOptions' smart constructor.
newtype SplunkRetryOptions = SplunkRetryOptions'
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

-- | Creates a value of 'SplunkRetryOptions' with the minimum fields required to make a request.
--
-- * 'durationInSeconds' - The total amount of time that Kinesis Data Firehose spends on retries. This duration starts after the initial attempt to send data to Splunk fails. It doesn't include the periods during which Kinesis Data Firehose waits for acknowledgment from Splunk after each attempt.
mkSplunkRetryOptions ::
  SplunkRetryOptions
mkSplunkRetryOptions =
  SplunkRetryOptions' {durationInSeconds = Lude.Nothing}

-- | The total amount of time that Kinesis Data Firehose spends on retries. This duration starts after the initial attempt to send data to Splunk fails. It doesn't include the periods during which Kinesis Data Firehose waits for acknowledgment from Splunk after each attempt.
--
-- /Note:/ Consider using 'durationInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sroDurationInSeconds :: Lens.Lens' SplunkRetryOptions (Lude.Maybe Lude.Natural)
sroDurationInSeconds = Lens.lens (durationInSeconds :: SplunkRetryOptions -> Lude.Maybe Lude.Natural) (\s a -> s {durationInSeconds = a} :: SplunkRetryOptions)
{-# DEPRECATED sroDurationInSeconds "Use generic-lens or generic-optics with 'durationInSeconds' instead." #-}

instance Lude.FromJSON SplunkRetryOptions where
  parseJSON =
    Lude.withObject
      "SplunkRetryOptions"
      ( \x ->
          SplunkRetryOptions' Lude.<$> (x Lude..:? "DurationInSeconds")
      )

instance Lude.ToJSON SplunkRetryOptions where
  toJSON SplunkRetryOptions' {..} =
    Lude.object
      ( Lude.catMaybes
          [("DurationInSeconds" Lude..=) Lude.<$> durationInSeconds]
      )
