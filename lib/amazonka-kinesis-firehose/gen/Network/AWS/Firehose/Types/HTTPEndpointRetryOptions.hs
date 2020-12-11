-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.HTTPEndpointRetryOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.HTTPEndpointRetryOptions
  ( HTTPEndpointRetryOptions (..),

    -- * Smart constructor
    mkHTTPEndpointRetryOptions,

    -- * Lenses
    httperoDurationInSeconds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the retry behavior in case Kinesis Data Firehose is unable to deliver data to the specified HTTP endpoint destination, or if it doesn't receive a valid acknowledgment of receipt from the specified HTTP endpoint destination.
--
-- /See:/ 'mkHTTPEndpointRetryOptions' smart constructor.
newtype HTTPEndpointRetryOptions = HTTPEndpointRetryOptions'
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

-- | Creates a value of 'HTTPEndpointRetryOptions' with the minimum fields required to make a request.
--
-- * 'durationInSeconds' - The total amount of time that Kinesis Data Firehose spends on retries. This duration starts after the initial attempt to send data to the custom destination via HTTPS endpoint fails. It doesn't include the periods during which Kinesis Data Firehose waits for acknowledgment from the specified destination after each attempt.
mkHTTPEndpointRetryOptions ::
  HTTPEndpointRetryOptions
mkHTTPEndpointRetryOptions =
  HTTPEndpointRetryOptions' {durationInSeconds = Lude.Nothing}

-- | The total amount of time that Kinesis Data Firehose spends on retries. This duration starts after the initial attempt to send data to the custom destination via HTTPS endpoint fails. It doesn't include the periods during which Kinesis Data Firehose waits for acknowledgment from the specified destination after each attempt.
--
-- /Note:/ Consider using 'durationInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httperoDurationInSeconds :: Lens.Lens' HTTPEndpointRetryOptions (Lude.Maybe Lude.Natural)
httperoDurationInSeconds = Lens.lens (durationInSeconds :: HTTPEndpointRetryOptions -> Lude.Maybe Lude.Natural) (\s a -> s {durationInSeconds = a} :: HTTPEndpointRetryOptions)
{-# DEPRECATED httperoDurationInSeconds "Use generic-lens or generic-optics with 'durationInSeconds' instead." #-}

instance Lude.FromJSON HTTPEndpointRetryOptions where
  parseJSON =
    Lude.withObject
      "HTTPEndpointRetryOptions"
      ( \x ->
          HTTPEndpointRetryOptions'
            Lude.<$> (x Lude..:? "DurationInSeconds")
      )

instance Lude.ToJSON HTTPEndpointRetryOptions where
  toJSON HTTPEndpointRetryOptions' {..} =
    Lude.object
      ( Lude.catMaybes
          [("DurationInSeconds" Lude..=) Lude.<$> durationInSeconds]
      )
