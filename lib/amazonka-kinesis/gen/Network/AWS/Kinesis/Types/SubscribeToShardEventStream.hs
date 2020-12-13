{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.SubscribeToShardEventStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.SubscribeToShardEventStream
  ( SubscribeToShardEventStream (..),

    -- * Smart constructor
    mkSubscribeToShardEventStream,

    -- * Lenses
    stsesKMSInvalidStateException,
    stsesKMSThrottlingException,
    stsesKMSOptInRequired,
    stsesKMSNotFoundException,
    stsesKMSDisabledException,
    stsesInternalFailureException,
    stsesSubscribeToShardEvent,
    stsesResourceNotFoundException,
    stsesKMSAccessDeniedException,
    stsesResourceInUseException,
  )
where

import Network.AWS.Kinesis.Types.InternalFailureException
import Network.AWS.Kinesis.Types.KMSAccessDeniedException
import Network.AWS.Kinesis.Types.KMSDisabledException
import Network.AWS.Kinesis.Types.KMSInvalidStateException
import Network.AWS.Kinesis.Types.KMSNotFoundException
import Network.AWS.Kinesis.Types.KMSOptInRequired
import Network.AWS.Kinesis.Types.KMSThrottlingException
import Network.AWS.Kinesis.Types.ResourceInUseException
import Network.AWS.Kinesis.Types.ResourceNotFoundException
import Network.AWS.Kinesis.Types.SubscribeToShardEvent
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | This is a tagged union for all of the types of events an enhanced fan-out consumer can receive over HTTP/2 after a call to 'SubscribeToShard' .
--
-- /See:/ 'mkSubscribeToShardEventStream' smart constructor.
data SubscribeToShardEventStream = SubscribeToShardEventStream'
  { kmsInvalidStateException :: Lude.Maybe KMSInvalidStateException,
    kmsThrottlingException :: Lude.Maybe KMSThrottlingException,
    kmsOptInRequired :: Lude.Maybe KMSOptInRequired,
    kmsNotFoundException :: Lude.Maybe KMSNotFoundException,
    kmsDisabledException :: Lude.Maybe KMSDisabledException,
    -- | The processing of the request failed because of an unknown error, exception, or failure.
    internalFailureException :: Lude.Maybe InternalFailureException,
    -- | After you call 'SubscribeToShard' , Kinesis Data Streams sends events of this type to your consumer. For an example of how to handle these events, see </streams/latest/dev/building-enhanced-consumers-api.html Enhanced Fan-Out Using the Kinesis Data Streams API> .
    subscribeToShardEvent :: SubscribeToShardEvent,
    resourceNotFoundException :: Lude.Maybe ResourceNotFoundException,
    kmsAccessDeniedException :: Lude.Maybe KMSAccessDeniedException,
    resourceInUseException :: Lude.Maybe ResourceInUseException
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SubscribeToShardEventStream' with the minimum fields required to make a request.
--
-- * 'kmsInvalidStateException' -
-- * 'kmsThrottlingException' -
-- * 'kmsOptInRequired' -
-- * 'kmsNotFoundException' -
-- * 'kmsDisabledException' -
-- * 'internalFailureException' - The processing of the request failed because of an unknown error, exception, or failure.
-- * 'subscribeToShardEvent' - After you call 'SubscribeToShard' , Kinesis Data Streams sends events of this type to your consumer. For an example of how to handle these events, see </streams/latest/dev/building-enhanced-consumers-api.html Enhanced Fan-Out Using the Kinesis Data Streams API> .
-- * 'resourceNotFoundException' -
-- * 'kmsAccessDeniedException' -
-- * 'resourceInUseException' -
mkSubscribeToShardEventStream ::
  -- | 'subscribeToShardEvent'
  SubscribeToShardEvent ->
  SubscribeToShardEventStream
mkSubscribeToShardEventStream pSubscribeToShardEvent_ =
  SubscribeToShardEventStream'
    { kmsInvalidStateException =
        Lude.Nothing,
      kmsThrottlingException = Lude.Nothing,
      kmsOptInRequired = Lude.Nothing,
      kmsNotFoundException = Lude.Nothing,
      kmsDisabledException = Lude.Nothing,
      internalFailureException = Lude.Nothing,
      subscribeToShardEvent = pSubscribeToShardEvent_,
      resourceNotFoundException = Lude.Nothing,
      kmsAccessDeniedException = Lude.Nothing,
      resourceInUseException = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'kmsInvalidStateException' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsesKMSInvalidStateException :: Lens.Lens' SubscribeToShardEventStream (Lude.Maybe KMSInvalidStateException)
stsesKMSInvalidStateException = Lens.lens (kmsInvalidStateException :: SubscribeToShardEventStream -> Lude.Maybe KMSInvalidStateException) (\s a -> s {kmsInvalidStateException = a} :: SubscribeToShardEventStream)
{-# DEPRECATED stsesKMSInvalidStateException "Use generic-lens or generic-optics with 'kmsInvalidStateException' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'kmsThrottlingException' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsesKMSThrottlingException :: Lens.Lens' SubscribeToShardEventStream (Lude.Maybe KMSThrottlingException)
stsesKMSThrottlingException = Lens.lens (kmsThrottlingException :: SubscribeToShardEventStream -> Lude.Maybe KMSThrottlingException) (\s a -> s {kmsThrottlingException = a} :: SubscribeToShardEventStream)
{-# DEPRECATED stsesKMSThrottlingException "Use generic-lens or generic-optics with 'kmsThrottlingException' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'kmsOptInRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsesKMSOptInRequired :: Lens.Lens' SubscribeToShardEventStream (Lude.Maybe KMSOptInRequired)
stsesKMSOptInRequired = Lens.lens (kmsOptInRequired :: SubscribeToShardEventStream -> Lude.Maybe KMSOptInRequired) (\s a -> s {kmsOptInRequired = a} :: SubscribeToShardEventStream)
{-# DEPRECATED stsesKMSOptInRequired "Use generic-lens or generic-optics with 'kmsOptInRequired' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'kmsNotFoundException' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsesKMSNotFoundException :: Lens.Lens' SubscribeToShardEventStream (Lude.Maybe KMSNotFoundException)
stsesKMSNotFoundException = Lens.lens (kmsNotFoundException :: SubscribeToShardEventStream -> Lude.Maybe KMSNotFoundException) (\s a -> s {kmsNotFoundException = a} :: SubscribeToShardEventStream)
{-# DEPRECATED stsesKMSNotFoundException "Use generic-lens or generic-optics with 'kmsNotFoundException' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'kmsDisabledException' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsesKMSDisabledException :: Lens.Lens' SubscribeToShardEventStream (Lude.Maybe KMSDisabledException)
stsesKMSDisabledException = Lens.lens (kmsDisabledException :: SubscribeToShardEventStream -> Lude.Maybe KMSDisabledException) (\s a -> s {kmsDisabledException = a} :: SubscribeToShardEventStream)
{-# DEPRECATED stsesKMSDisabledException "Use generic-lens or generic-optics with 'kmsDisabledException' instead." #-}

-- | The processing of the request failed because of an unknown error, exception, or failure.
--
-- /Note:/ Consider using 'internalFailureException' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsesInternalFailureException :: Lens.Lens' SubscribeToShardEventStream (Lude.Maybe InternalFailureException)
stsesInternalFailureException = Lens.lens (internalFailureException :: SubscribeToShardEventStream -> Lude.Maybe InternalFailureException) (\s a -> s {internalFailureException = a} :: SubscribeToShardEventStream)
{-# DEPRECATED stsesInternalFailureException "Use generic-lens or generic-optics with 'internalFailureException' instead." #-}

-- | After you call 'SubscribeToShard' , Kinesis Data Streams sends events of this type to your consumer. For an example of how to handle these events, see </streams/latest/dev/building-enhanced-consumers-api.html Enhanced Fan-Out Using the Kinesis Data Streams API> .
--
-- /Note:/ Consider using 'subscribeToShardEvent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsesSubscribeToShardEvent :: Lens.Lens' SubscribeToShardEventStream SubscribeToShardEvent
stsesSubscribeToShardEvent = Lens.lens (subscribeToShardEvent :: SubscribeToShardEventStream -> SubscribeToShardEvent) (\s a -> s {subscribeToShardEvent = a} :: SubscribeToShardEventStream)
{-# DEPRECATED stsesSubscribeToShardEvent "Use generic-lens or generic-optics with 'subscribeToShardEvent' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'resourceNotFoundException' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsesResourceNotFoundException :: Lens.Lens' SubscribeToShardEventStream (Lude.Maybe ResourceNotFoundException)
stsesResourceNotFoundException = Lens.lens (resourceNotFoundException :: SubscribeToShardEventStream -> Lude.Maybe ResourceNotFoundException) (\s a -> s {resourceNotFoundException = a} :: SubscribeToShardEventStream)
{-# DEPRECATED stsesResourceNotFoundException "Use generic-lens or generic-optics with 'resourceNotFoundException' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'kmsAccessDeniedException' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsesKMSAccessDeniedException :: Lens.Lens' SubscribeToShardEventStream (Lude.Maybe KMSAccessDeniedException)
stsesKMSAccessDeniedException = Lens.lens (kmsAccessDeniedException :: SubscribeToShardEventStream -> Lude.Maybe KMSAccessDeniedException) (\s a -> s {kmsAccessDeniedException = a} :: SubscribeToShardEventStream)
{-# DEPRECATED stsesKMSAccessDeniedException "Use generic-lens or generic-optics with 'kmsAccessDeniedException' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'resourceInUseException' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsesResourceInUseException :: Lens.Lens' SubscribeToShardEventStream (Lude.Maybe ResourceInUseException)
stsesResourceInUseException = Lens.lens (resourceInUseException :: SubscribeToShardEventStream -> Lude.Maybe ResourceInUseException) (\s a -> s {resourceInUseException = a} :: SubscribeToShardEventStream)
{-# DEPRECATED stsesResourceInUseException "Use generic-lens or generic-optics with 'resourceInUseException' instead." #-}

instance Lude.FromJSON SubscribeToShardEventStream where
  parseJSON =
    Lude.withObject
      "SubscribeToShardEventStream"
      ( \x ->
          SubscribeToShardEventStream'
            Lude.<$> (x Lude..:? "KMSInvalidStateException")
            Lude.<*> (x Lude..:? "KMSThrottlingException")
            Lude.<*> (x Lude..:? "KMSOptInRequired")
            Lude.<*> (x Lude..:? "KMSNotFoundException")
            Lude.<*> (x Lude..:? "KMSDisabledException")
            Lude.<*> (x Lude..:? "InternalFailureException")
            Lude.<*> (x Lude..: "SubscribeToShardEvent")
            Lude.<*> (x Lude..:? "ResourceNotFoundException")
            Lude.<*> (x Lude..:? "KMSAccessDeniedException")
            Lude.<*> (x Lude..:? "ResourceInUseException")
      )
