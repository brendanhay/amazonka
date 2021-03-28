{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.SubscribeToShardEventStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Kinesis.Types.SubscribeToShardEventStream
  ( SubscribeToShardEventStream (..)
  -- * Smart constructor
  , mkSubscribeToShardEventStream
  -- * Lenses
  , stsesSubscribeToShardEvent
  , stsesInternalFailureException
  , stsesKMSAccessDeniedException
  , stsesKMSDisabledException
  , stsesKMSInvalidStateException
  , stsesKMSNotFoundException
  , stsesKMSOptInRequired
  , stsesKMSThrottlingException
  , stsesResourceInUseException
  , stsesResourceNotFoundException
  ) where

import qualified Network.AWS.Kinesis.Types.InternalFailureException as Types
import qualified Network.AWS.Kinesis.Types.KMSAccessDeniedException as Types
import qualified Network.AWS.Kinesis.Types.KMSDisabledException as Types
import qualified Network.AWS.Kinesis.Types.KMSInvalidStateException as Types
import qualified Network.AWS.Kinesis.Types.KMSNotFoundException as Types
import qualified Network.AWS.Kinesis.Types.KMSOptInRequired as Types
import qualified Network.AWS.Kinesis.Types.KMSThrottlingException as Types
import qualified Network.AWS.Kinesis.Types.ResourceInUseException as Types
import qualified Network.AWS.Kinesis.Types.ResourceNotFoundException as Types
import qualified Network.AWS.Kinesis.Types.SubscribeToShardEvent as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | This is a tagged union for all of the types of events an enhanced fan-out consumer can receive over HTTP/2 after a call to 'SubscribeToShard' .
--
-- /See:/ 'mkSubscribeToShardEventStream' smart constructor.
data SubscribeToShardEventStream = SubscribeToShardEventStream'
  { subscribeToShardEvent :: Types.SubscribeToShardEvent
    -- ^ After you call 'SubscribeToShard' , Kinesis Data Streams sends events of this type to your consumer. For an example of how to handle these events, see </streams/latest/dev/building-enhanced-consumers-api.html Enhanced Fan-Out Using the Kinesis Data Streams API> .
  , internalFailureException :: Core.Maybe Types.InternalFailureException
    -- ^ The processing of the request failed because of an unknown error, exception, or failure.
  , kMSAccessDeniedException :: Core.Maybe Types.KMSAccessDeniedException
  , kMSDisabledException :: Core.Maybe Types.KMSDisabledException
  , kMSInvalidStateException :: Core.Maybe Types.KMSInvalidStateException
  , kMSNotFoundException :: Core.Maybe Types.KMSNotFoundException
  , kMSOptInRequired :: Core.Maybe Types.KMSOptInRequired
  , kMSThrottlingException :: Core.Maybe Types.KMSThrottlingException
  , resourceInUseException :: Core.Maybe Types.ResourceInUseException
  , resourceNotFoundException :: Core.Maybe Types.ResourceNotFoundException
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'SubscribeToShardEventStream' value with any optional fields omitted.
mkSubscribeToShardEventStream
    :: Types.SubscribeToShardEvent -- ^ 'subscribeToShardEvent'
    -> SubscribeToShardEventStream
mkSubscribeToShardEventStream subscribeToShardEvent
  = SubscribeToShardEventStream'{subscribeToShardEvent,
                                 internalFailureException = Core.Nothing,
                                 kMSAccessDeniedException = Core.Nothing,
                                 kMSDisabledException = Core.Nothing,
                                 kMSInvalidStateException = Core.Nothing,
                                 kMSNotFoundException = Core.Nothing,
                                 kMSOptInRequired = Core.Nothing,
                                 kMSThrottlingException = Core.Nothing,
                                 resourceInUseException = Core.Nothing,
                                 resourceNotFoundException = Core.Nothing}

-- | After you call 'SubscribeToShard' , Kinesis Data Streams sends events of this type to your consumer. For an example of how to handle these events, see </streams/latest/dev/building-enhanced-consumers-api.html Enhanced Fan-Out Using the Kinesis Data Streams API> .
--
-- /Note:/ Consider using 'subscribeToShardEvent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsesSubscribeToShardEvent :: Lens.Lens' SubscribeToShardEventStream Types.SubscribeToShardEvent
stsesSubscribeToShardEvent = Lens.field @"subscribeToShardEvent"
{-# INLINEABLE stsesSubscribeToShardEvent #-}
{-# DEPRECATED subscribeToShardEvent "Use generic-lens or generic-optics with 'subscribeToShardEvent' instead"  #-}

-- | The processing of the request failed because of an unknown error, exception, or failure.
--
-- /Note:/ Consider using 'internalFailureException' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsesInternalFailureException :: Lens.Lens' SubscribeToShardEventStream (Core.Maybe Types.InternalFailureException)
stsesInternalFailureException = Lens.field @"internalFailureException"
{-# INLINEABLE stsesInternalFailureException #-}
{-# DEPRECATED internalFailureException "Use generic-lens or generic-optics with 'internalFailureException' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'kMSAccessDeniedException' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsesKMSAccessDeniedException :: Lens.Lens' SubscribeToShardEventStream (Core.Maybe Types.KMSAccessDeniedException)
stsesKMSAccessDeniedException = Lens.field @"kMSAccessDeniedException"
{-# INLINEABLE stsesKMSAccessDeniedException #-}
{-# DEPRECATED kMSAccessDeniedException "Use generic-lens or generic-optics with 'kMSAccessDeniedException' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'kMSDisabledException' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsesKMSDisabledException :: Lens.Lens' SubscribeToShardEventStream (Core.Maybe Types.KMSDisabledException)
stsesKMSDisabledException = Lens.field @"kMSDisabledException"
{-# INLINEABLE stsesKMSDisabledException #-}
{-# DEPRECATED kMSDisabledException "Use generic-lens or generic-optics with 'kMSDisabledException' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'kMSInvalidStateException' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsesKMSInvalidStateException :: Lens.Lens' SubscribeToShardEventStream (Core.Maybe Types.KMSInvalidStateException)
stsesKMSInvalidStateException = Lens.field @"kMSInvalidStateException"
{-# INLINEABLE stsesKMSInvalidStateException #-}
{-# DEPRECATED kMSInvalidStateException "Use generic-lens or generic-optics with 'kMSInvalidStateException' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'kMSNotFoundException' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsesKMSNotFoundException :: Lens.Lens' SubscribeToShardEventStream (Core.Maybe Types.KMSNotFoundException)
stsesKMSNotFoundException = Lens.field @"kMSNotFoundException"
{-# INLINEABLE stsesKMSNotFoundException #-}
{-# DEPRECATED kMSNotFoundException "Use generic-lens or generic-optics with 'kMSNotFoundException' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'kMSOptInRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsesKMSOptInRequired :: Lens.Lens' SubscribeToShardEventStream (Core.Maybe Types.KMSOptInRequired)
stsesKMSOptInRequired = Lens.field @"kMSOptInRequired"
{-# INLINEABLE stsesKMSOptInRequired #-}
{-# DEPRECATED kMSOptInRequired "Use generic-lens or generic-optics with 'kMSOptInRequired' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'kMSThrottlingException' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsesKMSThrottlingException :: Lens.Lens' SubscribeToShardEventStream (Core.Maybe Types.KMSThrottlingException)
stsesKMSThrottlingException = Lens.field @"kMSThrottlingException"
{-# INLINEABLE stsesKMSThrottlingException #-}
{-# DEPRECATED kMSThrottlingException "Use generic-lens or generic-optics with 'kMSThrottlingException' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'resourceInUseException' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsesResourceInUseException :: Lens.Lens' SubscribeToShardEventStream (Core.Maybe Types.ResourceInUseException)
stsesResourceInUseException = Lens.field @"resourceInUseException"
{-# INLINEABLE stsesResourceInUseException #-}
{-# DEPRECATED resourceInUseException "Use generic-lens or generic-optics with 'resourceInUseException' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'resourceNotFoundException' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsesResourceNotFoundException :: Lens.Lens' SubscribeToShardEventStream (Core.Maybe Types.ResourceNotFoundException)
stsesResourceNotFoundException = Lens.field @"resourceNotFoundException"
{-# INLINEABLE stsesResourceNotFoundException #-}
{-# DEPRECATED resourceNotFoundException "Use generic-lens or generic-optics with 'resourceNotFoundException' instead"  #-}

instance Core.FromJSON SubscribeToShardEventStream where
        parseJSON
          = Core.withObject "SubscribeToShardEventStream" Core.$
              \ x ->
                SubscribeToShardEventStream' Core.<$>
                  (x Core..: "SubscribeToShardEvent") Core.<*>
                    x Core..:? "InternalFailureException"
                    Core.<*> x Core..:? "KMSAccessDeniedException"
                    Core.<*> x Core..:? "KMSDisabledException"
                    Core.<*> x Core..:? "KMSInvalidStateException"
                    Core.<*> x Core..:? "KMSNotFoundException"
                    Core.<*> x Core..:? "KMSOptInRequired"
                    Core.<*> x Core..:? "KMSThrottlingException"
                    Core.<*> x Core..:? "ResourceInUseException"
                    Core.<*> x Core..:? "ResourceNotFoundException"
