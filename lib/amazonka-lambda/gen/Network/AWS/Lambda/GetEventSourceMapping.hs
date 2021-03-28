{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.GetEventSourceMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details about an event source mapping. You can get the identifier of a mapping from the output of 'ListEventSourceMappings' .
module Network.AWS.Lambda.GetEventSourceMapping
    (
    -- * Creating a request
      GetEventSourceMapping (..)
    , mkGetEventSourceMapping
    -- ** Request lenses
    , gesmUUID

     -- * Destructuring the response
    , Types.EventSourceMappingConfiguration (..)
    , Types.mkEventSourceMappingConfiguration
    -- ** Response lenses
    , Types.esmcBatchSize
    , Types.esmcBisectBatchOnFunctionError
    , Types.esmcDestinationConfig
    , Types.esmcEventSourceArn
    , Types.esmcFunctionArn
    , Types.esmcLastModified
    , Types.esmcLastProcessingResult
    , Types.esmcMaximumBatchingWindowInSeconds
    , Types.esmcMaximumRecordAgeInSeconds
    , Types.esmcMaximumRetryAttempts
    , Types.esmcParallelizationFactor
    , Types.esmcQueues
    , Types.esmcSourceAccessConfigurations
    , Types.esmcStartingPosition
    , Types.esmcStartingPositionTimestamp
    , Types.esmcState
    , Types.esmcStateTransitionReason
    , Types.esmcTopics
    , Types.esmcUUID
    ) where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetEventSourceMapping' smart constructor.
newtype GetEventSourceMapping = GetEventSourceMapping'
  { uuid :: Core.Text
    -- ^ The identifier of the event source mapping.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetEventSourceMapping' value with any optional fields omitted.
mkGetEventSourceMapping
    :: Core.Text -- ^ 'uuid'
    -> GetEventSourceMapping
mkGetEventSourceMapping uuid = GetEventSourceMapping'{uuid}

-- | The identifier of the event source mapping.
--
-- /Note:/ Consider using 'uuid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gesmUUID :: Lens.Lens' GetEventSourceMapping Core.Text
gesmUUID = Lens.field @"uuid"
{-# INLINEABLE gesmUUID #-}
{-# DEPRECATED uuid "Use generic-lens or generic-optics with 'uuid' instead"  #-}

instance Core.ToQuery GetEventSourceMapping where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetEventSourceMapping where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetEventSourceMapping where
        type Rs GetEventSourceMapping =
             Types.EventSourceMappingConfiguration
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2015-03-31/event-source-mappings/" Core.<> Core.toText uuid,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
