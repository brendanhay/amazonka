{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetEventSourceMapping (..),
    mkGetEventSourceMapping,

    -- ** Request lenses
    gesmUUId,

    -- * Destructuring the response
    EventSourceMappingConfiguration (..),
    mkEventSourceMappingConfiguration,

    -- ** Response lenses
    esmcEventSourceARN,
    esmcState,
    esmcStartingPositionTimestamp,
    esmcFunctionARN,
    esmcTopics,
    esmcQueues,
    esmcBisectBatchOnFunctionError,
    esmcUUId,
    esmcParallelizationFactor,
    esmcLastProcessingResult,
    esmcMaximumRetryAttempts,
    esmcBatchSize,
    esmcStateTransitionReason,
    esmcMaximumBatchingWindowInSeconds,
    esmcSourceAccessConfigurations,
    esmcMaximumRecordAgeInSeconds,
    esmcLastModified,
    esmcDestinationConfig,
    esmcStartingPosition,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetEventSourceMapping' smart constructor.
newtype GetEventSourceMapping = GetEventSourceMapping'
  { uUId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetEventSourceMapping' with the minimum fields required to make a request.
--
-- * 'uUId' - The identifier of the event source mapping.
mkGetEventSourceMapping ::
  -- | 'uUId'
  Lude.Text ->
  GetEventSourceMapping
mkGetEventSourceMapping pUUId_ =
  GetEventSourceMapping' {uUId = pUUId_}

-- | The identifier of the event source mapping.
--
-- /Note:/ Consider using 'uUId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gesmUUId :: Lens.Lens' GetEventSourceMapping Lude.Text
gesmUUId = Lens.lens (uUId :: GetEventSourceMapping -> Lude.Text) (\s a -> s {uUId = a} :: GetEventSourceMapping)
{-# DEPRECATED gesmUUId "Use generic-lens or generic-optics with 'uUId' instead." #-}

instance Lude.AWSRequest GetEventSourceMapping where
  type Rs GetEventSourceMapping = EventSourceMappingConfiguration
  request = Req.get lambdaService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders GetEventSourceMapping where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetEventSourceMapping where
  toPath GetEventSourceMapping' {..} =
    Lude.mconcat
      ["/2015-03-31/event-source-mappings/", Lude.toBS uUId]

instance Lude.ToQuery GetEventSourceMapping where
  toQuery = Lude.const Lude.mempty
