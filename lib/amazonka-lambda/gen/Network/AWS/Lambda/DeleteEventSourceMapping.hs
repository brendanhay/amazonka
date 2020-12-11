{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.DeleteEventSourceMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an <https://docs.aws.amazon.com/lambda/latest/dg/intro-invocation-modes.html event source mapping> . You can get the identifier of a mapping from the output of 'ListEventSourceMappings' .
--
-- When you delete an event source mapping, it enters a @Deleting@ state and might not be completely deleted for several seconds.
module Network.AWS.Lambda.DeleteEventSourceMapping
  ( -- * Creating a request
    DeleteEventSourceMapping (..),
    mkDeleteEventSourceMapping,

    -- ** Request lenses
    desmUUId,

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

-- | /See:/ 'mkDeleteEventSourceMapping' smart constructor.
newtype DeleteEventSourceMapping = DeleteEventSourceMapping'
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

-- | Creates a value of 'DeleteEventSourceMapping' with the minimum fields required to make a request.
--
-- * 'uUId' - The identifier of the event source mapping.
mkDeleteEventSourceMapping ::
  -- | 'uUId'
  Lude.Text ->
  DeleteEventSourceMapping
mkDeleteEventSourceMapping pUUId_ =
  DeleteEventSourceMapping' {uUId = pUUId_}

-- | The identifier of the event source mapping.
--
-- /Note:/ Consider using 'uUId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desmUUId :: Lens.Lens' DeleteEventSourceMapping Lude.Text
desmUUId = Lens.lens (uUId :: DeleteEventSourceMapping -> Lude.Text) (\s a -> s {uUId = a} :: DeleteEventSourceMapping)
{-# DEPRECATED desmUUId "Use generic-lens or generic-optics with 'uUId' instead." #-}

instance Lude.AWSRequest DeleteEventSourceMapping where
  type Rs DeleteEventSourceMapping = EventSourceMappingConfiguration
  request = Req.delete lambdaService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders DeleteEventSourceMapping where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteEventSourceMapping where
  toPath DeleteEventSourceMapping' {..} =
    Lude.mconcat
      ["/2015-03-31/event-source-mappings/", Lude.toBS uUId]

instance Lude.ToQuery DeleteEventSourceMapping where
  toQuery = Lude.const Lude.mempty
