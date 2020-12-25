{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.StopNotebookInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Terminates the ML compute instance. Before terminating the instance, Amazon SageMaker disconnects the ML storage volume from it. Amazon SageMaker preserves the ML storage volume. Amazon SageMaker stops charging you for the ML compute instance when you call @StopNotebookInstance@ .
--
-- To access data on the ML storage volume for a notebook instance that has been terminated, call the @StartNotebookInstance@ API. @StartNotebookInstance@ launches another ML compute instance, configures it, and attaches the preserved ML storage volume so you can continue your work.
module Network.AWS.SageMaker.StopNotebookInstance
  ( -- * Creating a request
    StopNotebookInstance (..),
    mkStopNotebookInstance,

    -- ** Request lenses
    sniNotebookInstanceName,

    -- * Destructuring the response
    StopNotebookInstanceResponse (..),
    mkStopNotebookInstanceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkStopNotebookInstance' smart constructor.
newtype StopNotebookInstance = StopNotebookInstance'
  { -- | The name of the notebook instance to terminate.
    notebookInstanceName :: Types.NotebookInstanceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopNotebookInstance' value with any optional fields omitted.
mkStopNotebookInstance ::
  -- | 'notebookInstanceName'
  Types.NotebookInstanceName ->
  StopNotebookInstance
mkStopNotebookInstance notebookInstanceName =
  StopNotebookInstance' {notebookInstanceName}

-- | The name of the notebook instance to terminate.
--
-- /Note:/ Consider using 'notebookInstanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sniNotebookInstanceName :: Lens.Lens' StopNotebookInstance Types.NotebookInstanceName
sniNotebookInstanceName = Lens.field @"notebookInstanceName"
{-# DEPRECATED sniNotebookInstanceName "Use generic-lens or generic-optics with 'notebookInstanceName' instead." #-}

instance Core.FromJSON StopNotebookInstance where
  toJSON StopNotebookInstance {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("NotebookInstanceName" Core..= notebookInstanceName)]
      )

instance Core.AWSRequest StopNotebookInstance where
  type Rs StopNotebookInstance = StopNotebookInstanceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.StopNotebookInstance")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull StopNotebookInstanceResponse'

-- | /See:/ 'mkStopNotebookInstanceResponse' smart constructor.
data StopNotebookInstanceResponse = StopNotebookInstanceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopNotebookInstanceResponse' value with any optional fields omitted.
mkStopNotebookInstanceResponse ::
  StopNotebookInstanceResponse
mkStopNotebookInstanceResponse = StopNotebookInstanceResponse'
