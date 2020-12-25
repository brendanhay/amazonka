{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.StartNotebookInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Launches an ML compute instance with the latest version of the libraries and attaches your ML storage volume. After configuring the notebook instance, Amazon SageMaker sets the notebook instance status to @InService@ . A notebook instance's status must be @InService@ before you can connect to your Jupyter notebook.
module Network.AWS.SageMaker.StartNotebookInstance
  ( -- * Creating a request
    StartNotebookInstance (..),
    mkStartNotebookInstance,

    -- ** Request lenses
    sNotebookInstanceName,

    -- * Destructuring the response
    StartNotebookInstanceResponse (..),
    mkStartNotebookInstanceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkStartNotebookInstance' smart constructor.
newtype StartNotebookInstance = StartNotebookInstance'
  { -- | The name of the notebook instance to start.
    notebookInstanceName :: Types.NotebookInstanceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartNotebookInstance' value with any optional fields omitted.
mkStartNotebookInstance ::
  -- | 'notebookInstanceName'
  Types.NotebookInstanceName ->
  StartNotebookInstance
mkStartNotebookInstance notebookInstanceName =
  StartNotebookInstance' {notebookInstanceName}

-- | The name of the notebook instance to start.
--
-- /Note:/ Consider using 'notebookInstanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sNotebookInstanceName :: Lens.Lens' StartNotebookInstance Types.NotebookInstanceName
sNotebookInstanceName = Lens.field @"notebookInstanceName"
{-# DEPRECATED sNotebookInstanceName "Use generic-lens or generic-optics with 'notebookInstanceName' instead." #-}

instance Core.FromJSON StartNotebookInstance where
  toJSON StartNotebookInstance {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("NotebookInstanceName" Core..= notebookInstanceName)]
      )

instance Core.AWSRequest StartNotebookInstance where
  type Rs StartNotebookInstance = StartNotebookInstanceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.StartNotebookInstance")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull StartNotebookInstanceResponse'

-- | /See:/ 'mkStartNotebookInstanceResponse' smart constructor.
data StartNotebookInstanceResponse = StartNotebookInstanceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartNotebookInstanceResponse' value with any optional fields omitted.
mkStartNotebookInstanceResponse ::
  StartNotebookInstanceResponse
mkStartNotebookInstanceResponse = StartNotebookInstanceResponse'
