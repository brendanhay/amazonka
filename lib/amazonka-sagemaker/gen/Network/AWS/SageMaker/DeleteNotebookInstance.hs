{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteNotebookInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon SageMaker notebook instance. Before you can delete a notebook instance, you must call the @StopNotebookInstance@ API.
--
-- /Important:/ When you delete a notebook instance, you lose all of your data. Amazon SageMaker removes the ML compute instance, and deletes the ML storage volume and the network interface associated with the notebook instance.
module Network.AWS.SageMaker.DeleteNotebookInstance
  ( -- * Creating a request
    DeleteNotebookInstance (..),
    mkDeleteNotebookInstance,

    -- ** Request lenses
    dNotebookInstanceName,

    -- * Destructuring the response
    DeleteNotebookInstanceResponse (..),
    mkDeleteNotebookInstanceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDeleteNotebookInstance' smart constructor.
newtype DeleteNotebookInstance = DeleteNotebookInstance'
  { -- | The name of the Amazon SageMaker notebook instance to delete.
    notebookInstanceName :: Types.NotebookInstanceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNotebookInstance' value with any optional fields omitted.
mkDeleteNotebookInstance ::
  -- | 'notebookInstanceName'
  Types.NotebookInstanceName ->
  DeleteNotebookInstance
mkDeleteNotebookInstance notebookInstanceName =
  DeleteNotebookInstance' {notebookInstanceName}

-- | The name of the Amazon SageMaker notebook instance to delete.
--
-- /Note:/ Consider using 'notebookInstanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNotebookInstanceName :: Lens.Lens' DeleteNotebookInstance Types.NotebookInstanceName
dNotebookInstanceName = Lens.field @"notebookInstanceName"
{-# DEPRECATED dNotebookInstanceName "Use generic-lens or generic-optics with 'notebookInstanceName' instead." #-}

instance Core.FromJSON DeleteNotebookInstance where
  toJSON DeleteNotebookInstance {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("NotebookInstanceName" Core..= notebookInstanceName)]
      )

instance Core.AWSRequest DeleteNotebookInstance where
  type Rs DeleteNotebookInstance = DeleteNotebookInstanceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.DeleteNotebookInstance")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull DeleteNotebookInstanceResponse'

-- | /See:/ 'mkDeleteNotebookInstanceResponse' smart constructor.
data DeleteNotebookInstanceResponse = DeleteNotebookInstanceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNotebookInstanceResponse' value with any optional fields omitted.
mkDeleteNotebookInstanceResponse ::
  DeleteNotebookInstanceResponse
mkDeleteNotebookInstanceResponse = DeleteNotebookInstanceResponse'
