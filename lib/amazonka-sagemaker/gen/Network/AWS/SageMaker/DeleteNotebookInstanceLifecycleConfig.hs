{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteNotebookInstanceLifecycleConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a notebook instance lifecycle configuration.
module Network.AWS.SageMaker.DeleteNotebookInstanceLifecycleConfig
  ( -- * Creating a request
    DeleteNotebookInstanceLifecycleConfig (..),
    mkDeleteNotebookInstanceLifecycleConfig,

    -- ** Request lenses
    dnilcNotebookInstanceLifecycleConfigName,

    -- * Destructuring the response
    DeleteNotebookInstanceLifecycleConfigResponse (..),
    mkDeleteNotebookInstanceLifecycleConfigResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDeleteNotebookInstanceLifecycleConfig' smart constructor.
newtype DeleteNotebookInstanceLifecycleConfig = DeleteNotebookInstanceLifecycleConfig'
  { -- | The name of the lifecycle configuration to delete.
    notebookInstanceLifecycleConfigName :: Types.NotebookInstanceLifecycleConfigName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNotebookInstanceLifecycleConfig' value with any optional fields omitted.
mkDeleteNotebookInstanceLifecycleConfig ::
  -- | 'notebookInstanceLifecycleConfigName'
  Types.NotebookInstanceLifecycleConfigName ->
  DeleteNotebookInstanceLifecycleConfig
mkDeleteNotebookInstanceLifecycleConfig
  notebookInstanceLifecycleConfigName =
    DeleteNotebookInstanceLifecycleConfig' {notebookInstanceLifecycleConfigName}

-- | The name of the lifecycle configuration to delete.
--
-- /Note:/ Consider using 'notebookInstanceLifecycleConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnilcNotebookInstanceLifecycleConfigName :: Lens.Lens' DeleteNotebookInstanceLifecycleConfig Types.NotebookInstanceLifecycleConfigName
dnilcNotebookInstanceLifecycleConfigName = Lens.field @"notebookInstanceLifecycleConfigName"
{-# DEPRECATED dnilcNotebookInstanceLifecycleConfigName "Use generic-lens or generic-optics with 'notebookInstanceLifecycleConfigName' instead." #-}

instance Core.FromJSON DeleteNotebookInstanceLifecycleConfig where
  toJSON DeleteNotebookInstanceLifecycleConfig {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "NotebookInstanceLifecycleConfigName"
                  Core..= notebookInstanceLifecycleConfigName
              )
          ]
      )

instance Core.AWSRequest DeleteNotebookInstanceLifecycleConfig where
  type
    Rs DeleteNotebookInstanceLifecycleConfig =
      DeleteNotebookInstanceLifecycleConfigResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "SageMaker.DeleteNotebookInstanceLifecycleConfig")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveNull
      DeleteNotebookInstanceLifecycleConfigResponse'

-- | /See:/ 'mkDeleteNotebookInstanceLifecycleConfigResponse' smart constructor.
data DeleteNotebookInstanceLifecycleConfigResponse = DeleteNotebookInstanceLifecycleConfigResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNotebookInstanceLifecycleConfigResponse' value with any optional fields omitted.
mkDeleteNotebookInstanceLifecycleConfigResponse ::
  DeleteNotebookInstanceLifecycleConfigResponse
mkDeleteNotebookInstanceLifecycleConfigResponse =
  DeleteNotebookInstanceLifecycleConfigResponse'
