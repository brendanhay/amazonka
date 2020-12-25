{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.UpdateNotebookInstanceLifecycleConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a notebook instance lifecycle configuration created with the 'CreateNotebookInstanceLifecycleConfig' API.
module Network.AWS.SageMaker.UpdateNotebookInstanceLifecycleConfig
  ( -- * Creating a request
    UpdateNotebookInstanceLifecycleConfig (..),
    mkUpdateNotebookInstanceLifecycleConfig,

    -- ** Request lenses
    unilcNotebookInstanceLifecycleConfigName,
    unilcOnCreate,
    unilcOnStart,

    -- * Destructuring the response
    UpdateNotebookInstanceLifecycleConfigResponse (..),
    mkUpdateNotebookInstanceLifecycleConfigResponse,

    -- ** Response lenses
    unilcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkUpdateNotebookInstanceLifecycleConfig' smart constructor.
data UpdateNotebookInstanceLifecycleConfig = UpdateNotebookInstanceLifecycleConfig'
  { -- | The name of the lifecycle configuration.
    notebookInstanceLifecycleConfigName :: Types.NotebookInstanceLifecycleConfigName,
    -- | The shell script that runs only once, when you create a notebook instance. The shell script must be a base64-encoded string.
    onCreate :: Core.Maybe [Types.NotebookInstanceLifecycleHook],
    -- | The shell script that runs every time you start a notebook instance, including when you create the notebook instance. The shell script must be a base64-encoded string.
    onStart :: Core.Maybe [Types.NotebookInstanceLifecycleHook]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateNotebookInstanceLifecycleConfig' value with any optional fields omitted.
mkUpdateNotebookInstanceLifecycleConfig ::
  -- | 'notebookInstanceLifecycleConfigName'
  Types.NotebookInstanceLifecycleConfigName ->
  UpdateNotebookInstanceLifecycleConfig
mkUpdateNotebookInstanceLifecycleConfig
  notebookInstanceLifecycleConfigName =
    UpdateNotebookInstanceLifecycleConfig'
      { notebookInstanceLifecycleConfigName,
        onCreate = Core.Nothing,
        onStart = Core.Nothing
      }

-- | The name of the lifecycle configuration.
--
-- /Note:/ Consider using 'notebookInstanceLifecycleConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unilcNotebookInstanceLifecycleConfigName :: Lens.Lens' UpdateNotebookInstanceLifecycleConfig Types.NotebookInstanceLifecycleConfigName
unilcNotebookInstanceLifecycleConfigName = Lens.field @"notebookInstanceLifecycleConfigName"
{-# DEPRECATED unilcNotebookInstanceLifecycleConfigName "Use generic-lens or generic-optics with 'notebookInstanceLifecycleConfigName' instead." #-}

-- | The shell script that runs only once, when you create a notebook instance. The shell script must be a base64-encoded string.
--
-- /Note:/ Consider using 'onCreate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unilcOnCreate :: Lens.Lens' UpdateNotebookInstanceLifecycleConfig (Core.Maybe [Types.NotebookInstanceLifecycleHook])
unilcOnCreate = Lens.field @"onCreate"
{-# DEPRECATED unilcOnCreate "Use generic-lens or generic-optics with 'onCreate' instead." #-}

-- | The shell script that runs every time you start a notebook instance, including when you create the notebook instance. The shell script must be a base64-encoded string.
--
-- /Note:/ Consider using 'onStart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unilcOnStart :: Lens.Lens' UpdateNotebookInstanceLifecycleConfig (Core.Maybe [Types.NotebookInstanceLifecycleHook])
unilcOnStart = Lens.field @"onStart"
{-# DEPRECATED unilcOnStart "Use generic-lens or generic-optics with 'onStart' instead." #-}

instance Core.FromJSON UpdateNotebookInstanceLifecycleConfig where
  toJSON UpdateNotebookInstanceLifecycleConfig {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "NotebookInstanceLifecycleConfigName"
                  Core..= notebookInstanceLifecycleConfigName
              ),
            ("OnCreate" Core..=) Core.<$> onCreate,
            ("OnStart" Core..=) Core.<$> onStart
          ]
      )

instance Core.AWSRequest UpdateNotebookInstanceLifecycleConfig where
  type
    Rs UpdateNotebookInstanceLifecycleConfig =
      UpdateNotebookInstanceLifecycleConfigResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "SageMaker.UpdateNotebookInstanceLifecycleConfig")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateNotebookInstanceLifecycleConfigResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateNotebookInstanceLifecycleConfigResponse' smart constructor.
newtype UpdateNotebookInstanceLifecycleConfigResponse = UpdateNotebookInstanceLifecycleConfigResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateNotebookInstanceLifecycleConfigResponse' value with any optional fields omitted.
mkUpdateNotebookInstanceLifecycleConfigResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateNotebookInstanceLifecycleConfigResponse
mkUpdateNotebookInstanceLifecycleConfigResponse responseStatus =
  UpdateNotebookInstanceLifecycleConfigResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unilcrrsResponseStatus :: Lens.Lens' UpdateNotebookInstanceLifecycleConfigResponse Core.Int
unilcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED unilcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
