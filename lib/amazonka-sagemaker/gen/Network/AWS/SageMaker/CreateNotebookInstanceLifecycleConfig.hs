{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateNotebookInstanceLifecycleConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a lifecycle configuration that you can associate with a notebook instance. A /lifecycle configuration/ is a collection of shell scripts that run when you create or start a notebook instance.
--
-- Each lifecycle configuration script has a limit of 16384 characters.
-- The value of the @> PATH@ environment variable that is available to both scripts is @/sbin:bin:/usr/sbin:/usr/bin@ .
-- View CloudWatch Logs for notebook instance lifecycle configurations in log group @/aws/sagemaker/NotebookInstances@ in log stream @[notebook-instance-name]/[LifecycleConfigHook]@ .
-- Lifecycle configuration scripts cannot run for longer than 5 minutes. If a script runs for longer than 5 minutes, it fails and the notebook instance is not created or started.
-- For information about notebook instance lifestyle configurations, see <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance> .
module Network.AWS.SageMaker.CreateNotebookInstanceLifecycleConfig
    (
    -- * Creating a request
      CreateNotebookInstanceLifecycleConfig (..)
    , mkCreateNotebookInstanceLifecycleConfig
    -- ** Request lenses
    , cnilcNotebookInstanceLifecycleConfigName
    , cnilcOnCreate
    , cnilcOnStart

    -- * Destructuring the response
    , CreateNotebookInstanceLifecycleConfigResponse (..)
    , mkCreateNotebookInstanceLifecycleConfigResponse
    -- ** Response lenses
    , cnilcrrsNotebookInstanceLifecycleConfigArn
    , cnilcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkCreateNotebookInstanceLifecycleConfig' smart constructor.
data CreateNotebookInstanceLifecycleConfig = CreateNotebookInstanceLifecycleConfig'
  { notebookInstanceLifecycleConfigName :: Types.NotebookInstanceLifecycleConfigName
    -- ^ The name of the lifecycle configuration.
  , onCreate :: Core.Maybe [Types.NotebookInstanceLifecycleHook]
    -- ^ A shell script that runs only once, when you create a notebook instance. The shell script must be a base64-encoded string.
  , onStart :: Core.Maybe [Types.NotebookInstanceLifecycleHook]
    -- ^ A shell script that runs every time you start a notebook instance, including when you create the notebook instance. The shell script must be a base64-encoded string.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateNotebookInstanceLifecycleConfig' value with any optional fields omitted.
mkCreateNotebookInstanceLifecycleConfig
    :: Types.NotebookInstanceLifecycleConfigName -- ^ 'notebookInstanceLifecycleConfigName'
    -> CreateNotebookInstanceLifecycleConfig
mkCreateNotebookInstanceLifecycleConfig
  notebookInstanceLifecycleConfigName
  = CreateNotebookInstanceLifecycleConfig'{notebookInstanceLifecycleConfigName,
                                           onCreate = Core.Nothing, onStart = Core.Nothing}

-- | The name of the lifecycle configuration.
--
-- /Note:/ Consider using 'notebookInstanceLifecycleConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnilcNotebookInstanceLifecycleConfigName :: Lens.Lens' CreateNotebookInstanceLifecycleConfig Types.NotebookInstanceLifecycleConfigName
cnilcNotebookInstanceLifecycleConfigName = Lens.field @"notebookInstanceLifecycleConfigName"
{-# INLINEABLE cnilcNotebookInstanceLifecycleConfigName #-}
{-# DEPRECATED notebookInstanceLifecycleConfigName "Use generic-lens or generic-optics with 'notebookInstanceLifecycleConfigName' instead"  #-}

-- | A shell script that runs only once, when you create a notebook instance. The shell script must be a base64-encoded string.
--
-- /Note:/ Consider using 'onCreate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnilcOnCreate :: Lens.Lens' CreateNotebookInstanceLifecycleConfig (Core.Maybe [Types.NotebookInstanceLifecycleHook])
cnilcOnCreate = Lens.field @"onCreate"
{-# INLINEABLE cnilcOnCreate #-}
{-# DEPRECATED onCreate "Use generic-lens or generic-optics with 'onCreate' instead"  #-}

-- | A shell script that runs every time you start a notebook instance, including when you create the notebook instance. The shell script must be a base64-encoded string.
--
-- /Note:/ Consider using 'onStart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnilcOnStart :: Lens.Lens' CreateNotebookInstanceLifecycleConfig (Core.Maybe [Types.NotebookInstanceLifecycleHook])
cnilcOnStart = Lens.field @"onStart"
{-# INLINEABLE cnilcOnStart #-}
{-# DEPRECATED onStart "Use generic-lens or generic-optics with 'onStart' instead"  #-}

instance Core.ToQuery CreateNotebookInstanceLifecycleConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateNotebookInstanceLifecycleConfig where
        toHeaders CreateNotebookInstanceLifecycleConfig{..}
          = Core.pure
              ("X-Amz-Target", "SageMaker.CreateNotebookInstanceLifecycleConfig")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateNotebookInstanceLifecycleConfig where
        toJSON CreateNotebookInstanceLifecycleConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("NotebookInstanceLifecycleConfigName" Core..=
                       notebookInstanceLifecycleConfigName),
                  ("OnCreate" Core..=) Core.<$> onCreate,
                  ("OnStart" Core..=) Core.<$> onStart])

instance Core.AWSRequest CreateNotebookInstanceLifecycleConfig
         where
        type Rs CreateNotebookInstanceLifecycleConfig =
             CreateNotebookInstanceLifecycleConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateNotebookInstanceLifecycleConfigResponse' Core.<$>
                   (x Core..:? "NotebookInstanceLifecycleConfigArn") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateNotebookInstanceLifecycleConfigResponse' smart constructor.
data CreateNotebookInstanceLifecycleConfigResponse = CreateNotebookInstanceLifecycleConfigResponse'
  { notebookInstanceLifecycleConfigArn :: Core.Maybe Types.NotebookInstanceLifecycleConfigArn
    -- ^ The Amazon Resource Name (ARN) of the lifecycle configuration.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateNotebookInstanceLifecycleConfigResponse' value with any optional fields omitted.
mkCreateNotebookInstanceLifecycleConfigResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateNotebookInstanceLifecycleConfigResponse
mkCreateNotebookInstanceLifecycleConfigResponse responseStatus
  = CreateNotebookInstanceLifecycleConfigResponse'{notebookInstanceLifecycleConfigArn
                                                     = Core.Nothing,
                                                   responseStatus}

-- | The Amazon Resource Name (ARN) of the lifecycle configuration.
--
-- /Note:/ Consider using 'notebookInstanceLifecycleConfigArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnilcrrsNotebookInstanceLifecycleConfigArn :: Lens.Lens' CreateNotebookInstanceLifecycleConfigResponse (Core.Maybe Types.NotebookInstanceLifecycleConfigArn)
cnilcrrsNotebookInstanceLifecycleConfigArn = Lens.field @"notebookInstanceLifecycleConfigArn"
{-# INLINEABLE cnilcrrsNotebookInstanceLifecycleConfigArn #-}
{-# DEPRECATED notebookInstanceLifecycleConfigArn "Use generic-lens or generic-optics with 'notebookInstanceLifecycleConfigArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnilcrrsResponseStatus :: Lens.Lens' CreateNotebookInstanceLifecycleConfigResponse Core.Int
cnilcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cnilcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
