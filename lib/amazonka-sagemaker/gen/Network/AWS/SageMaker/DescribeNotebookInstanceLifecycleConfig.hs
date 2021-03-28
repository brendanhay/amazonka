{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeNotebookInstanceLifecycleConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of a notebook instance lifecycle configuration.
--
-- For information about notebook instance lifestyle configurations, see <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance> .
module Network.AWS.SageMaker.DescribeNotebookInstanceLifecycleConfig
    (
    -- * Creating a request
      DescribeNotebookInstanceLifecycleConfig (..)
    , mkDescribeNotebookInstanceLifecycleConfig
    -- ** Request lenses
    , dNotebookInstanceLifecycleConfigName

    -- * Destructuring the response
    , DescribeNotebookInstanceLifecycleConfigResponse (..)
    , mkDescribeNotebookInstanceLifecycleConfigResponse
    -- ** Response lenses
    , dnilcrrsCreationTime
    , dnilcrrsLastModifiedTime
    , dnilcrrsNotebookInstanceLifecycleConfigArn
    , dnilcrrsNotebookInstanceLifecycleConfigName
    , dnilcrrsOnCreate
    , dnilcrrsOnStart
    , dnilcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeNotebookInstanceLifecycleConfig' smart constructor.
newtype DescribeNotebookInstanceLifecycleConfig = DescribeNotebookInstanceLifecycleConfig'
  { notebookInstanceLifecycleConfigName :: Types.NotebookInstanceLifecycleConfigName
    -- ^ The name of the lifecycle configuration to describe.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeNotebookInstanceLifecycleConfig' value with any optional fields omitted.
mkDescribeNotebookInstanceLifecycleConfig
    :: Types.NotebookInstanceLifecycleConfigName -- ^ 'notebookInstanceLifecycleConfigName'
    -> DescribeNotebookInstanceLifecycleConfig
mkDescribeNotebookInstanceLifecycleConfig
  notebookInstanceLifecycleConfigName
  = DescribeNotebookInstanceLifecycleConfig'{notebookInstanceLifecycleConfigName}

-- | The name of the lifecycle configuration to describe.
--
-- /Note:/ Consider using 'notebookInstanceLifecycleConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNotebookInstanceLifecycleConfigName :: Lens.Lens' DescribeNotebookInstanceLifecycleConfig Types.NotebookInstanceLifecycleConfigName
dNotebookInstanceLifecycleConfigName = Lens.field @"notebookInstanceLifecycleConfigName"
{-# INLINEABLE dNotebookInstanceLifecycleConfigName #-}
{-# DEPRECATED notebookInstanceLifecycleConfigName "Use generic-lens or generic-optics with 'notebookInstanceLifecycleConfigName' instead"  #-}

instance Core.ToQuery DescribeNotebookInstanceLifecycleConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeNotebookInstanceLifecycleConfig
         where
        toHeaders DescribeNotebookInstanceLifecycleConfig{..}
          = Core.pure
              ("X-Amz-Target",
               "SageMaker.DescribeNotebookInstanceLifecycleConfig")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeNotebookInstanceLifecycleConfig
         where
        toJSON DescribeNotebookInstanceLifecycleConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("NotebookInstanceLifecycleConfigName" Core..=
                       notebookInstanceLifecycleConfigName)])

instance Core.AWSRequest DescribeNotebookInstanceLifecycleConfig
         where
        type Rs DescribeNotebookInstanceLifecycleConfig =
             DescribeNotebookInstanceLifecycleConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeNotebookInstanceLifecycleConfigResponse' Core.<$>
                   (x Core..:? "CreationTime") Core.<*> x Core..:? "LastModifiedTime"
                     Core.<*> x Core..:? "NotebookInstanceLifecycleConfigArn"
                     Core.<*> x Core..:? "NotebookInstanceLifecycleConfigName"
                     Core.<*> x Core..:? "OnCreate"
                     Core.<*> x Core..:? "OnStart"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeNotebookInstanceLifecycleConfigResponse' smart constructor.
data DescribeNotebookInstanceLifecycleConfigResponse = DescribeNotebookInstanceLifecycleConfigResponse'
  { creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ A timestamp that tells when the lifecycle configuration was created.
  , lastModifiedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ A timestamp that tells when the lifecycle configuration was last modified.
  , notebookInstanceLifecycleConfigArn :: Core.Maybe Types.NotebookInstanceLifecycleConfigArn
    -- ^ The Amazon Resource Name (ARN) of the lifecycle configuration.
  , notebookInstanceLifecycleConfigName :: Core.Maybe Types.NotebookInstanceLifecycleConfigName
    -- ^ The name of the lifecycle configuration.
  , onCreate :: Core.Maybe [Types.NotebookInstanceLifecycleHook]
    -- ^ The shell script that runs only once, when you create a notebook instance.
  , onStart :: Core.Maybe [Types.NotebookInstanceLifecycleHook]
    -- ^ The shell script that runs every time you start a notebook instance, including when you create the notebook instance.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeNotebookInstanceLifecycleConfigResponse' value with any optional fields omitted.
mkDescribeNotebookInstanceLifecycleConfigResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeNotebookInstanceLifecycleConfigResponse
mkDescribeNotebookInstanceLifecycleConfigResponse responseStatus
  = DescribeNotebookInstanceLifecycleConfigResponse'{creationTime =
                                                       Core.Nothing,
                                                     lastModifiedTime = Core.Nothing,
                                                     notebookInstanceLifecycleConfigArn =
                                                       Core.Nothing,
                                                     notebookInstanceLifecycleConfigName =
                                                       Core.Nothing,
                                                     onCreate = Core.Nothing,
                                                     onStart = Core.Nothing, responseStatus}

-- | A timestamp that tells when the lifecycle configuration was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnilcrrsCreationTime :: Lens.Lens' DescribeNotebookInstanceLifecycleConfigResponse (Core.Maybe Core.NominalDiffTime)
dnilcrrsCreationTime = Lens.field @"creationTime"
{-# INLINEABLE dnilcrrsCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | A timestamp that tells when the lifecycle configuration was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnilcrrsLastModifiedTime :: Lens.Lens' DescribeNotebookInstanceLifecycleConfigResponse (Core.Maybe Core.NominalDiffTime)
dnilcrrsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# INLINEABLE dnilcrrsLastModifiedTime #-}
{-# DEPRECATED lastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead"  #-}

-- | The Amazon Resource Name (ARN) of the lifecycle configuration.
--
-- /Note:/ Consider using 'notebookInstanceLifecycleConfigArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnilcrrsNotebookInstanceLifecycleConfigArn :: Lens.Lens' DescribeNotebookInstanceLifecycleConfigResponse (Core.Maybe Types.NotebookInstanceLifecycleConfigArn)
dnilcrrsNotebookInstanceLifecycleConfigArn = Lens.field @"notebookInstanceLifecycleConfigArn"
{-# INLINEABLE dnilcrrsNotebookInstanceLifecycleConfigArn #-}
{-# DEPRECATED notebookInstanceLifecycleConfigArn "Use generic-lens or generic-optics with 'notebookInstanceLifecycleConfigArn' instead"  #-}

-- | The name of the lifecycle configuration.
--
-- /Note:/ Consider using 'notebookInstanceLifecycleConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnilcrrsNotebookInstanceLifecycleConfigName :: Lens.Lens' DescribeNotebookInstanceLifecycleConfigResponse (Core.Maybe Types.NotebookInstanceLifecycleConfigName)
dnilcrrsNotebookInstanceLifecycleConfigName = Lens.field @"notebookInstanceLifecycleConfigName"
{-# INLINEABLE dnilcrrsNotebookInstanceLifecycleConfigName #-}
{-# DEPRECATED notebookInstanceLifecycleConfigName "Use generic-lens or generic-optics with 'notebookInstanceLifecycleConfigName' instead"  #-}

-- | The shell script that runs only once, when you create a notebook instance.
--
-- /Note:/ Consider using 'onCreate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnilcrrsOnCreate :: Lens.Lens' DescribeNotebookInstanceLifecycleConfigResponse (Core.Maybe [Types.NotebookInstanceLifecycleHook])
dnilcrrsOnCreate = Lens.field @"onCreate"
{-# INLINEABLE dnilcrrsOnCreate #-}
{-# DEPRECATED onCreate "Use generic-lens or generic-optics with 'onCreate' instead"  #-}

-- | The shell script that runs every time you start a notebook instance, including when you create the notebook instance.
--
-- /Note:/ Consider using 'onStart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnilcrrsOnStart :: Lens.Lens' DescribeNotebookInstanceLifecycleConfigResponse (Core.Maybe [Types.NotebookInstanceLifecycleHook])
dnilcrrsOnStart = Lens.field @"onStart"
{-# INLINEABLE dnilcrrsOnStart #-}
{-# DEPRECATED onStart "Use generic-lens or generic-optics with 'onStart' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnilcrrsResponseStatus :: Lens.Lens' DescribeNotebookInstanceLifecycleConfigResponse Core.Int
dnilcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dnilcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
