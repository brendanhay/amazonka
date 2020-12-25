{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.NotebookInstanceLifecycleHook
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.NotebookInstanceLifecycleHook
  ( NotebookInstanceLifecycleHook (..),

    -- * Smart constructor
    mkNotebookInstanceLifecycleHook,

    -- * Lenses
    nilhContent,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.NotebookInstanceLifecycleConfigContent as Types

-- | Contains the notebook instance lifecycle configuration script.
--
-- Each lifecycle configuration script has a limit of 16384 characters.
-- The value of the @> PATH@ environment variable that is available to both scripts is @/sbin:bin:/usr/sbin:/usr/bin@ .
-- View CloudWatch Logs for notebook instance lifecycle configurations in log group @/aws/sagemaker/NotebookInstances@ in log stream @[notebook-instance-name]/[LifecycleConfigHook]@ .
-- Lifecycle configuration scripts cannot run for longer than 5 minutes. If a script runs for longer than 5 minutes, it fails and the notebook instance is not created or started.
-- For information about notebook instance lifestyle configurations, see <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance> .
--
-- /See:/ 'mkNotebookInstanceLifecycleHook' smart constructor.
newtype NotebookInstanceLifecycleHook = NotebookInstanceLifecycleHook'
  { -- | A base64-encoded string that contains a shell script for a notebook instance lifecycle configuration.
    content :: Core.Maybe Types.NotebookInstanceLifecycleConfigContent
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'NotebookInstanceLifecycleHook' value with any optional fields omitted.
mkNotebookInstanceLifecycleHook ::
  NotebookInstanceLifecycleHook
mkNotebookInstanceLifecycleHook =
  NotebookInstanceLifecycleHook' {content = Core.Nothing}

-- | A base64-encoded string that contains a shell script for a notebook instance lifecycle configuration.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nilhContent :: Lens.Lens' NotebookInstanceLifecycleHook (Core.Maybe Types.NotebookInstanceLifecycleConfigContent)
nilhContent = Lens.field @"content"
{-# DEPRECATED nilhContent "Use generic-lens or generic-optics with 'content' instead." #-}

instance Core.FromJSON NotebookInstanceLifecycleHook where
  toJSON NotebookInstanceLifecycleHook {..} =
    Core.object
      (Core.catMaybes [("Content" Core..=) Core.<$> content])

instance Core.FromJSON NotebookInstanceLifecycleHook where
  parseJSON =
    Core.withObject "NotebookInstanceLifecycleHook" Core.$
      \x ->
        NotebookInstanceLifecycleHook' Core.<$> (x Core..:? "Content")
