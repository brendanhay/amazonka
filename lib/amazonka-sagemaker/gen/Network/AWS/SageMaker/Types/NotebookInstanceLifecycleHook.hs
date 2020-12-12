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
import qualified Network.AWS.Prelude as Lude

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
  { content ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NotebookInstanceLifecycleHook' with the minimum fields required to make a request.
--
-- * 'content' - A base64-encoded string that contains a shell script for a notebook instance lifecycle configuration.
mkNotebookInstanceLifecycleHook ::
  NotebookInstanceLifecycleHook
mkNotebookInstanceLifecycleHook =
  NotebookInstanceLifecycleHook' {content = Lude.Nothing}

-- | A base64-encoded string that contains a shell script for a notebook instance lifecycle configuration.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nilhContent :: Lens.Lens' NotebookInstanceLifecycleHook (Lude.Maybe Lude.Text)
nilhContent = Lens.lens (content :: NotebookInstanceLifecycleHook -> Lude.Maybe Lude.Text) (\s a -> s {content = a} :: NotebookInstanceLifecycleHook)
{-# DEPRECATED nilhContent "Use generic-lens or generic-optics with 'content' instead." #-}

instance Lude.FromJSON NotebookInstanceLifecycleHook where
  parseJSON =
    Lude.withObject
      "NotebookInstanceLifecycleHook"
      ( \x ->
          NotebookInstanceLifecycleHook' Lude.<$> (x Lude..:? "Content")
      )

instance Lude.ToJSON NotebookInstanceLifecycleHook where
  toJSON NotebookInstanceLifecycleHook' {..} =
    Lude.object
      (Lude.catMaybes [("Content" Lude..=) Lude.<$> content])
