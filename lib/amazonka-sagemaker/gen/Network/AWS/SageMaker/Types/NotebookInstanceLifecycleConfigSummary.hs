-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.NotebookInstanceLifecycleConfigSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.NotebookInstanceLifecycleConfigSummary
  ( NotebookInstanceLifecycleConfigSummary (..),

    -- * Smart constructor
    mkNotebookInstanceLifecycleConfigSummary,

    -- * Lenses
    nilcsCreationTime,
    nilcsLastModifiedTime,
    nilcsNotebookInstanceLifecycleConfigName,
    nilcsNotebookInstanceLifecycleConfigARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides a summary of a notebook instance lifecycle configuration.
--
-- /See:/ 'mkNotebookInstanceLifecycleConfigSummary' smart constructor.
data NotebookInstanceLifecycleConfigSummary = NotebookInstanceLifecycleConfigSummary'
  { creationTime ::
      Lude.Maybe
        Lude.Timestamp,
    lastModifiedTime ::
      Lude.Maybe
        Lude.Timestamp,
    notebookInstanceLifecycleConfigName ::
      Lude.Text,
    notebookInstanceLifecycleConfigARN ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NotebookInstanceLifecycleConfigSummary' with the minimum fields required to make a request.
--
-- * 'creationTime' - A timestamp that tells when the lifecycle configuration was created.
-- * 'lastModifiedTime' - A timestamp that tells when the lifecycle configuration was last modified.
-- * 'notebookInstanceLifecycleConfigARN' - The Amazon Resource Name (ARN) of the lifecycle configuration.
-- * 'notebookInstanceLifecycleConfigName' - The name of the lifecycle configuration.
mkNotebookInstanceLifecycleConfigSummary ::
  -- | 'notebookInstanceLifecycleConfigName'
  Lude.Text ->
  -- | 'notebookInstanceLifecycleConfigARN'
  Lude.Text ->
  NotebookInstanceLifecycleConfigSummary
mkNotebookInstanceLifecycleConfigSummary
  pNotebookInstanceLifecycleConfigName_
  pNotebookInstanceLifecycleConfigARN_ =
    NotebookInstanceLifecycleConfigSummary'
      { creationTime =
          Lude.Nothing,
        lastModifiedTime = Lude.Nothing,
        notebookInstanceLifecycleConfigName =
          pNotebookInstanceLifecycleConfigName_,
        notebookInstanceLifecycleConfigARN =
          pNotebookInstanceLifecycleConfigARN_
      }

-- | A timestamp that tells when the lifecycle configuration was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nilcsCreationTime :: Lens.Lens' NotebookInstanceLifecycleConfigSummary (Lude.Maybe Lude.Timestamp)
nilcsCreationTime = Lens.lens (creationTime :: NotebookInstanceLifecycleConfigSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: NotebookInstanceLifecycleConfigSummary)
{-# DEPRECATED nilcsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | A timestamp that tells when the lifecycle configuration was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nilcsLastModifiedTime :: Lens.Lens' NotebookInstanceLifecycleConfigSummary (Lude.Maybe Lude.Timestamp)
nilcsLastModifiedTime = Lens.lens (lastModifiedTime :: NotebookInstanceLifecycleConfigSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: NotebookInstanceLifecycleConfigSummary)
{-# DEPRECATED nilcsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The name of the lifecycle configuration.
--
-- /Note:/ Consider using 'notebookInstanceLifecycleConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nilcsNotebookInstanceLifecycleConfigName :: Lens.Lens' NotebookInstanceLifecycleConfigSummary Lude.Text
nilcsNotebookInstanceLifecycleConfigName = Lens.lens (notebookInstanceLifecycleConfigName :: NotebookInstanceLifecycleConfigSummary -> Lude.Text) (\s a -> s {notebookInstanceLifecycleConfigName = a} :: NotebookInstanceLifecycleConfigSummary)
{-# DEPRECATED nilcsNotebookInstanceLifecycleConfigName "Use generic-lens or generic-optics with 'notebookInstanceLifecycleConfigName' instead." #-}

-- | The Amazon Resource Name (ARN) of the lifecycle configuration.
--
-- /Note:/ Consider using 'notebookInstanceLifecycleConfigARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nilcsNotebookInstanceLifecycleConfigARN :: Lens.Lens' NotebookInstanceLifecycleConfigSummary Lude.Text
nilcsNotebookInstanceLifecycleConfigARN = Lens.lens (notebookInstanceLifecycleConfigARN :: NotebookInstanceLifecycleConfigSummary -> Lude.Text) (\s a -> s {notebookInstanceLifecycleConfigARN = a} :: NotebookInstanceLifecycleConfigSummary)
{-# DEPRECATED nilcsNotebookInstanceLifecycleConfigARN "Use generic-lens or generic-optics with 'notebookInstanceLifecycleConfigARN' instead." #-}

instance Lude.FromJSON NotebookInstanceLifecycleConfigSummary where
  parseJSON =
    Lude.withObject
      "NotebookInstanceLifecycleConfigSummary"
      ( \x ->
          NotebookInstanceLifecycleConfigSummary'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "LastModifiedTime")
            Lude.<*> (x Lude..: "NotebookInstanceLifecycleConfigName")
            Lude.<*> (x Lude..: "NotebookInstanceLifecycleConfigArn")
      )
