{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    nilcsNotebookInstanceLifecycleConfigName,
    nilcsNotebookInstanceLifecycleConfigArn,
    nilcsCreationTime,
    nilcsLastModifiedTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.NotebookInstanceLifecycleConfigArn as Types
import qualified Network.AWS.SageMaker.Types.NotebookInstanceLifecycleConfigName as Types

-- | Provides a summary of a notebook instance lifecycle configuration.
--
-- /See:/ 'mkNotebookInstanceLifecycleConfigSummary' smart constructor.
data NotebookInstanceLifecycleConfigSummary = NotebookInstanceLifecycleConfigSummary'
  { -- | The name of the lifecycle configuration.
    notebookInstanceLifecycleConfigName :: Types.NotebookInstanceLifecycleConfigName,
    -- | The Amazon Resource Name (ARN) of the lifecycle configuration.
    notebookInstanceLifecycleConfigArn :: Types.NotebookInstanceLifecycleConfigArn,
    -- | A timestamp that tells when the lifecycle configuration was created.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | A timestamp that tells when the lifecycle configuration was last modified.
    lastModifiedTime :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'NotebookInstanceLifecycleConfigSummary' value with any optional fields omitted.
mkNotebookInstanceLifecycleConfigSummary ::
  -- | 'notebookInstanceLifecycleConfigName'
  Types.NotebookInstanceLifecycleConfigName ->
  -- | 'notebookInstanceLifecycleConfigArn'
  Types.NotebookInstanceLifecycleConfigArn ->
  NotebookInstanceLifecycleConfigSummary
mkNotebookInstanceLifecycleConfigSummary
  notebookInstanceLifecycleConfigName
  notebookInstanceLifecycleConfigArn =
    NotebookInstanceLifecycleConfigSummary'
      { notebookInstanceLifecycleConfigName,
        notebookInstanceLifecycleConfigArn,
        creationTime = Core.Nothing,
        lastModifiedTime = Core.Nothing
      }

-- | The name of the lifecycle configuration.
--
-- /Note:/ Consider using 'notebookInstanceLifecycleConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nilcsNotebookInstanceLifecycleConfigName :: Lens.Lens' NotebookInstanceLifecycleConfigSummary Types.NotebookInstanceLifecycleConfigName
nilcsNotebookInstanceLifecycleConfigName = Lens.field @"notebookInstanceLifecycleConfigName"
{-# DEPRECATED nilcsNotebookInstanceLifecycleConfigName "Use generic-lens or generic-optics with 'notebookInstanceLifecycleConfigName' instead." #-}

-- | The Amazon Resource Name (ARN) of the lifecycle configuration.
--
-- /Note:/ Consider using 'notebookInstanceLifecycleConfigArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nilcsNotebookInstanceLifecycleConfigArn :: Lens.Lens' NotebookInstanceLifecycleConfigSummary Types.NotebookInstanceLifecycleConfigArn
nilcsNotebookInstanceLifecycleConfigArn = Lens.field @"notebookInstanceLifecycleConfigArn"
{-# DEPRECATED nilcsNotebookInstanceLifecycleConfigArn "Use generic-lens or generic-optics with 'notebookInstanceLifecycleConfigArn' instead." #-}

-- | A timestamp that tells when the lifecycle configuration was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nilcsCreationTime :: Lens.Lens' NotebookInstanceLifecycleConfigSummary (Core.Maybe Core.NominalDiffTime)
nilcsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED nilcsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | A timestamp that tells when the lifecycle configuration was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nilcsLastModifiedTime :: Lens.Lens' NotebookInstanceLifecycleConfigSummary (Core.Maybe Core.NominalDiffTime)
nilcsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED nilcsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

instance Core.FromJSON NotebookInstanceLifecycleConfigSummary where
  parseJSON =
    Core.withObject "NotebookInstanceLifecycleConfigSummary" Core.$
      \x ->
        NotebookInstanceLifecycleConfigSummary'
          Core.<$> (x Core..: "NotebookInstanceLifecycleConfigName")
          Core.<*> (x Core..: "NotebookInstanceLifecycleConfigArn")
          Core.<*> (x Core..:? "CreationTime")
          Core.<*> (x Core..:? "LastModifiedTime")
