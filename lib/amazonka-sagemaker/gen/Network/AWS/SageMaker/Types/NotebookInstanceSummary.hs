{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.NotebookInstanceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.NotebookInstanceSummary
  ( NotebookInstanceSummary (..),

    -- * Smart constructor
    mkNotebookInstanceSummary,

    -- * Lenses
    nisNotebookInstanceName,
    nisNotebookInstanceArn,
    nisAdditionalCodeRepositories,
    nisCreationTime,
    nisDefaultCodeRepository,
    nisInstanceType,
    nisLastModifiedTime,
    nisNotebookInstanceLifecycleConfigName,
    nisNotebookInstanceStatus,
    nisUrl,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.CodeRepositoryNameOrUrl as Types
import qualified Network.AWS.SageMaker.Types.DefaultCodeRepository as Types
import qualified Network.AWS.SageMaker.Types.InstanceType as Types
import qualified Network.AWS.SageMaker.Types.NotebookInstanceArn as Types
import qualified Network.AWS.SageMaker.Types.NotebookInstanceLifecycleConfigName as Types
import qualified Network.AWS.SageMaker.Types.NotebookInstanceName as Types
import qualified Network.AWS.SageMaker.Types.NotebookInstanceStatus as Types
import qualified Network.AWS.SageMaker.Types.Url as Types

-- | Provides summary information for an Amazon SageMaker notebook instance.
--
-- /See:/ 'mkNotebookInstanceSummary' smart constructor.
data NotebookInstanceSummary = NotebookInstanceSummary'
  { -- | The name of the notebook instance that you want a summary for.
    notebookInstanceName :: Types.NotebookInstanceName,
    -- | The Amazon Resource Name (ARN) of the notebook instance.
    notebookInstanceArn :: Types.NotebookInstanceArn,
    -- | An array of up to three Git repositories associated with the notebook instance. These can be either the names of Git repositories stored as resources in your account, or the URL of Git repositories in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. These repositories are cloned at the same level as the default repository of your notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
    additionalCodeRepositories :: Core.Maybe [Types.CodeRepositoryNameOrUrl],
    -- | A timestamp that shows when the notebook instance was created.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | The Git repository associated with the notebook instance as its default code repository. This can be either the name of a Git repository stored as a resource in your account, or the URL of a Git repository in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. When you open a notebook instance, it opens in the directory that contains this repository. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
    defaultCodeRepository :: Core.Maybe Types.DefaultCodeRepository,
    -- | The type of ML compute instance that the notebook instance is running on.
    instanceType :: Core.Maybe Types.InstanceType,
    -- | A timestamp that shows when the notebook instance was last modified.
    lastModifiedTime :: Core.Maybe Core.NominalDiffTime,
    -- | The name of a notebook instance lifecycle configuration associated with this notebook instance.
    --
    -- For information about notebook instance lifestyle configurations, see <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance> .
    notebookInstanceLifecycleConfigName :: Core.Maybe Types.NotebookInstanceLifecycleConfigName,
    -- | The status of the notebook instance.
    notebookInstanceStatus :: Core.Maybe Types.NotebookInstanceStatus,
    -- | The URL that you use to connect to the Jupyter instance running in your notebook instance.
    url :: Core.Maybe Types.Url
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'NotebookInstanceSummary' value with any optional fields omitted.
mkNotebookInstanceSummary ::
  -- | 'notebookInstanceName'
  Types.NotebookInstanceName ->
  -- | 'notebookInstanceArn'
  Types.NotebookInstanceArn ->
  NotebookInstanceSummary
mkNotebookInstanceSummary notebookInstanceName notebookInstanceArn =
  NotebookInstanceSummary'
    { notebookInstanceName,
      notebookInstanceArn,
      additionalCodeRepositories = Core.Nothing,
      creationTime = Core.Nothing,
      defaultCodeRepository = Core.Nothing,
      instanceType = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      notebookInstanceLifecycleConfigName = Core.Nothing,
      notebookInstanceStatus = Core.Nothing,
      url = Core.Nothing
    }

-- | The name of the notebook instance that you want a summary for.
--
-- /Note:/ Consider using 'notebookInstanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nisNotebookInstanceName :: Lens.Lens' NotebookInstanceSummary Types.NotebookInstanceName
nisNotebookInstanceName = Lens.field @"notebookInstanceName"
{-# DEPRECATED nisNotebookInstanceName "Use generic-lens or generic-optics with 'notebookInstanceName' instead." #-}

-- | The Amazon Resource Name (ARN) of the notebook instance.
--
-- /Note:/ Consider using 'notebookInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nisNotebookInstanceArn :: Lens.Lens' NotebookInstanceSummary Types.NotebookInstanceArn
nisNotebookInstanceArn = Lens.field @"notebookInstanceArn"
{-# DEPRECATED nisNotebookInstanceArn "Use generic-lens or generic-optics with 'notebookInstanceArn' instead." #-}

-- | An array of up to three Git repositories associated with the notebook instance. These can be either the names of Git repositories stored as resources in your account, or the URL of Git repositories in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. These repositories are cloned at the same level as the default repository of your notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
--
-- /Note:/ Consider using 'additionalCodeRepositories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nisAdditionalCodeRepositories :: Lens.Lens' NotebookInstanceSummary (Core.Maybe [Types.CodeRepositoryNameOrUrl])
nisAdditionalCodeRepositories = Lens.field @"additionalCodeRepositories"
{-# DEPRECATED nisAdditionalCodeRepositories "Use generic-lens or generic-optics with 'additionalCodeRepositories' instead." #-}

-- | A timestamp that shows when the notebook instance was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nisCreationTime :: Lens.Lens' NotebookInstanceSummary (Core.Maybe Core.NominalDiffTime)
nisCreationTime = Lens.field @"creationTime"
{-# DEPRECATED nisCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The Git repository associated with the notebook instance as its default code repository. This can be either the name of a Git repository stored as a resource in your account, or the URL of a Git repository in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. When you open a notebook instance, it opens in the directory that contains this repository. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
--
-- /Note:/ Consider using 'defaultCodeRepository' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nisDefaultCodeRepository :: Lens.Lens' NotebookInstanceSummary (Core.Maybe Types.DefaultCodeRepository)
nisDefaultCodeRepository = Lens.field @"defaultCodeRepository"
{-# DEPRECATED nisDefaultCodeRepository "Use generic-lens or generic-optics with 'defaultCodeRepository' instead." #-}

-- | The type of ML compute instance that the notebook instance is running on.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nisInstanceType :: Lens.Lens' NotebookInstanceSummary (Core.Maybe Types.InstanceType)
nisInstanceType = Lens.field @"instanceType"
{-# DEPRECATED nisInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | A timestamp that shows when the notebook instance was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nisLastModifiedTime :: Lens.Lens' NotebookInstanceSummary (Core.Maybe Core.NominalDiffTime)
nisLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED nisLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The name of a notebook instance lifecycle configuration associated with this notebook instance.
--
-- For information about notebook instance lifestyle configurations, see <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance> .
--
-- /Note:/ Consider using 'notebookInstanceLifecycleConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nisNotebookInstanceLifecycleConfigName :: Lens.Lens' NotebookInstanceSummary (Core.Maybe Types.NotebookInstanceLifecycleConfigName)
nisNotebookInstanceLifecycleConfigName = Lens.field @"notebookInstanceLifecycleConfigName"
{-# DEPRECATED nisNotebookInstanceLifecycleConfigName "Use generic-lens or generic-optics with 'notebookInstanceLifecycleConfigName' instead." #-}

-- | The status of the notebook instance.
--
-- /Note:/ Consider using 'notebookInstanceStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nisNotebookInstanceStatus :: Lens.Lens' NotebookInstanceSummary (Core.Maybe Types.NotebookInstanceStatus)
nisNotebookInstanceStatus = Lens.field @"notebookInstanceStatus"
{-# DEPRECATED nisNotebookInstanceStatus "Use generic-lens or generic-optics with 'notebookInstanceStatus' instead." #-}

-- | The URL that you use to connect to the Jupyter instance running in your notebook instance.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nisUrl :: Lens.Lens' NotebookInstanceSummary (Core.Maybe Types.Url)
nisUrl = Lens.field @"url"
{-# DEPRECATED nisUrl "Use generic-lens or generic-optics with 'url' instead." #-}

instance Core.FromJSON NotebookInstanceSummary where
  parseJSON =
    Core.withObject "NotebookInstanceSummary" Core.$
      \x ->
        NotebookInstanceSummary'
          Core.<$> (x Core..: "NotebookInstanceName")
          Core.<*> (x Core..: "NotebookInstanceArn")
          Core.<*> (x Core..:? "AdditionalCodeRepositories")
          Core.<*> (x Core..:? "CreationTime")
          Core.<*> (x Core..:? "DefaultCodeRepository")
          Core.<*> (x Core..:? "InstanceType")
          Core.<*> (x Core..:? "LastModifiedTime")
          Core.<*> (x Core..:? "NotebookInstanceLifecycleConfigName")
          Core.<*> (x Core..:? "NotebookInstanceStatus")
          Core.<*> (x Core..:? "Url")
