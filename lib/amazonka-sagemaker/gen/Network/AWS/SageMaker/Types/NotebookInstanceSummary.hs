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
    nisCreationTime,
    nisAdditionalCodeRepositories,
    nisURL,
    nisLastModifiedTime,
    nisInstanceType,
    nisNotebookInstanceStatus,
    nisDefaultCodeRepository,
    nisNotebookInstanceLifecycleConfigName,
    nisNotebookInstanceName,
    nisNotebookInstanceARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.InstanceType
import Network.AWS.SageMaker.Types.NotebookInstanceStatus

-- | Provides summary information for an Amazon SageMaker notebook instance.
--
-- /See:/ 'mkNotebookInstanceSummary' smart constructor.
data NotebookInstanceSummary = NotebookInstanceSummary'
  { creationTime ::
      Lude.Maybe Lude.Timestamp,
    additionalCodeRepositories ::
      Lude.Maybe [Lude.Text],
    url :: Lude.Maybe Lude.Text,
    lastModifiedTime ::
      Lude.Maybe Lude.Timestamp,
    instanceType :: Lude.Maybe InstanceType,
    notebookInstanceStatus ::
      Lude.Maybe NotebookInstanceStatus,
    defaultCodeRepository ::
      Lude.Maybe Lude.Text,
    notebookInstanceLifecycleConfigName ::
      Lude.Maybe Lude.Text,
    notebookInstanceName :: Lude.Text,
    notebookInstanceARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NotebookInstanceSummary' with the minimum fields required to make a request.
--
-- * 'additionalCodeRepositories' - An array of up to three Git repositories associated with the notebook instance. These can be either the names of Git repositories stored as resources in your account, or the URL of Git repositories in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. These repositories are cloned at the same level as the default repository of your notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
-- * 'creationTime' - A timestamp that shows when the notebook instance was created.
-- * 'defaultCodeRepository' - The Git repository associated with the notebook instance as its default code repository. This can be either the name of a Git repository stored as a resource in your account, or the URL of a Git repository in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. When you open a notebook instance, it opens in the directory that contains this repository. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
-- * 'instanceType' - The type of ML compute instance that the notebook instance is running on.
-- * 'lastModifiedTime' - A timestamp that shows when the notebook instance was last modified.
-- * 'notebookInstanceARN' - The Amazon Resource Name (ARN) of the notebook instance.
-- * 'notebookInstanceLifecycleConfigName' - The name of a notebook instance lifecycle configuration associated with this notebook instance.
--
-- For information about notebook instance lifestyle configurations, see <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance> .
-- * 'notebookInstanceName' - The name of the notebook instance that you want a summary for.
-- * 'notebookInstanceStatus' - The status of the notebook instance.
-- * 'url' - The URL that you use to connect to the Jupyter instance running in your notebook instance.
mkNotebookInstanceSummary ::
  -- | 'notebookInstanceName'
  Lude.Text ->
  -- | 'notebookInstanceARN'
  Lude.Text ->
  NotebookInstanceSummary
mkNotebookInstanceSummary
  pNotebookInstanceName_
  pNotebookInstanceARN_ =
    NotebookInstanceSummary'
      { creationTime = Lude.Nothing,
        additionalCodeRepositories = Lude.Nothing,
        url = Lude.Nothing,
        lastModifiedTime = Lude.Nothing,
        instanceType = Lude.Nothing,
        notebookInstanceStatus = Lude.Nothing,
        defaultCodeRepository = Lude.Nothing,
        notebookInstanceLifecycleConfigName = Lude.Nothing,
        notebookInstanceName = pNotebookInstanceName_,
        notebookInstanceARN = pNotebookInstanceARN_
      }

-- | A timestamp that shows when the notebook instance was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nisCreationTime :: Lens.Lens' NotebookInstanceSummary (Lude.Maybe Lude.Timestamp)
nisCreationTime = Lens.lens (creationTime :: NotebookInstanceSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: NotebookInstanceSummary)
{-# DEPRECATED nisCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | An array of up to three Git repositories associated with the notebook instance. These can be either the names of Git repositories stored as resources in your account, or the URL of Git repositories in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. These repositories are cloned at the same level as the default repository of your notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
--
-- /Note:/ Consider using 'additionalCodeRepositories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nisAdditionalCodeRepositories :: Lens.Lens' NotebookInstanceSummary (Lude.Maybe [Lude.Text])
nisAdditionalCodeRepositories = Lens.lens (additionalCodeRepositories :: NotebookInstanceSummary -> Lude.Maybe [Lude.Text]) (\s a -> s {additionalCodeRepositories = a} :: NotebookInstanceSummary)
{-# DEPRECATED nisAdditionalCodeRepositories "Use generic-lens or generic-optics with 'additionalCodeRepositories' instead." #-}

-- | The URL that you use to connect to the Jupyter instance running in your notebook instance.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nisURL :: Lens.Lens' NotebookInstanceSummary (Lude.Maybe Lude.Text)
nisURL = Lens.lens (url :: NotebookInstanceSummary -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: NotebookInstanceSummary)
{-# DEPRECATED nisURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | A timestamp that shows when the notebook instance was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nisLastModifiedTime :: Lens.Lens' NotebookInstanceSummary (Lude.Maybe Lude.Timestamp)
nisLastModifiedTime = Lens.lens (lastModifiedTime :: NotebookInstanceSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: NotebookInstanceSummary)
{-# DEPRECATED nisLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The type of ML compute instance that the notebook instance is running on.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nisInstanceType :: Lens.Lens' NotebookInstanceSummary (Lude.Maybe InstanceType)
nisInstanceType = Lens.lens (instanceType :: NotebookInstanceSummary -> Lude.Maybe InstanceType) (\s a -> s {instanceType = a} :: NotebookInstanceSummary)
{-# DEPRECATED nisInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The status of the notebook instance.
--
-- /Note:/ Consider using 'notebookInstanceStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nisNotebookInstanceStatus :: Lens.Lens' NotebookInstanceSummary (Lude.Maybe NotebookInstanceStatus)
nisNotebookInstanceStatus = Lens.lens (notebookInstanceStatus :: NotebookInstanceSummary -> Lude.Maybe NotebookInstanceStatus) (\s a -> s {notebookInstanceStatus = a} :: NotebookInstanceSummary)
{-# DEPRECATED nisNotebookInstanceStatus "Use generic-lens or generic-optics with 'notebookInstanceStatus' instead." #-}

-- | The Git repository associated with the notebook instance as its default code repository. This can be either the name of a Git repository stored as a resource in your account, or the URL of a Git repository in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. When you open a notebook instance, it opens in the directory that contains this repository. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
--
-- /Note:/ Consider using 'defaultCodeRepository' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nisDefaultCodeRepository :: Lens.Lens' NotebookInstanceSummary (Lude.Maybe Lude.Text)
nisDefaultCodeRepository = Lens.lens (defaultCodeRepository :: NotebookInstanceSummary -> Lude.Maybe Lude.Text) (\s a -> s {defaultCodeRepository = a} :: NotebookInstanceSummary)
{-# DEPRECATED nisDefaultCodeRepository "Use generic-lens or generic-optics with 'defaultCodeRepository' instead." #-}

-- | The name of a notebook instance lifecycle configuration associated with this notebook instance.
--
-- For information about notebook instance lifestyle configurations, see <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance> .
--
-- /Note:/ Consider using 'notebookInstanceLifecycleConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nisNotebookInstanceLifecycleConfigName :: Lens.Lens' NotebookInstanceSummary (Lude.Maybe Lude.Text)
nisNotebookInstanceLifecycleConfigName = Lens.lens (notebookInstanceLifecycleConfigName :: NotebookInstanceSummary -> Lude.Maybe Lude.Text) (\s a -> s {notebookInstanceLifecycleConfigName = a} :: NotebookInstanceSummary)
{-# DEPRECATED nisNotebookInstanceLifecycleConfigName "Use generic-lens or generic-optics with 'notebookInstanceLifecycleConfigName' instead." #-}

-- | The name of the notebook instance that you want a summary for.
--
-- /Note:/ Consider using 'notebookInstanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nisNotebookInstanceName :: Lens.Lens' NotebookInstanceSummary Lude.Text
nisNotebookInstanceName = Lens.lens (notebookInstanceName :: NotebookInstanceSummary -> Lude.Text) (\s a -> s {notebookInstanceName = a} :: NotebookInstanceSummary)
{-# DEPRECATED nisNotebookInstanceName "Use generic-lens or generic-optics with 'notebookInstanceName' instead." #-}

-- | The Amazon Resource Name (ARN) of the notebook instance.
--
-- /Note:/ Consider using 'notebookInstanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nisNotebookInstanceARN :: Lens.Lens' NotebookInstanceSummary Lude.Text
nisNotebookInstanceARN = Lens.lens (notebookInstanceARN :: NotebookInstanceSummary -> Lude.Text) (\s a -> s {notebookInstanceARN = a} :: NotebookInstanceSummary)
{-# DEPRECATED nisNotebookInstanceARN "Use generic-lens or generic-optics with 'notebookInstanceARN' instead." #-}

instance Lude.FromJSON NotebookInstanceSummary where
  parseJSON =
    Lude.withObject
      "NotebookInstanceSummary"
      ( \x ->
          NotebookInstanceSummary'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "AdditionalCodeRepositories" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Url")
            Lude.<*> (x Lude..:? "LastModifiedTime")
            Lude.<*> (x Lude..:? "InstanceType")
            Lude.<*> (x Lude..:? "NotebookInstanceStatus")
            Lude.<*> (x Lude..:? "DefaultCodeRepository")
            Lude.<*> (x Lude..:? "NotebookInstanceLifecycleConfigName")
            Lude.<*> (x Lude..: "NotebookInstanceName")
            Lude.<*> (x Lude..: "NotebookInstanceArn")
      )
