{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.CreateUnreferencedMergeCommit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an unreferenced commit that represents the result of merging two branches using a specified merge strategy. This can help you determine the outcome of a potential merge. This API cannot be used with the fast-forward merge strategy because that strategy does not create a merge commit.
module Network.AWS.CodeCommit.CreateUnreferencedMergeCommit
  ( -- * Creating a request
    CreateUnreferencedMergeCommit (..),
    mkCreateUnreferencedMergeCommit,

    -- ** Request lenses
    cumcEmail,
    cumcAuthorName,
    cumcConflictDetailLevel,
    cumcCommitMessage,
    cumcConflictResolution,
    cumcConflictResolutionStrategy,
    cumcKeepEmptyFolders,
    cumcRepositoryName,
    cumcSourceCommitSpecifier,
    cumcDestinationCommitSpecifier,
    cumcMergeOption,

    -- * Destructuring the response
    CreateUnreferencedMergeCommitResponse (..),
    mkCreateUnreferencedMergeCommitResponse,

    -- ** Response lenses
    cumcrsCommitId,
    cumcrsTreeId,
    cumcrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateUnreferencedMergeCommit' smart constructor.
data CreateUnreferencedMergeCommit = CreateUnreferencedMergeCommit'
  { email ::
      Lude.Maybe Lude.Text,
    authorName ::
      Lude.Maybe Lude.Text,
    conflictDetailLevel ::
      Lude.Maybe
        ConflictDetailLevelTypeEnum,
    commitMessage ::
      Lude.Maybe Lude.Text,
    conflictResolution ::
      Lude.Maybe ConflictResolution,
    conflictResolutionStrategy ::
      Lude.Maybe
        ConflictResolutionStrategyTypeEnum,
    keepEmptyFolders ::
      Lude.Maybe Lude.Bool,
    repositoryName :: Lude.Text,
    sourceCommitSpecifier ::
      Lude.Text,
    destinationCommitSpecifier ::
      Lude.Text,
    mergeOption ::
      MergeOptionTypeEnum
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateUnreferencedMergeCommit' with the minimum fields required to make a request.
--
-- * 'authorName' - The name of the author who created the unreferenced commit. This information is used as both the author and committer for the commit.
-- * 'commitMessage' - The commit message for the unreferenced commit.
-- * 'conflictDetailLevel' - The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
-- * 'conflictResolution' - If AUTOMERGE is the conflict resolution strategy, a list of inputs to use when resolving conflicts during a merge.
-- * 'conflictResolutionStrategy' - Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
-- * 'destinationCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
-- * 'email' - The email address for the person who created the unreferenced commit.
-- * 'keepEmptyFolders' - If the commit contains deletions, whether to keep a folder or folder structure if the changes leave the folders empty. If this is specified as true, a .gitkeep file is created for empty folders. The default is false.
-- * 'mergeOption' - The merge option or strategy you want to use to merge the code.
-- * 'repositoryName' - The name of the repository where you want to create the unreferenced merge commit.
-- * 'sourceCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
mkCreateUnreferencedMergeCommit ::
  -- | 'repositoryName'
  Lude.Text ->
  -- | 'sourceCommitSpecifier'
  Lude.Text ->
  -- | 'destinationCommitSpecifier'
  Lude.Text ->
  -- | 'mergeOption'
  MergeOptionTypeEnum ->
  CreateUnreferencedMergeCommit
mkCreateUnreferencedMergeCommit
  pRepositoryName_
  pSourceCommitSpecifier_
  pDestinationCommitSpecifier_
  pMergeOption_ =
    CreateUnreferencedMergeCommit'
      { email = Lude.Nothing,
        authorName = Lude.Nothing,
        conflictDetailLevel = Lude.Nothing,
        commitMessage = Lude.Nothing,
        conflictResolution = Lude.Nothing,
        conflictResolutionStrategy = Lude.Nothing,
        keepEmptyFolders = Lude.Nothing,
        repositoryName = pRepositoryName_,
        sourceCommitSpecifier = pSourceCommitSpecifier_,
        destinationCommitSpecifier = pDestinationCommitSpecifier_,
        mergeOption = pMergeOption_
      }

-- | The email address for the person who created the unreferenced commit.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cumcEmail :: Lens.Lens' CreateUnreferencedMergeCommit (Lude.Maybe Lude.Text)
cumcEmail = Lens.lens (email :: CreateUnreferencedMergeCommit -> Lude.Maybe Lude.Text) (\s a -> s {email = a} :: CreateUnreferencedMergeCommit)
{-# DEPRECATED cumcEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | The name of the author who created the unreferenced commit. This information is used as both the author and committer for the commit.
--
-- /Note:/ Consider using 'authorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cumcAuthorName :: Lens.Lens' CreateUnreferencedMergeCommit (Lude.Maybe Lude.Text)
cumcAuthorName = Lens.lens (authorName :: CreateUnreferencedMergeCommit -> Lude.Maybe Lude.Text) (\s a -> s {authorName = a} :: CreateUnreferencedMergeCommit)
{-# DEPRECATED cumcAuthorName "Use generic-lens or generic-optics with 'authorName' instead." #-}

-- | The level of conflict detail to use. If unspecified, the default FILE_LEVEL is used, which returns a not-mergeable result if the same file has differences in both branches. If LINE_LEVEL is specified, a conflict is considered not mergeable if the same file in both branches has differences on the same line.
--
-- /Note:/ Consider using 'conflictDetailLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cumcConflictDetailLevel :: Lens.Lens' CreateUnreferencedMergeCommit (Lude.Maybe ConflictDetailLevelTypeEnum)
cumcConflictDetailLevel = Lens.lens (conflictDetailLevel :: CreateUnreferencedMergeCommit -> Lude.Maybe ConflictDetailLevelTypeEnum) (\s a -> s {conflictDetailLevel = a} :: CreateUnreferencedMergeCommit)
{-# DEPRECATED cumcConflictDetailLevel "Use generic-lens or generic-optics with 'conflictDetailLevel' instead." #-}

-- | The commit message for the unreferenced commit.
--
-- /Note:/ Consider using 'commitMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cumcCommitMessage :: Lens.Lens' CreateUnreferencedMergeCommit (Lude.Maybe Lude.Text)
cumcCommitMessage = Lens.lens (commitMessage :: CreateUnreferencedMergeCommit -> Lude.Maybe Lude.Text) (\s a -> s {commitMessage = a} :: CreateUnreferencedMergeCommit)
{-# DEPRECATED cumcCommitMessage "Use generic-lens or generic-optics with 'commitMessage' instead." #-}

-- | If AUTOMERGE is the conflict resolution strategy, a list of inputs to use when resolving conflicts during a merge.
--
-- /Note:/ Consider using 'conflictResolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cumcConflictResolution :: Lens.Lens' CreateUnreferencedMergeCommit (Lude.Maybe ConflictResolution)
cumcConflictResolution = Lens.lens (conflictResolution :: CreateUnreferencedMergeCommit -> Lude.Maybe ConflictResolution) (\s a -> s {conflictResolution = a} :: CreateUnreferencedMergeCommit)
{-# DEPRECATED cumcConflictResolution "Use generic-lens or generic-optics with 'conflictResolution' instead." #-}

-- | Specifies which branch to use when resolving conflicts, or whether to attempt automatically merging two versions of a file. The default is NONE, which requires any conflicts to be resolved manually before the merge operation is successful.
--
-- /Note:/ Consider using 'conflictResolutionStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cumcConflictResolutionStrategy :: Lens.Lens' CreateUnreferencedMergeCommit (Lude.Maybe ConflictResolutionStrategyTypeEnum)
cumcConflictResolutionStrategy = Lens.lens (conflictResolutionStrategy :: CreateUnreferencedMergeCommit -> Lude.Maybe ConflictResolutionStrategyTypeEnum) (\s a -> s {conflictResolutionStrategy = a} :: CreateUnreferencedMergeCommit)
{-# DEPRECATED cumcConflictResolutionStrategy "Use generic-lens or generic-optics with 'conflictResolutionStrategy' instead." #-}

-- | If the commit contains deletions, whether to keep a folder or folder structure if the changes leave the folders empty. If this is specified as true, a .gitkeep file is created for empty folders. The default is false.
--
-- /Note:/ Consider using 'keepEmptyFolders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cumcKeepEmptyFolders :: Lens.Lens' CreateUnreferencedMergeCommit (Lude.Maybe Lude.Bool)
cumcKeepEmptyFolders = Lens.lens (keepEmptyFolders :: CreateUnreferencedMergeCommit -> Lude.Maybe Lude.Bool) (\s a -> s {keepEmptyFolders = a} :: CreateUnreferencedMergeCommit)
{-# DEPRECATED cumcKeepEmptyFolders "Use generic-lens or generic-optics with 'keepEmptyFolders' instead." #-}

-- | The name of the repository where you want to create the unreferenced merge commit.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cumcRepositoryName :: Lens.Lens' CreateUnreferencedMergeCommit Lude.Text
cumcRepositoryName = Lens.lens (repositoryName :: CreateUnreferencedMergeCommit -> Lude.Text) (\s a -> s {repositoryName = a} :: CreateUnreferencedMergeCommit)
{-# DEPRECATED cumcRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'sourceCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cumcSourceCommitSpecifier :: Lens.Lens' CreateUnreferencedMergeCommit Lude.Text
cumcSourceCommitSpecifier = Lens.lens (sourceCommitSpecifier :: CreateUnreferencedMergeCommit -> Lude.Text) (\s a -> s {sourceCommitSpecifier = a} :: CreateUnreferencedMergeCommit)
{-# DEPRECATED cumcSourceCommitSpecifier "Use generic-lens or generic-optics with 'sourceCommitSpecifier' instead." #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'destinationCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cumcDestinationCommitSpecifier :: Lens.Lens' CreateUnreferencedMergeCommit Lude.Text
cumcDestinationCommitSpecifier = Lens.lens (destinationCommitSpecifier :: CreateUnreferencedMergeCommit -> Lude.Text) (\s a -> s {destinationCommitSpecifier = a} :: CreateUnreferencedMergeCommit)
{-# DEPRECATED cumcDestinationCommitSpecifier "Use generic-lens or generic-optics with 'destinationCommitSpecifier' instead." #-}

-- | The merge option or strategy you want to use to merge the code.
--
-- /Note:/ Consider using 'mergeOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cumcMergeOption :: Lens.Lens' CreateUnreferencedMergeCommit MergeOptionTypeEnum
cumcMergeOption = Lens.lens (mergeOption :: CreateUnreferencedMergeCommit -> MergeOptionTypeEnum) (\s a -> s {mergeOption = a} :: CreateUnreferencedMergeCommit)
{-# DEPRECATED cumcMergeOption "Use generic-lens or generic-optics with 'mergeOption' instead." #-}

instance Lude.AWSRequest CreateUnreferencedMergeCommit where
  type
    Rs CreateUnreferencedMergeCommit =
      CreateUnreferencedMergeCommitResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateUnreferencedMergeCommitResponse'
            Lude.<$> (x Lude..?> "commitId")
            Lude.<*> (x Lude..?> "treeId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateUnreferencedMergeCommit where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeCommit_20150413.CreateUnreferencedMergeCommit" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateUnreferencedMergeCommit where
  toJSON CreateUnreferencedMergeCommit' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("email" Lude..=) Lude.<$> email,
            ("authorName" Lude..=) Lude.<$> authorName,
            ("conflictDetailLevel" Lude..=) Lude.<$> conflictDetailLevel,
            ("commitMessage" Lude..=) Lude.<$> commitMessage,
            ("conflictResolution" Lude..=) Lude.<$> conflictResolution,
            ("conflictResolutionStrategy" Lude..=)
              Lude.<$> conflictResolutionStrategy,
            ("keepEmptyFolders" Lude..=) Lude.<$> keepEmptyFolders,
            Lude.Just ("repositoryName" Lude..= repositoryName),
            Lude.Just ("sourceCommitSpecifier" Lude..= sourceCommitSpecifier),
            Lude.Just
              ("destinationCommitSpecifier" Lude..= destinationCommitSpecifier),
            Lude.Just ("mergeOption" Lude..= mergeOption)
          ]
      )

instance Lude.ToPath CreateUnreferencedMergeCommit where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateUnreferencedMergeCommit where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateUnreferencedMergeCommitResponse' smart constructor.
data CreateUnreferencedMergeCommitResponse = CreateUnreferencedMergeCommitResponse'
  { commitId ::
      Lude.Maybe
        Lude.Text,
    treeId ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateUnreferencedMergeCommitResponse' with the minimum fields required to make a request.
--
-- * 'commitId' - The full commit ID of the commit that contains your merge results.
-- * 'responseStatus' - The response status code.
-- * 'treeId' - The full SHA-1 pointer of the tree information for the commit that contains the merge results.
mkCreateUnreferencedMergeCommitResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateUnreferencedMergeCommitResponse
mkCreateUnreferencedMergeCommitResponse pResponseStatus_ =
  CreateUnreferencedMergeCommitResponse'
    { commitId = Lude.Nothing,
      treeId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The full commit ID of the commit that contains your merge results.
--
-- /Note:/ Consider using 'commitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cumcrsCommitId :: Lens.Lens' CreateUnreferencedMergeCommitResponse (Lude.Maybe Lude.Text)
cumcrsCommitId = Lens.lens (commitId :: CreateUnreferencedMergeCommitResponse -> Lude.Maybe Lude.Text) (\s a -> s {commitId = a} :: CreateUnreferencedMergeCommitResponse)
{-# DEPRECATED cumcrsCommitId "Use generic-lens or generic-optics with 'commitId' instead." #-}

-- | The full SHA-1 pointer of the tree information for the commit that contains the merge results.
--
-- /Note:/ Consider using 'treeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cumcrsTreeId :: Lens.Lens' CreateUnreferencedMergeCommitResponse (Lude.Maybe Lude.Text)
cumcrsTreeId = Lens.lens (treeId :: CreateUnreferencedMergeCommitResponse -> Lude.Maybe Lude.Text) (\s a -> s {treeId = a} :: CreateUnreferencedMergeCommitResponse)
{-# DEPRECATED cumcrsTreeId "Use generic-lens or generic-optics with 'treeId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cumcrsResponseStatus :: Lens.Lens' CreateUnreferencedMergeCommitResponse Lude.Int
cumcrsResponseStatus = Lens.lens (responseStatus :: CreateUnreferencedMergeCommitResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateUnreferencedMergeCommitResponse)
{-# DEPRECATED cumcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
