{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.TestRepositoryTriggers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tests the functionality of repository triggers by sending information to the trigger target. If real data is available in the repository, the test sends data from the last commit. If no data is available, sample data is generated.
module Network.AWS.CodeCommit.TestRepositoryTriggers
  ( -- * Creating a request
    TestRepositoryTriggers (..),
    mkTestRepositoryTriggers,

    -- ** Request lenses
    trtRepositoryName,
    trtTriggers,

    -- * Destructuring the response
    TestRepositoryTriggersResponse (..),
    mkTestRepositoryTriggersResponse,

    -- ** Response lenses
    trtrsFailedExecutions,
    trtrsSuccessfulExecutions,
    trtrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a test repository triggers operation.
--
-- /See:/ 'mkTestRepositoryTriggers' smart constructor.
data TestRepositoryTriggers = TestRepositoryTriggers'
  { repositoryName ::
      Lude.Text,
    triggers :: [RepositoryTrigger]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TestRepositoryTriggers' with the minimum fields required to make a request.
--
-- * 'repositoryName' - The name of the repository in which to test the triggers.
-- * 'triggers' - The list of triggers to test.
mkTestRepositoryTriggers ::
  -- | 'repositoryName'
  Lude.Text ->
  TestRepositoryTriggers
mkTestRepositoryTriggers pRepositoryName_ =
  TestRepositoryTriggers'
    { repositoryName = pRepositoryName_,
      triggers = Lude.mempty
    }

-- | The name of the repository in which to test the triggers.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trtRepositoryName :: Lens.Lens' TestRepositoryTriggers Lude.Text
trtRepositoryName = Lens.lens (repositoryName :: TestRepositoryTriggers -> Lude.Text) (\s a -> s {repositoryName = a} :: TestRepositoryTriggers)
{-# DEPRECATED trtRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The list of triggers to test.
--
-- /Note:/ Consider using 'triggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trtTriggers :: Lens.Lens' TestRepositoryTriggers [RepositoryTrigger]
trtTriggers = Lens.lens (triggers :: TestRepositoryTriggers -> [RepositoryTrigger]) (\s a -> s {triggers = a} :: TestRepositoryTriggers)
{-# DEPRECATED trtTriggers "Use generic-lens or generic-optics with 'triggers' instead." #-}

instance Lude.AWSRequest TestRepositoryTriggers where
  type Rs TestRepositoryTriggers = TestRepositoryTriggersResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          TestRepositoryTriggersResponse'
            Lude.<$> (x Lude..?> "failedExecutions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "successfulExecutions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders TestRepositoryTriggers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.TestRepositoryTriggers" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON TestRepositoryTriggers where
  toJSON TestRepositoryTriggers' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("repositoryName" Lude..= repositoryName),
            Lude.Just ("triggers" Lude..= triggers)
          ]
      )

instance Lude.ToPath TestRepositoryTriggers where
  toPath = Lude.const "/"

instance Lude.ToQuery TestRepositoryTriggers where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a test repository triggers operation.
--
-- /See:/ 'mkTestRepositoryTriggersResponse' smart constructor.
data TestRepositoryTriggersResponse = TestRepositoryTriggersResponse'
  { failedExecutions ::
      Lude.Maybe
        [RepositoryTriggerExecutionFailure],
    successfulExecutions ::
      Lude.Maybe [Lude.Text],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TestRepositoryTriggersResponse' with the minimum fields required to make a request.
--
-- * 'failedExecutions' - The list of triggers that were not tested. This list provides the names of the triggers that could not be tested, separated by commas.
-- * 'responseStatus' - The response status code.
-- * 'successfulExecutions' - The list of triggers that were successfully tested. This list provides the names of the triggers that were successfully tested, separated by commas.
mkTestRepositoryTriggersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  TestRepositoryTriggersResponse
mkTestRepositoryTriggersResponse pResponseStatus_ =
  TestRepositoryTriggersResponse'
    { failedExecutions = Lude.Nothing,
      successfulExecutions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of triggers that were not tested. This list provides the names of the triggers that could not be tested, separated by commas.
--
-- /Note:/ Consider using 'failedExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trtrsFailedExecutions :: Lens.Lens' TestRepositoryTriggersResponse (Lude.Maybe [RepositoryTriggerExecutionFailure])
trtrsFailedExecutions = Lens.lens (failedExecutions :: TestRepositoryTriggersResponse -> Lude.Maybe [RepositoryTriggerExecutionFailure]) (\s a -> s {failedExecutions = a} :: TestRepositoryTriggersResponse)
{-# DEPRECATED trtrsFailedExecutions "Use generic-lens or generic-optics with 'failedExecutions' instead." #-}

-- | The list of triggers that were successfully tested. This list provides the names of the triggers that were successfully tested, separated by commas.
--
-- /Note:/ Consider using 'successfulExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trtrsSuccessfulExecutions :: Lens.Lens' TestRepositoryTriggersResponse (Lude.Maybe [Lude.Text])
trtrsSuccessfulExecutions = Lens.lens (successfulExecutions :: TestRepositoryTriggersResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {successfulExecutions = a} :: TestRepositoryTriggersResponse)
{-# DEPRECATED trtrsSuccessfulExecutions "Use generic-lens or generic-optics with 'successfulExecutions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trtrsResponseStatus :: Lens.Lens' TestRepositoryTriggersResponse Lude.Int
trtrsResponseStatus = Lens.lens (responseStatus :: TestRepositoryTriggersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: TestRepositoryTriggersResponse)
{-# DEPRECATED trtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
