{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.DescribeMigrationTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of all attributes associated with a specific migration task.
module Network.AWS.MigrationHub.DescribeMigrationTask
  ( -- * Creating a request
    DescribeMigrationTask (..),
    mkDescribeMigrationTask,

    -- ** Request lenses
    dmtProgressUpdateStream,
    dmtMigrationTaskName,

    -- * Destructuring the response
    DescribeMigrationTaskResponse (..),
    mkDescribeMigrationTaskResponse,

    -- ** Response lenses
    dmtrsMigrationTask,
    dmtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeMigrationTask' smart constructor.
data DescribeMigrationTask = DescribeMigrationTask'
  { -- | The name of the ProgressUpdateStream.
    progressUpdateStream :: Lude.Text,
    -- | The identifier given to the MigrationTask. /Do not store personal data in this field./
    migrationTaskName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMigrationTask' with the minimum fields required to make a request.
--
-- * 'progressUpdateStream' - The name of the ProgressUpdateStream.
-- * 'migrationTaskName' - The identifier given to the MigrationTask. /Do not store personal data in this field./
mkDescribeMigrationTask ::
  -- | 'progressUpdateStream'
  Lude.Text ->
  -- | 'migrationTaskName'
  Lude.Text ->
  DescribeMigrationTask
mkDescribeMigrationTask pProgressUpdateStream_ pMigrationTaskName_ =
  DescribeMigrationTask'
    { progressUpdateStream =
        pProgressUpdateStream_,
      migrationTaskName = pMigrationTaskName_
    }

-- | The name of the ProgressUpdateStream.
--
-- /Note:/ Consider using 'progressUpdateStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmtProgressUpdateStream :: Lens.Lens' DescribeMigrationTask Lude.Text
dmtProgressUpdateStream = Lens.lens (progressUpdateStream :: DescribeMigrationTask -> Lude.Text) (\s a -> s {progressUpdateStream = a} :: DescribeMigrationTask)
{-# DEPRECATED dmtProgressUpdateStream "Use generic-lens or generic-optics with 'progressUpdateStream' instead." #-}

-- | The identifier given to the MigrationTask. /Do not store personal data in this field./
--
-- /Note:/ Consider using 'migrationTaskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmtMigrationTaskName :: Lens.Lens' DescribeMigrationTask Lude.Text
dmtMigrationTaskName = Lens.lens (migrationTaskName :: DescribeMigrationTask -> Lude.Text) (\s a -> s {migrationTaskName = a} :: DescribeMigrationTask)
{-# DEPRECATED dmtMigrationTaskName "Use generic-lens or generic-optics with 'migrationTaskName' instead." #-}

instance Lude.AWSRequest DescribeMigrationTask where
  type Rs DescribeMigrationTask = DescribeMigrationTaskResponse
  request = Req.postJSON migrationHubService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeMigrationTaskResponse'
            Lude.<$> (x Lude..?> "MigrationTask")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeMigrationTask where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSMigrationHub.DescribeMigrationTask" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeMigrationTask where
  toJSON DescribeMigrationTask' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ProgressUpdateStream" Lude..= progressUpdateStream),
            Lude.Just ("MigrationTaskName" Lude..= migrationTaskName)
          ]
      )

instance Lude.ToPath DescribeMigrationTask where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeMigrationTask where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeMigrationTaskResponse' smart constructor.
data DescribeMigrationTaskResponse = DescribeMigrationTaskResponse'
  { -- | Object encapsulating information about the migration task.
    migrationTask :: Lude.Maybe MigrationTask,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMigrationTaskResponse' with the minimum fields required to make a request.
--
-- * 'migrationTask' - Object encapsulating information about the migration task.
-- * 'responseStatus' - The response status code.
mkDescribeMigrationTaskResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeMigrationTaskResponse
mkDescribeMigrationTaskResponse pResponseStatus_ =
  DescribeMigrationTaskResponse'
    { migrationTask = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Object encapsulating information about the migration task.
--
-- /Note:/ Consider using 'migrationTask' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmtrsMigrationTask :: Lens.Lens' DescribeMigrationTaskResponse (Lude.Maybe MigrationTask)
dmtrsMigrationTask = Lens.lens (migrationTask :: DescribeMigrationTaskResponse -> Lude.Maybe MigrationTask) (\s a -> s {migrationTask = a} :: DescribeMigrationTaskResponse)
{-# DEPRECATED dmtrsMigrationTask "Use generic-lens or generic-optics with 'migrationTask' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmtrsResponseStatus :: Lens.Lens' DescribeMigrationTaskResponse Lude.Int
dmtrsResponseStatus = Lens.lens (responseStatus :: DescribeMigrationTaskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeMigrationTaskResponse)
{-# DEPRECATED dmtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
