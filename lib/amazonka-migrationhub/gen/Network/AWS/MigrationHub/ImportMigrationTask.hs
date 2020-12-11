{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.ImportMigrationTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a new migration task which represents a server, database, etc., being migrated to AWS by a migration tool.
--
-- This API is a prerequisite to calling the @NotifyMigrationTaskState@ API as the migration tool must first register the migration task with Migration Hub.
module Network.AWS.MigrationHub.ImportMigrationTask
  ( -- * Creating a request
    ImportMigrationTask (..),
    mkImportMigrationTask,

    -- ** Request lenses
    imtDryRun,
    imtProgressUpdateStream,
    imtMigrationTaskName,

    -- * Destructuring the response
    ImportMigrationTaskResponse (..),
    mkImportMigrationTaskResponse,

    -- ** Response lenses
    imtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkImportMigrationTask' smart constructor.
data ImportMigrationTask = ImportMigrationTask'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    progressUpdateStream :: Lude.Text,
    migrationTaskName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportMigrationTask' with the minimum fields required to make a request.
--
-- * 'dryRun' - Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
-- * 'migrationTaskName' - Unique identifier that references the migration task. /Do not store personal data in this field./
-- * 'progressUpdateStream' - The name of the ProgressUpdateStream. >
mkImportMigrationTask ::
  -- | 'progressUpdateStream'
  Lude.Text ->
  -- | 'migrationTaskName'
  Lude.Text ->
  ImportMigrationTask
mkImportMigrationTask pProgressUpdateStream_ pMigrationTaskName_ =
  ImportMigrationTask'
    { dryRun = Lude.Nothing,
      progressUpdateStream = pProgressUpdateStream_,
      migrationTaskName = pMigrationTaskName_
    }

-- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imtDryRun :: Lens.Lens' ImportMigrationTask (Lude.Maybe Lude.Bool)
imtDryRun = Lens.lens (dryRun :: ImportMigrationTask -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ImportMigrationTask)
{-# DEPRECATED imtDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The name of the ProgressUpdateStream. >
--
-- /Note:/ Consider using 'progressUpdateStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imtProgressUpdateStream :: Lens.Lens' ImportMigrationTask Lude.Text
imtProgressUpdateStream = Lens.lens (progressUpdateStream :: ImportMigrationTask -> Lude.Text) (\s a -> s {progressUpdateStream = a} :: ImportMigrationTask)
{-# DEPRECATED imtProgressUpdateStream "Use generic-lens or generic-optics with 'progressUpdateStream' instead." #-}

-- | Unique identifier that references the migration task. /Do not store personal data in this field./
--
-- /Note:/ Consider using 'migrationTaskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imtMigrationTaskName :: Lens.Lens' ImportMigrationTask Lude.Text
imtMigrationTaskName = Lens.lens (migrationTaskName :: ImportMigrationTask -> Lude.Text) (\s a -> s {migrationTaskName = a} :: ImportMigrationTask)
{-# DEPRECATED imtMigrationTaskName "Use generic-lens or generic-optics with 'migrationTaskName' instead." #-}

instance Lude.AWSRequest ImportMigrationTask where
  type Rs ImportMigrationTask = ImportMigrationTaskResponse
  request = Req.postJSON migrationHubService
  response =
    Res.receiveEmpty
      ( \s h x ->
          ImportMigrationTaskResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ImportMigrationTask where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSMigrationHub.ImportMigrationTask" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ImportMigrationTask where
  toJSON ImportMigrationTask' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DryRun" Lude..=) Lude.<$> dryRun,
            Lude.Just ("ProgressUpdateStream" Lude..= progressUpdateStream),
            Lude.Just ("MigrationTaskName" Lude..= migrationTaskName)
          ]
      )

instance Lude.ToPath ImportMigrationTask where
  toPath = Lude.const "/"

instance Lude.ToQuery ImportMigrationTask where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkImportMigrationTaskResponse' smart constructor.
newtype ImportMigrationTaskResponse = ImportMigrationTaskResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportMigrationTaskResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkImportMigrationTaskResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ImportMigrationTaskResponse
mkImportMigrationTaskResponse pResponseStatus_ =
  ImportMigrationTaskResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imtrsResponseStatus :: Lens.Lens' ImportMigrationTaskResponse Lude.Int
imtrsResponseStatus = Lens.lens (responseStatus :: ImportMigrationTaskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ImportMigrationTaskResponse)
{-# DEPRECATED imtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
