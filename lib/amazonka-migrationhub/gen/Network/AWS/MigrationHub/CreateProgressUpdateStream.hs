{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.CreateProgressUpdateStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a progress update stream which is an AWS resource used for access control as well as a namespace for migration task names that is implicitly linked to your AWS account. It must uniquely identify the migration tool as it is used for all updates made by the tool; however, it does not need to be unique for each AWS account because it is scoped to the AWS account.
module Network.AWS.MigrationHub.CreateProgressUpdateStream
  ( -- * Creating a request
    CreateProgressUpdateStream (..),
    mkCreateProgressUpdateStream,

    -- ** Request lenses
    cpusProgressUpdateStreamName,
    cpusDryRun,

    -- * Destructuring the response
    CreateProgressUpdateStreamResponse (..),
    mkCreateProgressUpdateStreamResponse,

    -- ** Response lenses
    cpusrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateProgressUpdateStream' smart constructor.
data CreateProgressUpdateStream = CreateProgressUpdateStream'
  { -- | The name of the ProgressUpdateStream. /Do not store personal data in this field./
    progressUpdateStreamName :: Lude.Text,
    -- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateProgressUpdateStream' with the minimum fields required to make a request.
--
-- * 'progressUpdateStreamName' - The name of the ProgressUpdateStream. /Do not store personal data in this field./
-- * 'dryRun' - Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
mkCreateProgressUpdateStream ::
  -- | 'progressUpdateStreamName'
  Lude.Text ->
  CreateProgressUpdateStream
mkCreateProgressUpdateStream pProgressUpdateStreamName_ =
  CreateProgressUpdateStream'
    { progressUpdateStreamName =
        pProgressUpdateStreamName_,
      dryRun = Lude.Nothing
    }

-- | The name of the ProgressUpdateStream. /Do not store personal data in this field./
--
-- /Note:/ Consider using 'progressUpdateStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpusProgressUpdateStreamName :: Lens.Lens' CreateProgressUpdateStream Lude.Text
cpusProgressUpdateStreamName = Lens.lens (progressUpdateStreamName :: CreateProgressUpdateStream -> Lude.Text) (\s a -> s {progressUpdateStreamName = a} :: CreateProgressUpdateStream)
{-# DEPRECATED cpusProgressUpdateStreamName "Use generic-lens or generic-optics with 'progressUpdateStreamName' instead." #-}

-- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpusDryRun :: Lens.Lens' CreateProgressUpdateStream (Lude.Maybe Lude.Bool)
cpusDryRun = Lens.lens (dryRun :: CreateProgressUpdateStream -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateProgressUpdateStream)
{-# DEPRECATED cpusDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest CreateProgressUpdateStream where
  type
    Rs CreateProgressUpdateStream =
      CreateProgressUpdateStreamResponse
  request = Req.postJSON migrationHubService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CreateProgressUpdateStreamResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateProgressUpdateStream where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSMigrationHub.CreateProgressUpdateStream" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateProgressUpdateStream where
  toJSON CreateProgressUpdateStream' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("ProgressUpdateStreamName" Lude..= progressUpdateStreamName),
            ("DryRun" Lude..=) Lude.<$> dryRun
          ]
      )

instance Lude.ToPath CreateProgressUpdateStream where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateProgressUpdateStream where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateProgressUpdateStreamResponse' smart constructor.
newtype CreateProgressUpdateStreamResponse = CreateProgressUpdateStreamResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateProgressUpdateStreamResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateProgressUpdateStreamResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateProgressUpdateStreamResponse
mkCreateProgressUpdateStreamResponse pResponseStatus_ =
  CreateProgressUpdateStreamResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpusrsResponseStatus :: Lens.Lens' CreateProgressUpdateStreamResponse Lude.Int
cpusrsResponseStatus = Lens.lens (responseStatus :: CreateProgressUpdateStreamResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateProgressUpdateStreamResponse)
{-# DEPRECATED cpusrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
