{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.NotifyApplicationState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the migration state of an application. For a given application identified by the value passed to @ApplicationId@ , its status is set or updated by passing one of three values to @Status@ : @NOT_STARTED | IN_PROGRESS | COMPLETED@ .
module Network.AWS.MigrationHub.NotifyApplicationState
  ( -- * Creating a request
    NotifyApplicationState (..),
    mkNotifyApplicationState,

    -- ** Request lenses
    nasUpdateDateTime,
    nasDryRun,
    nasApplicationId,
    nasStatus,

    -- * Destructuring the response
    NotifyApplicationStateResponse (..),
    mkNotifyApplicationStateResponse,

    -- ** Response lenses
    nasrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkNotifyApplicationState' smart constructor.
data NotifyApplicationState = NotifyApplicationState'
  { updateDateTime ::
      Lude.Maybe Lude.Timestamp,
    dryRun :: Lude.Maybe Lude.Bool,
    applicationId :: Lude.Text,
    status :: ApplicationStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NotifyApplicationState' with the minimum fields required to make a request.
--
-- * 'applicationId' - The configurationId in Application Discovery Service that uniquely identifies the grouped application.
-- * 'dryRun' - Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
-- * 'status' - Status of the application - Not Started, In-Progress, Complete.
-- * 'updateDateTime' - The timestamp when the application state changed.
mkNotifyApplicationState ::
  -- | 'applicationId'
  Lude.Text ->
  -- | 'status'
  ApplicationStatus ->
  NotifyApplicationState
mkNotifyApplicationState pApplicationId_ pStatus_ =
  NotifyApplicationState'
    { updateDateTime = Lude.Nothing,
      dryRun = Lude.Nothing,
      applicationId = pApplicationId_,
      status = pStatus_
    }

-- | The timestamp when the application state changed.
--
-- /Note:/ Consider using 'updateDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nasUpdateDateTime :: Lens.Lens' NotifyApplicationState (Lude.Maybe Lude.Timestamp)
nasUpdateDateTime = Lens.lens (updateDateTime :: NotifyApplicationState -> Lude.Maybe Lude.Timestamp) (\s a -> s {updateDateTime = a} :: NotifyApplicationState)
{-# DEPRECATED nasUpdateDateTime "Use generic-lens or generic-optics with 'updateDateTime' instead." #-}

-- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nasDryRun :: Lens.Lens' NotifyApplicationState (Lude.Maybe Lude.Bool)
nasDryRun = Lens.lens (dryRun :: NotifyApplicationState -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: NotifyApplicationState)
{-# DEPRECATED nasDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The configurationId in Application Discovery Service that uniquely identifies the grouped application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nasApplicationId :: Lens.Lens' NotifyApplicationState Lude.Text
nasApplicationId = Lens.lens (applicationId :: NotifyApplicationState -> Lude.Text) (\s a -> s {applicationId = a} :: NotifyApplicationState)
{-# DEPRECATED nasApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | Status of the application - Not Started, In-Progress, Complete.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nasStatus :: Lens.Lens' NotifyApplicationState ApplicationStatus
nasStatus = Lens.lens (status :: NotifyApplicationState -> ApplicationStatus) (\s a -> s {status = a} :: NotifyApplicationState)
{-# DEPRECATED nasStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.AWSRequest NotifyApplicationState where
  type Rs NotifyApplicationState = NotifyApplicationStateResponse
  request = Req.postJSON migrationHubService
  response =
    Res.receiveEmpty
      ( \s h x ->
          NotifyApplicationStateResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders NotifyApplicationState where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSMigrationHub.NotifyApplicationState" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON NotifyApplicationState where
  toJSON NotifyApplicationState' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("UpdateDateTime" Lude..=) Lude.<$> updateDateTime,
            ("DryRun" Lude..=) Lude.<$> dryRun,
            Lude.Just ("ApplicationId" Lude..= applicationId),
            Lude.Just ("Status" Lude..= status)
          ]
      )

instance Lude.ToPath NotifyApplicationState where
  toPath = Lude.const "/"

instance Lude.ToQuery NotifyApplicationState where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkNotifyApplicationStateResponse' smart constructor.
newtype NotifyApplicationStateResponse = NotifyApplicationStateResponse'
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

-- | Creates a value of 'NotifyApplicationStateResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkNotifyApplicationStateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  NotifyApplicationStateResponse
mkNotifyApplicationStateResponse pResponseStatus_ =
  NotifyApplicationStateResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nasrsResponseStatus :: Lens.Lens' NotifyApplicationStateResponse Lude.Int
nasrsResponseStatus = Lens.lens (responseStatus :: NotifyApplicationStateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: NotifyApplicationStateResponse)
{-# DEPRECATED nasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
