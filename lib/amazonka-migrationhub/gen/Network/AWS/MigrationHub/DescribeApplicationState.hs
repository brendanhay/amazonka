{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.DescribeApplicationState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the migration status of an application.
module Network.AWS.MigrationHub.DescribeApplicationState
  ( -- * Creating a request
    DescribeApplicationState (..),
    mkDescribeApplicationState,

    -- ** Request lenses
    dasApplicationId,

    -- * Destructuring the response
    DescribeApplicationStateResponse (..),
    mkDescribeApplicationStateResponse,

    -- ** Response lenses
    dasrsLastUpdatedTime,
    dasrsApplicationStatus,
    dasrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeApplicationState' smart constructor.
newtype DescribeApplicationState = DescribeApplicationState'
  { applicationId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeApplicationState' with the minimum fields required to make a request.
--
-- * 'applicationId' - The configurationId in Application Discovery Service that uniquely identifies the grouped application.
mkDescribeApplicationState ::
  -- | 'applicationId'
  Lude.Text ->
  DescribeApplicationState
mkDescribeApplicationState pApplicationId_ =
  DescribeApplicationState' {applicationId = pApplicationId_}

-- | The configurationId in Application Discovery Service that uniquely identifies the grouped application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasApplicationId :: Lens.Lens' DescribeApplicationState Lude.Text
dasApplicationId = Lens.lens (applicationId :: DescribeApplicationState -> Lude.Text) (\s a -> s {applicationId = a} :: DescribeApplicationState)
{-# DEPRECATED dasApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest DescribeApplicationState where
  type Rs DescribeApplicationState = DescribeApplicationStateResponse
  request = Req.postJSON migrationHubService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeApplicationStateResponse'
            Lude.<$> (x Lude..?> "LastUpdatedTime")
            Lude.<*> (x Lude..?> "ApplicationStatus")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeApplicationState where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSMigrationHub.DescribeApplicationState" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeApplicationState where
  toJSON DescribeApplicationState' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ApplicationId" Lude..= applicationId)]
      )

instance Lude.ToPath DescribeApplicationState where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeApplicationState where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeApplicationStateResponse' smart constructor.
data DescribeApplicationStateResponse = DescribeApplicationStateResponse'
  { lastUpdatedTime ::
      Lude.Maybe Lude.Timestamp,
    applicationStatus ::
      Lude.Maybe
        ApplicationStatus,
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

-- | Creates a value of 'DescribeApplicationStateResponse' with the minimum fields required to make a request.
--
-- * 'applicationStatus' - Status of the application - Not Started, In-Progress, Complete.
-- * 'lastUpdatedTime' - The timestamp when the application status was last updated.
-- * 'responseStatus' - The response status code.
mkDescribeApplicationStateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeApplicationStateResponse
mkDescribeApplicationStateResponse pResponseStatus_ =
  DescribeApplicationStateResponse'
    { lastUpdatedTime = Lude.Nothing,
      applicationStatus = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The timestamp when the application status was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrsLastUpdatedTime :: Lens.Lens' DescribeApplicationStateResponse (Lude.Maybe Lude.Timestamp)
dasrsLastUpdatedTime = Lens.lens (lastUpdatedTime :: DescribeApplicationStateResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedTime = a} :: DescribeApplicationStateResponse)
{-# DEPRECATED dasrsLastUpdatedTime "Use generic-lens or generic-optics with 'lastUpdatedTime' instead." #-}

-- | Status of the application - Not Started, In-Progress, Complete.
--
-- /Note:/ Consider using 'applicationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrsApplicationStatus :: Lens.Lens' DescribeApplicationStateResponse (Lude.Maybe ApplicationStatus)
dasrsApplicationStatus = Lens.lens (applicationStatus :: DescribeApplicationStateResponse -> Lude.Maybe ApplicationStatus) (\s a -> s {applicationStatus = a} :: DescribeApplicationStateResponse)
{-# DEPRECATED dasrsApplicationStatus "Use generic-lens or generic-optics with 'applicationStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrsResponseStatus :: Lens.Lens' DescribeApplicationStateResponse Lude.Int
dasrsResponseStatus = Lens.lens (responseStatus :: DescribeApplicationStateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeApplicationStateResponse)
{-# DEPRECATED dasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
