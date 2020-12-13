{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.Types.ApplicationState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MigrationHub.Types.ApplicationState
  ( ApplicationState (..),

    -- * Smart constructor
    mkApplicationState,

    -- * Lenses
    asLastUpdatedTime,
    asApplicationId,
    asApplicationStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types.ApplicationStatus
import qualified Network.AWS.Prelude as Lude

-- | The state of an application discovered through Migration Hub import, the AWS Agentless Discovery Connector, or the AWS Application Discovery Agent.
--
-- /See:/ 'mkApplicationState' smart constructor.
data ApplicationState = ApplicationState'
  { -- | The timestamp when the application status was last updated.
    lastUpdatedTime :: Lude.Maybe Lude.Timestamp,
    -- | The configurationId from the Application Discovery Service that uniquely identifies an application.
    applicationId :: Lude.Maybe Lude.Text,
    -- | The current status of an application.
    applicationStatus :: Lude.Maybe ApplicationStatus
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ApplicationState' with the minimum fields required to make a request.
--
-- * 'lastUpdatedTime' - The timestamp when the application status was last updated.
-- * 'applicationId' - The configurationId from the Application Discovery Service that uniquely identifies an application.
-- * 'applicationStatus' - The current status of an application.
mkApplicationState ::
  ApplicationState
mkApplicationState =
  ApplicationState'
    { lastUpdatedTime = Lude.Nothing,
      applicationId = Lude.Nothing,
      applicationStatus = Lude.Nothing
    }

-- | The timestamp when the application status was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asLastUpdatedTime :: Lens.Lens' ApplicationState (Lude.Maybe Lude.Timestamp)
asLastUpdatedTime = Lens.lens (lastUpdatedTime :: ApplicationState -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedTime = a} :: ApplicationState)
{-# DEPRECATED asLastUpdatedTime "Use generic-lens or generic-optics with 'lastUpdatedTime' instead." #-}

-- | The configurationId from the Application Discovery Service that uniquely identifies an application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asApplicationId :: Lens.Lens' ApplicationState (Lude.Maybe Lude.Text)
asApplicationId = Lens.lens (applicationId :: ApplicationState -> Lude.Maybe Lude.Text) (\s a -> s {applicationId = a} :: ApplicationState)
{-# DEPRECATED asApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The current status of an application.
--
-- /Note:/ Consider using 'applicationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asApplicationStatus :: Lens.Lens' ApplicationState (Lude.Maybe ApplicationStatus)
asApplicationStatus = Lens.lens (applicationStatus :: ApplicationState -> Lude.Maybe ApplicationStatus) (\s a -> s {applicationStatus = a} :: ApplicationState)
{-# DEPRECATED asApplicationStatus "Use generic-lens or generic-optics with 'applicationStatus' instead." #-}

instance Lude.FromJSON ApplicationState where
  parseJSON =
    Lude.withObject
      "ApplicationState"
      ( \x ->
          ApplicationState'
            Lude.<$> (x Lude..:? "LastUpdatedTime")
            Lude.<*> (x Lude..:? "ApplicationId")
            Lude.<*> (x Lude..:? "ApplicationStatus")
      )
