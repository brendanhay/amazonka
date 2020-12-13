{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.LaunchDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.LaunchDetails
  ( LaunchDetails (..),

    -- * Smart constructor
    mkLaunchDetails,

    -- * Lenses
    ldStackId,
    ldLatestLaunchTime,
    ldStackName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details about the latest launch of an application.
--
-- /See:/ 'mkLaunchDetails' smart constructor.
data LaunchDetails = LaunchDetails'
  { -- | The ID of the latest stack launched for this application.
    stackId :: Lude.Maybe Lude.Text,
    -- | The latest time that this application was launched successfully.
    latestLaunchTime :: Lude.Maybe Lude.Timestamp,
    -- | The name of the latest stack launched for this application.
    stackName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchDetails' with the minimum fields required to make a request.
--
-- * 'stackId' - The ID of the latest stack launched for this application.
-- * 'latestLaunchTime' - The latest time that this application was launched successfully.
-- * 'stackName' - The name of the latest stack launched for this application.
mkLaunchDetails ::
  LaunchDetails
mkLaunchDetails =
  LaunchDetails'
    { stackId = Lude.Nothing,
      latestLaunchTime = Lude.Nothing,
      stackName = Lude.Nothing
    }

-- | The ID of the latest stack launched for this application.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldStackId :: Lens.Lens' LaunchDetails (Lude.Maybe Lude.Text)
ldStackId = Lens.lens (stackId :: LaunchDetails -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: LaunchDetails)
{-# DEPRECATED ldStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The latest time that this application was launched successfully.
--
-- /Note:/ Consider using 'latestLaunchTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldLatestLaunchTime :: Lens.Lens' LaunchDetails (Lude.Maybe Lude.Timestamp)
ldLatestLaunchTime = Lens.lens (latestLaunchTime :: LaunchDetails -> Lude.Maybe Lude.Timestamp) (\s a -> s {latestLaunchTime = a} :: LaunchDetails)
{-# DEPRECATED ldLatestLaunchTime "Use generic-lens or generic-optics with 'latestLaunchTime' instead." #-}

-- | The name of the latest stack launched for this application.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldStackName :: Lens.Lens' LaunchDetails (Lude.Maybe Lude.Text)
ldStackName = Lens.lens (stackName :: LaunchDetails -> Lude.Maybe Lude.Text) (\s a -> s {stackName = a} :: LaunchDetails)
{-# DEPRECATED ldStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Lude.FromJSON LaunchDetails where
  parseJSON =
    Lude.withObject
      "LaunchDetails"
      ( \x ->
          LaunchDetails'
            Lude.<$> (x Lude..:? "stackId")
            Lude.<*> (x Lude..:? "latestLaunchTime")
            Lude.<*> (x Lude..:? "stackName")
      )
