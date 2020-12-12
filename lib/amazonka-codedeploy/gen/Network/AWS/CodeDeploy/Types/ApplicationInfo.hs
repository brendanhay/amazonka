{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.ApplicationInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.ApplicationInfo
  ( ApplicationInfo (..),

    -- * Smart constructor
    mkApplicationInfo,

    -- * Lenses
    aiLinkedToGitHub,
    aiComputePlatform,
    aiApplicationId,
    aiApplicationName,
    aiGitHubAccountName,
    aiCreateTime,
  )
where

import Network.AWS.CodeDeploy.Types.ComputePlatform
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about an application.
--
-- /See:/ 'mkApplicationInfo' smart constructor.
data ApplicationInfo = ApplicationInfo'
  { linkedToGitHub ::
      Lude.Maybe Lude.Bool,
    computePlatform :: Lude.Maybe ComputePlatform,
    applicationId :: Lude.Maybe Lude.Text,
    applicationName :: Lude.Maybe Lude.Text,
    gitHubAccountName :: Lude.Maybe Lude.Text,
    createTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ApplicationInfo' with the minimum fields required to make a request.
--
-- * 'applicationId' - The application ID.
-- * 'applicationName' - The application name.
-- * 'computePlatform' - The destination platform type for deployment of the application (@Lambda@ or @Server@ ).
-- * 'createTime' - The time at which the application was created.
-- * 'gitHubAccountName' - The name for a connection to a GitHub account.
-- * 'linkedToGitHub' - True if the user has authenticated with GitHub for the specified application. Otherwise, false.
mkApplicationInfo ::
  ApplicationInfo
mkApplicationInfo =
  ApplicationInfo'
    { linkedToGitHub = Lude.Nothing,
      computePlatform = Lude.Nothing,
      applicationId = Lude.Nothing,
      applicationName = Lude.Nothing,
      gitHubAccountName = Lude.Nothing,
      createTime = Lude.Nothing
    }

-- | True if the user has authenticated with GitHub for the specified application. Otherwise, false.
--
-- /Note:/ Consider using 'linkedToGitHub' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiLinkedToGitHub :: Lens.Lens' ApplicationInfo (Lude.Maybe Lude.Bool)
aiLinkedToGitHub = Lens.lens (linkedToGitHub :: ApplicationInfo -> Lude.Maybe Lude.Bool) (\s a -> s {linkedToGitHub = a} :: ApplicationInfo)
{-# DEPRECATED aiLinkedToGitHub "Use generic-lens or generic-optics with 'linkedToGitHub' instead." #-}

-- | The destination platform type for deployment of the application (@Lambda@ or @Server@ ).
--
-- /Note:/ Consider using 'computePlatform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiComputePlatform :: Lens.Lens' ApplicationInfo (Lude.Maybe ComputePlatform)
aiComputePlatform = Lens.lens (computePlatform :: ApplicationInfo -> Lude.Maybe ComputePlatform) (\s a -> s {computePlatform = a} :: ApplicationInfo)
{-# DEPRECATED aiComputePlatform "Use generic-lens or generic-optics with 'computePlatform' instead." #-}

-- | The application ID.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiApplicationId :: Lens.Lens' ApplicationInfo (Lude.Maybe Lude.Text)
aiApplicationId = Lens.lens (applicationId :: ApplicationInfo -> Lude.Maybe Lude.Text) (\s a -> s {applicationId = a} :: ApplicationInfo)
{-# DEPRECATED aiApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The application name.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiApplicationName :: Lens.Lens' ApplicationInfo (Lude.Maybe Lude.Text)
aiApplicationName = Lens.lens (applicationName :: ApplicationInfo -> Lude.Maybe Lude.Text) (\s a -> s {applicationName = a} :: ApplicationInfo)
{-# DEPRECATED aiApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | The name for a connection to a GitHub account.
--
-- /Note:/ Consider using 'gitHubAccountName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiGitHubAccountName :: Lens.Lens' ApplicationInfo (Lude.Maybe Lude.Text)
aiGitHubAccountName = Lens.lens (gitHubAccountName :: ApplicationInfo -> Lude.Maybe Lude.Text) (\s a -> s {gitHubAccountName = a} :: ApplicationInfo)
{-# DEPRECATED aiGitHubAccountName "Use generic-lens or generic-optics with 'gitHubAccountName' instead." #-}

-- | The time at which the application was created.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiCreateTime :: Lens.Lens' ApplicationInfo (Lude.Maybe Lude.Timestamp)
aiCreateTime = Lens.lens (createTime :: ApplicationInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {createTime = a} :: ApplicationInfo)
{-# DEPRECATED aiCreateTime "Use generic-lens or generic-optics with 'createTime' instead." #-}

instance Lude.FromJSON ApplicationInfo where
  parseJSON =
    Lude.withObject
      "ApplicationInfo"
      ( \x ->
          ApplicationInfo'
            Lude.<$> (x Lude..:? "linkedToGitHub")
            Lude.<*> (x Lude..:? "computePlatform")
            Lude.<*> (x Lude..:? "applicationId")
            Lude.<*> (x Lude..:? "applicationName")
            Lude.<*> (x Lude..:? "gitHubAccountName")
            Lude.<*> (x Lude..:? "createTime")
      )
