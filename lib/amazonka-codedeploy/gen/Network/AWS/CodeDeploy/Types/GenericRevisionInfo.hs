{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.GenericRevisionInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.GenericRevisionInfo
  ( GenericRevisionInfo (..),

    -- * Smart constructor
    mkGenericRevisionInfo,

    -- * Lenses
    griRegisterTime,
    griFirstUsedTime,
    griDeploymentGroups,
    griLastUsedTime,
    griDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about an application revision.
--
-- /See:/ 'mkGenericRevisionInfo' smart constructor.
data GenericRevisionInfo = GenericRevisionInfo'
  { registerTime ::
      Lude.Maybe Lude.Timestamp,
    firstUsedTime :: Lude.Maybe Lude.Timestamp,
    deploymentGroups :: Lude.Maybe [Lude.Text],
    lastUsedTime :: Lude.Maybe Lude.Timestamp,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GenericRevisionInfo' with the minimum fields required to make a request.
--
-- * 'deploymentGroups' - The deployment groups for which this is the current target revision.
-- * 'description' - A comment about the revision.
-- * 'firstUsedTime' - When the revision was first used by AWS CodeDeploy.
-- * 'lastUsedTime' - When the revision was last used by AWS CodeDeploy.
-- * 'registerTime' - When the revision was registered with AWS CodeDeploy.
mkGenericRevisionInfo ::
  GenericRevisionInfo
mkGenericRevisionInfo =
  GenericRevisionInfo'
    { registerTime = Lude.Nothing,
      firstUsedTime = Lude.Nothing,
      deploymentGroups = Lude.Nothing,
      lastUsedTime = Lude.Nothing,
      description = Lude.Nothing
    }

-- | When the revision was registered with AWS CodeDeploy.
--
-- /Note:/ Consider using 'registerTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
griRegisterTime :: Lens.Lens' GenericRevisionInfo (Lude.Maybe Lude.Timestamp)
griRegisterTime = Lens.lens (registerTime :: GenericRevisionInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {registerTime = a} :: GenericRevisionInfo)
{-# DEPRECATED griRegisterTime "Use generic-lens or generic-optics with 'registerTime' instead." #-}

-- | When the revision was first used by AWS CodeDeploy.
--
-- /Note:/ Consider using 'firstUsedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
griFirstUsedTime :: Lens.Lens' GenericRevisionInfo (Lude.Maybe Lude.Timestamp)
griFirstUsedTime = Lens.lens (firstUsedTime :: GenericRevisionInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {firstUsedTime = a} :: GenericRevisionInfo)
{-# DEPRECATED griFirstUsedTime "Use generic-lens or generic-optics with 'firstUsedTime' instead." #-}

-- | The deployment groups for which this is the current target revision.
--
-- /Note:/ Consider using 'deploymentGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
griDeploymentGroups :: Lens.Lens' GenericRevisionInfo (Lude.Maybe [Lude.Text])
griDeploymentGroups = Lens.lens (deploymentGroups :: GenericRevisionInfo -> Lude.Maybe [Lude.Text]) (\s a -> s {deploymentGroups = a} :: GenericRevisionInfo)
{-# DEPRECATED griDeploymentGroups "Use generic-lens or generic-optics with 'deploymentGroups' instead." #-}

-- | When the revision was last used by AWS CodeDeploy.
--
-- /Note:/ Consider using 'lastUsedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
griLastUsedTime :: Lens.Lens' GenericRevisionInfo (Lude.Maybe Lude.Timestamp)
griLastUsedTime = Lens.lens (lastUsedTime :: GenericRevisionInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUsedTime = a} :: GenericRevisionInfo)
{-# DEPRECATED griLastUsedTime "Use generic-lens or generic-optics with 'lastUsedTime' instead." #-}

-- | A comment about the revision.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
griDescription :: Lens.Lens' GenericRevisionInfo (Lude.Maybe Lude.Text)
griDescription = Lens.lens (description :: GenericRevisionInfo -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: GenericRevisionInfo)
{-# DEPRECATED griDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON GenericRevisionInfo where
  parseJSON =
    Lude.withObject
      "GenericRevisionInfo"
      ( \x ->
          GenericRevisionInfo'
            Lude.<$> (x Lude..:? "registerTime")
            Lude.<*> (x Lude..:? "firstUsedTime")
            Lude.<*> (x Lude..:? "deploymentGroups" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "lastUsedTime")
            Lude.<*> (x Lude..:? "description")
      )
