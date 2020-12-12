{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.GroupInformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.GroupInformation
  ( GroupInformation (..),

    -- * Smart constructor
    mkGroupInformation,

    -- * Lenses
    giLatestVersionARN,
    giARN,
    giName,
    giCreationTimestamp,
    giId,
    giLatestVersion,
    giLastUpdatedTimestamp,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a group.
--
-- /See:/ 'mkGroupInformation' smart constructor.
data GroupInformation = GroupInformation'
  { latestVersionARN ::
      Lude.Maybe Lude.Text,
    arn :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    creationTimestamp :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    latestVersion :: Lude.Maybe Lude.Text,
    lastUpdatedTimestamp :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GroupInformation' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the group.
-- * 'creationTimestamp' - The time, in milliseconds since the epoch, when the group was created.
-- * 'id' - The ID of the group.
-- * 'lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the group was last updated.
-- * 'latestVersion' - The ID of the latest version associated with the group.
-- * 'latestVersionARN' - The ARN of the latest version associated with the group.
-- * 'name' - The name of the group.
mkGroupInformation ::
  GroupInformation
mkGroupInformation =
  GroupInformation'
    { latestVersionARN = Lude.Nothing,
      arn = Lude.Nothing,
      name = Lude.Nothing,
      creationTimestamp = Lude.Nothing,
      id = Lude.Nothing,
      latestVersion = Lude.Nothing,
      lastUpdatedTimestamp = Lude.Nothing
    }

-- | The ARN of the latest version associated with the group.
--
-- /Note:/ Consider using 'latestVersionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giLatestVersionARN :: Lens.Lens' GroupInformation (Lude.Maybe Lude.Text)
giLatestVersionARN = Lens.lens (latestVersionARN :: GroupInformation -> Lude.Maybe Lude.Text) (\s a -> s {latestVersionARN = a} :: GroupInformation)
{-# DEPRECATED giLatestVersionARN "Use generic-lens or generic-optics with 'latestVersionARN' instead." #-}

-- | The ARN of the group.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giARN :: Lens.Lens' GroupInformation (Lude.Maybe Lude.Text)
giARN = Lens.lens (arn :: GroupInformation -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: GroupInformation)
{-# DEPRECATED giARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giName :: Lens.Lens' GroupInformation (Lude.Maybe Lude.Text)
giName = Lens.lens (name :: GroupInformation -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GroupInformation)
{-# DEPRECATED giName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The time, in milliseconds since the epoch, when the group was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giCreationTimestamp :: Lens.Lens' GroupInformation (Lude.Maybe Lude.Text)
giCreationTimestamp = Lens.lens (creationTimestamp :: GroupInformation -> Lude.Maybe Lude.Text) (\s a -> s {creationTimestamp = a} :: GroupInformation)
{-# DEPRECATED giCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the group.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giId :: Lens.Lens' GroupInformation (Lude.Maybe Lude.Text)
giId = Lens.lens (id :: GroupInformation -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: GroupInformation)
{-# DEPRECATED giId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The ID of the latest version associated with the group.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giLatestVersion :: Lens.Lens' GroupInformation (Lude.Maybe Lude.Text)
giLatestVersion = Lens.lens (latestVersion :: GroupInformation -> Lude.Maybe Lude.Text) (\s a -> s {latestVersion = a} :: GroupInformation)
{-# DEPRECATED giLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

-- | The time, in milliseconds since the epoch, when the group was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giLastUpdatedTimestamp :: Lens.Lens' GroupInformation (Lude.Maybe Lude.Text)
giLastUpdatedTimestamp = Lens.lens (lastUpdatedTimestamp :: GroupInformation -> Lude.Maybe Lude.Text) (\s a -> s {lastUpdatedTimestamp = a} :: GroupInformation)
{-# DEPRECATED giLastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead." #-}

instance Lude.FromJSON GroupInformation where
  parseJSON =
    Lude.withObject
      "GroupInformation"
      ( \x ->
          GroupInformation'
            Lude.<$> (x Lude..:? "LatestVersionArn")
            Lude.<*> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "CreationTimestamp")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "LatestVersion")
            Lude.<*> (x Lude..:? "LastUpdatedTimestamp")
      )
