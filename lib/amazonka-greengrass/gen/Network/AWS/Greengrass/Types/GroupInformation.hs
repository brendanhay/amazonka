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
    giArn,
    giCreationTimestamp,
    giId,
    giLastUpdatedTimestamp,
    giLatestVersion,
    giLatestVersionArn,
    giName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a group.
--
-- /See:/ 'mkGroupInformation' smart constructor.
data GroupInformation = GroupInformation'
  { -- | The ARN of the group.
    arn :: Core.Maybe Core.Text,
    -- | The time, in milliseconds since the epoch, when the group was created.
    creationTimestamp :: Core.Maybe Core.Text,
    -- | The ID of the group.
    id :: Core.Maybe Core.Text,
    -- | The time, in milliseconds since the epoch, when the group was last updated.
    lastUpdatedTimestamp :: Core.Maybe Core.Text,
    -- | The ID of the latest version associated with the group.
    latestVersion :: Core.Maybe Core.Text,
    -- | The ARN of the latest version associated with the group.
    latestVersionArn :: Core.Maybe Core.Text,
    -- | The name of the group.
    name :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GroupInformation' value with any optional fields omitted.
mkGroupInformation ::
  GroupInformation
mkGroupInformation =
  GroupInformation'
    { arn = Core.Nothing,
      creationTimestamp = Core.Nothing,
      id = Core.Nothing,
      lastUpdatedTimestamp = Core.Nothing,
      latestVersion = Core.Nothing,
      latestVersionArn = Core.Nothing,
      name = Core.Nothing
    }

-- | The ARN of the group.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giArn :: Lens.Lens' GroupInformation (Core.Maybe Core.Text)
giArn = Lens.field @"arn"
{-# DEPRECATED giArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time, in milliseconds since the epoch, when the group was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giCreationTimestamp :: Lens.Lens' GroupInformation (Core.Maybe Core.Text)
giCreationTimestamp = Lens.field @"creationTimestamp"
{-# DEPRECATED giCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the group.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giId :: Lens.Lens' GroupInformation (Core.Maybe Core.Text)
giId = Lens.field @"id"
{-# DEPRECATED giId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The time, in milliseconds since the epoch, when the group was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giLastUpdatedTimestamp :: Lens.Lens' GroupInformation (Core.Maybe Core.Text)
giLastUpdatedTimestamp = Lens.field @"lastUpdatedTimestamp"
{-# DEPRECATED giLastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead." #-}

-- | The ID of the latest version associated with the group.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giLatestVersion :: Lens.Lens' GroupInformation (Core.Maybe Core.Text)
giLatestVersion = Lens.field @"latestVersion"
{-# DEPRECATED giLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

-- | The ARN of the latest version associated with the group.
--
-- /Note:/ Consider using 'latestVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giLatestVersionArn :: Lens.Lens' GroupInformation (Core.Maybe Core.Text)
giLatestVersionArn = Lens.field @"latestVersionArn"
{-# DEPRECATED giLatestVersionArn "Use generic-lens or generic-optics with 'latestVersionArn' instead." #-}

-- | The name of the group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giName :: Lens.Lens' GroupInformation (Core.Maybe Core.Text)
giName = Lens.field @"name"
{-# DEPRECATED giName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON GroupInformation where
  parseJSON =
    Core.withObject "GroupInformation" Core.$
      \x ->
        GroupInformation'
          Core.<$> (x Core..:? "Arn")
          Core.<*> (x Core..:? "CreationTimestamp")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "LastUpdatedTimestamp")
          Core.<*> (x Core..:? "LatestVersion")
          Core.<*> (x Core..:? "LatestVersionArn")
          Core.<*> (x Core..:? "Name")
