{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.DefinitionInformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.DefinitionInformation
  ( DefinitionInformation (..),

    -- * Smart constructor
    mkDefinitionInformation,

    -- * Lenses
    diArn,
    diCreationTimestamp,
    diId,
    diLastUpdatedTimestamp,
    diLatestVersion,
    diLatestVersionArn,
    diName,
    diTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a definition.
--
-- /See:/ 'mkDefinitionInformation' smart constructor.
data DefinitionInformation = DefinitionInformation'
  { -- | The ARN of the definition.
    arn :: Core.Maybe Core.Text,
    -- | The time, in milliseconds since the epoch, when the definition was created.
    creationTimestamp :: Core.Maybe Core.Text,
    -- | The ID of the definition.
    id :: Core.Maybe Core.Text,
    -- | The time, in milliseconds since the epoch, when the definition was last updated.
    lastUpdatedTimestamp :: Core.Maybe Core.Text,
    -- | The ID of the latest version associated with the definition.
    latestVersion :: Core.Maybe Core.Text,
    -- | The ARN of the latest version associated with the definition.
    latestVersionArn :: Core.Maybe Core.Text,
    -- | The name of the definition.
    name :: Core.Maybe Core.Text,
    -- | Tag(s) attached to the resource arn.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DefinitionInformation' value with any optional fields omitted.
mkDefinitionInformation ::
  DefinitionInformation
mkDefinitionInformation =
  DefinitionInformation'
    { arn = Core.Nothing,
      creationTimestamp = Core.Nothing,
      id = Core.Nothing,
      lastUpdatedTimestamp = Core.Nothing,
      latestVersion = Core.Nothing,
      latestVersionArn = Core.Nothing,
      name = Core.Nothing,
      tags = Core.Nothing
    }

-- | The ARN of the definition.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diArn :: Lens.Lens' DefinitionInformation (Core.Maybe Core.Text)
diArn = Lens.field @"arn"
{-# DEPRECATED diArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diCreationTimestamp :: Lens.Lens' DefinitionInformation (Core.Maybe Core.Text)
diCreationTimestamp = Lens.field @"creationTimestamp"
{-# DEPRECATED diCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diId :: Lens.Lens' DefinitionInformation (Core.Maybe Core.Text)
diId = Lens.field @"id"
{-# DEPRECATED diId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diLastUpdatedTimestamp :: Lens.Lens' DefinitionInformation (Core.Maybe Core.Text)
diLastUpdatedTimestamp = Lens.field @"lastUpdatedTimestamp"
{-# DEPRECATED diLastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead." #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diLatestVersion :: Lens.Lens' DefinitionInformation (Core.Maybe Core.Text)
diLatestVersion = Lens.field @"latestVersion"
{-# DEPRECATED diLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

-- | The ARN of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diLatestVersionArn :: Lens.Lens' DefinitionInformation (Core.Maybe Core.Text)
diLatestVersionArn = Lens.field @"latestVersionArn"
{-# DEPRECATED diLatestVersionArn "Use generic-lens or generic-optics with 'latestVersionArn' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diName :: Lens.Lens' DefinitionInformation (Core.Maybe Core.Text)
diName = Lens.field @"name"
{-# DEPRECATED diName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Tag(s) attached to the resource arn.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diTags :: Lens.Lens' DefinitionInformation (Core.Maybe (Core.HashMap Core.Text Core.Text))
diTags = Lens.field @"tags"
{-# DEPRECATED diTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON DefinitionInformation where
  parseJSON =
    Core.withObject "DefinitionInformation" Core.$
      \x ->
        DefinitionInformation'
          Core.<$> (x Core..:? "Arn")
          Core.<*> (x Core..:? "CreationTimestamp")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "LastUpdatedTimestamp")
          Core.<*> (x Core..:? "LatestVersion")
          Core.<*> (x Core..:? "LatestVersionArn")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "tags")
