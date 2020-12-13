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
    diLatestVersionARN,
    diARN,
    diName,
    diCreationTimestamp,
    diId,
    diTags,
    diLatestVersion,
    diLastUpdatedTimestamp,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a definition.
--
-- /See:/ 'mkDefinitionInformation' smart constructor.
data DefinitionInformation = DefinitionInformation'
  { -- | The ARN of the latest version associated with the definition.
    latestVersionARN :: Lude.Maybe Lude.Text,
    -- | The ARN of the definition.
    arn :: Lude.Maybe Lude.Text,
    -- | The name of the definition.
    name :: Lude.Maybe Lude.Text,
    -- | The time, in milliseconds since the epoch, when the definition was created.
    creationTimestamp :: Lude.Maybe Lude.Text,
    -- | The ID of the definition.
    id :: Lude.Maybe Lude.Text,
    -- | Tag(s) attached to the resource arn.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The ID of the latest version associated with the definition.
    latestVersion :: Lude.Maybe Lude.Text,
    -- | The time, in milliseconds since the epoch, when the definition was last updated.
    lastUpdatedTimestamp :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DefinitionInformation' with the minimum fields required to make a request.
--
-- * 'latestVersionARN' - The ARN of the latest version associated with the definition.
-- * 'arn' - The ARN of the definition.
-- * 'name' - The name of the definition.
-- * 'creationTimestamp' - The time, in milliseconds since the epoch, when the definition was created.
-- * 'id' - The ID of the definition.
-- * 'tags' - Tag(s) attached to the resource arn.
-- * 'latestVersion' - The ID of the latest version associated with the definition.
-- * 'lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last updated.
mkDefinitionInformation ::
  DefinitionInformation
mkDefinitionInformation =
  DefinitionInformation'
    { latestVersionARN = Lude.Nothing,
      arn = Lude.Nothing,
      name = Lude.Nothing,
      creationTimestamp = Lude.Nothing,
      id = Lude.Nothing,
      tags = Lude.Nothing,
      latestVersion = Lude.Nothing,
      lastUpdatedTimestamp = Lude.Nothing
    }

-- | The ARN of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diLatestVersionARN :: Lens.Lens' DefinitionInformation (Lude.Maybe Lude.Text)
diLatestVersionARN = Lens.lens (latestVersionARN :: DefinitionInformation -> Lude.Maybe Lude.Text) (\s a -> s {latestVersionARN = a} :: DefinitionInformation)
{-# DEPRECATED diLatestVersionARN "Use generic-lens or generic-optics with 'latestVersionARN' instead." #-}

-- | The ARN of the definition.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diARN :: Lens.Lens' DefinitionInformation (Lude.Maybe Lude.Text)
diARN = Lens.lens (arn :: DefinitionInformation -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DefinitionInformation)
{-# DEPRECATED diARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diName :: Lens.Lens' DefinitionInformation (Lude.Maybe Lude.Text)
diName = Lens.lens (name :: DefinitionInformation -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DefinitionInformation)
{-# DEPRECATED diName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diCreationTimestamp :: Lens.Lens' DefinitionInformation (Lude.Maybe Lude.Text)
diCreationTimestamp = Lens.lens (creationTimestamp :: DefinitionInformation -> Lude.Maybe Lude.Text) (\s a -> s {creationTimestamp = a} :: DefinitionInformation)
{-# DEPRECATED diCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diId :: Lens.Lens' DefinitionInformation (Lude.Maybe Lude.Text)
diId = Lens.lens (id :: DefinitionInformation -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: DefinitionInformation)
{-# DEPRECATED diId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Tag(s) attached to the resource arn.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diTags :: Lens.Lens' DefinitionInformation (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
diTags = Lens.lens (tags :: DefinitionInformation -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: DefinitionInformation)
{-# DEPRECATED diTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diLatestVersion :: Lens.Lens' DefinitionInformation (Lude.Maybe Lude.Text)
diLatestVersion = Lens.lens (latestVersion :: DefinitionInformation -> Lude.Maybe Lude.Text) (\s a -> s {latestVersion = a} :: DefinitionInformation)
{-# DEPRECATED diLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diLastUpdatedTimestamp :: Lens.Lens' DefinitionInformation (Lude.Maybe Lude.Text)
diLastUpdatedTimestamp = Lens.lens (lastUpdatedTimestamp :: DefinitionInformation -> Lude.Maybe Lude.Text) (\s a -> s {lastUpdatedTimestamp = a} :: DefinitionInformation)
{-# DEPRECATED diLastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead." #-}

instance Lude.FromJSON DefinitionInformation where
  parseJSON =
    Lude.withObject
      "DefinitionInformation"
      ( \x ->
          DefinitionInformation'
            Lude.<$> (x Lude..:? "LatestVersionArn")
            Lude.<*> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "CreationTimestamp")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "LatestVersion")
            Lude.<*> (x Lude..:? "LastUpdatedTimestamp")
      )
