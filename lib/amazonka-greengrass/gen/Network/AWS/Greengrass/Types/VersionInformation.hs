{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.VersionInformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.VersionInformation
  ( VersionInformation (..),

    -- * Smart constructor
    mkVersionInformation,

    -- * Lenses
    viARN,
    viCreationTimestamp,
    viVersion,
    viId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a version.
--
-- /See:/ 'mkVersionInformation' smart constructor.
data VersionInformation = VersionInformation'
  { -- | The ARN of the version.
    arn :: Lude.Maybe Lude.Text,
    -- | The time, in milliseconds since the epoch, when the version was created.
    creationTimestamp :: Lude.Maybe Lude.Text,
    -- | The ID of the version.
    version :: Lude.Maybe Lude.Text,
    -- | The ID of the parent definition that the version is associated with.
    id :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VersionInformation' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the version.
-- * 'creationTimestamp' - The time, in milliseconds since the epoch, when the version was created.
-- * 'version' - The ID of the version.
-- * 'id' - The ID of the parent definition that the version is associated with.
mkVersionInformation ::
  VersionInformation
mkVersionInformation =
  VersionInformation'
    { arn = Lude.Nothing,
      creationTimestamp = Lude.Nothing,
      version = Lude.Nothing,
      id = Lude.Nothing
    }

-- | The ARN of the version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viARN :: Lens.Lens' VersionInformation (Lude.Maybe Lude.Text)
viARN = Lens.lens (arn :: VersionInformation -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: VersionInformation)
{-# DEPRECATED viARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time, in milliseconds since the epoch, when the version was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viCreationTimestamp :: Lens.Lens' VersionInformation (Lude.Maybe Lude.Text)
viCreationTimestamp = Lens.lens (creationTimestamp :: VersionInformation -> Lude.Maybe Lude.Text) (\s a -> s {creationTimestamp = a} :: VersionInformation)
{-# DEPRECATED viCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viVersion :: Lens.Lens' VersionInformation (Lude.Maybe Lude.Text)
viVersion = Lens.lens (version :: VersionInformation -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: VersionInformation)
{-# DEPRECATED viVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The ID of the parent definition that the version is associated with.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viId :: Lens.Lens' VersionInformation (Lude.Maybe Lude.Text)
viId = Lens.lens (id :: VersionInformation -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: VersionInformation)
{-# DEPRECATED viId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON VersionInformation where
  parseJSON =
    Lude.withObject
      "VersionInformation"
      ( \x ->
          VersionInformation'
            Lude.<$> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "CreationTimestamp")
            Lude.<*> (x Lude..:? "Version")
            Lude.<*> (x Lude..:? "Id")
      )
