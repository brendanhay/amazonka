{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.TestGridProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.TestGridProject
  ( TestGridProject (..),

    -- * Smart constructor
    mkTestGridProject,

    -- * Lenses
    tgpArn,
    tgpCreated,
    tgpName,
    tgpDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A Selenium testing project. Projects are used to collect and collate sessions.
--
-- /See:/ 'mkTestGridProject' smart constructor.
data TestGridProject = TestGridProject'
  { -- | The ARN for the project.
    arn :: Lude.Maybe Lude.Text,
    -- | When the project was created.
    created :: Lude.Maybe Lude.Timestamp,
    -- | A human-readable name for the project.
    name :: Lude.Maybe Lude.Text,
    -- | A human-readable description for the project.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TestGridProject' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN for the project.
-- * 'created' - When the project was created.
-- * 'name' - A human-readable name for the project.
-- * 'description' - A human-readable description for the project.
mkTestGridProject ::
  TestGridProject
mkTestGridProject =
  TestGridProject'
    { arn = Lude.Nothing,
      created = Lude.Nothing,
      name = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The ARN for the project.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpArn :: Lens.Lens' TestGridProject (Lude.Maybe Lude.Text)
tgpArn = Lens.lens (arn :: TestGridProject -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: TestGridProject)
{-# DEPRECATED tgpArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | When the project was created.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpCreated :: Lens.Lens' TestGridProject (Lude.Maybe Lude.Timestamp)
tgpCreated = Lens.lens (created :: TestGridProject -> Lude.Maybe Lude.Timestamp) (\s a -> s {created = a} :: TestGridProject)
{-# DEPRECATED tgpCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | A human-readable name for the project.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpName :: Lens.Lens' TestGridProject (Lude.Maybe Lude.Text)
tgpName = Lens.lens (name :: TestGridProject -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: TestGridProject)
{-# DEPRECATED tgpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A human-readable description for the project.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpDescription :: Lens.Lens' TestGridProject (Lude.Maybe Lude.Text)
tgpDescription = Lens.lens (description :: TestGridProject -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: TestGridProject)
{-# DEPRECATED tgpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON TestGridProject where
  parseJSON =
    Lude.withObject
      "TestGridProject"
      ( \x ->
          TestGridProject'
            Lude.<$> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "created")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "description")
      )
