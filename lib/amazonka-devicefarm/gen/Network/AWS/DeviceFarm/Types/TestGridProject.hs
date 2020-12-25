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
    tgpDescription,
    tgpName,
  )
where

import qualified Network.AWS.DeviceFarm.Types.Arn as Types
import qualified Network.AWS.DeviceFarm.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A Selenium testing project. Projects are used to collect and collate sessions.
--
-- /See:/ 'mkTestGridProject' smart constructor.
data TestGridProject = TestGridProject'
  { -- | The ARN for the project.
    arn :: Core.Maybe Types.Arn,
    -- | When the project was created.
    created :: Core.Maybe Core.NominalDiffTime,
    -- | A human-readable description for the project.
    description :: Core.Maybe Types.String,
    -- | A human-readable name for the project.
    name :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'TestGridProject' value with any optional fields omitted.
mkTestGridProject ::
  TestGridProject
mkTestGridProject =
  TestGridProject'
    { arn = Core.Nothing,
      created = Core.Nothing,
      description = Core.Nothing,
      name = Core.Nothing
    }

-- | The ARN for the project.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpArn :: Lens.Lens' TestGridProject (Core.Maybe Types.Arn)
tgpArn = Lens.field @"arn"
{-# DEPRECATED tgpArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | When the project was created.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpCreated :: Lens.Lens' TestGridProject (Core.Maybe Core.NominalDiffTime)
tgpCreated = Lens.field @"created"
{-# DEPRECATED tgpCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | A human-readable description for the project.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpDescription :: Lens.Lens' TestGridProject (Core.Maybe Types.String)
tgpDescription = Lens.field @"description"
{-# DEPRECATED tgpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A human-readable name for the project.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpName :: Lens.Lens' TestGridProject (Core.Maybe Types.String)
tgpName = Lens.field @"name"
{-# DEPRECATED tgpName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON TestGridProject where
  parseJSON =
    Core.withObject "TestGridProject" Core.$
      \x ->
        TestGridProject'
          Core.<$> (x Core..:? "arn")
          Core.<*> (x Core..:? "created")
          Core.<*> (x Core..:? "description")
          Core.<*> (x Core..:? "name")
