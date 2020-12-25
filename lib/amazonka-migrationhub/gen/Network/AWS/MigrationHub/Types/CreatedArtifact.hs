{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.Types.CreatedArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MigrationHub.Types.CreatedArtifact
  ( CreatedArtifact (..),

    -- * Smart constructor
    mkCreatedArtifact,

    -- * Lenses
    caName,
    caDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MigrationHub.Types.Description as Types
import qualified Network.AWS.MigrationHub.Types.Name as Types
import qualified Network.AWS.Prelude as Core

-- | An ARN of the AWS cloud resource target receiving the migration (e.g., AMI, EC2 instance, RDS instance, etc.).
--
-- /See:/ 'mkCreatedArtifact' smart constructor.
data CreatedArtifact = CreatedArtifact'
  { -- | An ARN that uniquely identifies the result of a migration task.
    name :: Types.Name,
    -- | A description that can be free-form text to record additional detail about the artifact for clarity or for later reference.
    description :: Core.Maybe Types.Description
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatedArtifact' value with any optional fields omitted.
mkCreatedArtifact ::
  -- | 'name'
  Types.Name ->
  CreatedArtifact
mkCreatedArtifact name =
  CreatedArtifact' {name, description = Core.Nothing}

-- | An ARN that uniquely identifies the result of a migration task.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caName :: Lens.Lens' CreatedArtifact Types.Name
caName = Lens.field @"name"
{-# DEPRECATED caName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A description that can be free-form text to record additional detail about the artifact for clarity or for later reference.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDescription :: Lens.Lens' CreatedArtifact (Core.Maybe Types.Description)
caDescription = Lens.field @"description"
{-# DEPRECATED caDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Core.FromJSON CreatedArtifact where
  toJSON CreatedArtifact {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            ("Description" Core..=) Core.<$> description
          ]
      )

instance Core.FromJSON CreatedArtifact where
  parseJSON =
    Core.withObject "CreatedArtifact" Core.$
      \x ->
        CreatedArtifact'
          Core.<$> (x Core..: "Name") Core.<*> (x Core..:? "Description")
