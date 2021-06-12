{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.Types.CreatedArtifact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MigrationHub.Types.CreatedArtifact where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An ARN of the AWS cloud resource target receiving the migration (e.g.,
-- AMI, EC2 instance, RDS instance, etc.).
--
-- /See:/ 'newCreatedArtifact' smart constructor.
data CreatedArtifact = CreatedArtifact'
  { -- | A description that can be free-form text to record additional detail
    -- about the artifact for clarity or for later reference.
    description :: Core.Maybe Core.Text,
    -- | An ARN that uniquely identifies the result of a migration task.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreatedArtifact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createdArtifact_description' - A description that can be free-form text to record additional detail
-- about the artifact for clarity or for later reference.
--
-- 'name', 'createdArtifact_name' - An ARN that uniquely identifies the result of a migration task.
newCreatedArtifact ::
  -- | 'name'
  Core.Text ->
  CreatedArtifact
newCreatedArtifact pName_ =
  CreatedArtifact'
    { description = Core.Nothing,
      name = pName_
    }

-- | A description that can be free-form text to record additional detail
-- about the artifact for clarity or for later reference.
createdArtifact_description :: Lens.Lens' CreatedArtifact (Core.Maybe Core.Text)
createdArtifact_description = Lens.lens (\CreatedArtifact' {description} -> description) (\s@CreatedArtifact' {} a -> s {description = a} :: CreatedArtifact)

-- | An ARN that uniquely identifies the result of a migration task.
createdArtifact_name :: Lens.Lens' CreatedArtifact Core.Text
createdArtifact_name = Lens.lens (\CreatedArtifact' {name} -> name) (\s@CreatedArtifact' {} a -> s {name = a} :: CreatedArtifact)

instance Core.FromJSON CreatedArtifact where
  parseJSON =
    Core.withObject
      "CreatedArtifact"
      ( \x ->
          CreatedArtifact'
            Core.<$> (x Core..:? "Description")
            Core.<*> (x Core..: "Name")
      )

instance Core.Hashable CreatedArtifact

instance Core.NFData CreatedArtifact

instance Core.ToJSON CreatedArtifact where
  toJSON CreatedArtifact' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Description" Core..=) Core.<$> description,
            Core.Just ("Name" Core..= name)
          ]
      )
