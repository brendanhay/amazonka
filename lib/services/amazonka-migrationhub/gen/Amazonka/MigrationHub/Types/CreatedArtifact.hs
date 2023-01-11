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
-- Module      : Amazonka.MigrationHub.Types.CreatedArtifact
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHub.Types.CreatedArtifact where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An ARN of the AWS cloud resource target receiving the migration (e.g.,
-- AMI, EC2 instance, RDS instance, etc.).
--
-- /See:/ 'newCreatedArtifact' smart constructor.
data CreatedArtifact = CreatedArtifact'
  { -- | A description that can be free-form text to record additional detail
    -- about the artifact for clarity or for later reference.
    description :: Prelude.Maybe Prelude.Text,
    -- | An ARN that uniquely identifies the result of a migration task.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  CreatedArtifact
newCreatedArtifact pName_ =
  CreatedArtifact'
    { description = Prelude.Nothing,
      name = pName_
    }

-- | A description that can be free-form text to record additional detail
-- about the artifact for clarity or for later reference.
createdArtifact_description :: Lens.Lens' CreatedArtifact (Prelude.Maybe Prelude.Text)
createdArtifact_description = Lens.lens (\CreatedArtifact' {description} -> description) (\s@CreatedArtifact' {} a -> s {description = a} :: CreatedArtifact)

-- | An ARN that uniquely identifies the result of a migration task.
createdArtifact_name :: Lens.Lens' CreatedArtifact Prelude.Text
createdArtifact_name = Lens.lens (\CreatedArtifact' {name} -> name) (\s@CreatedArtifact' {} a -> s {name = a} :: CreatedArtifact)

instance Data.FromJSON CreatedArtifact where
  parseJSON =
    Data.withObject
      "CreatedArtifact"
      ( \x ->
          CreatedArtifact'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..: "Name")
      )

instance Prelude.Hashable CreatedArtifact where
  hashWithSalt _salt CreatedArtifact' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreatedArtifact where
  rnf CreatedArtifact' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON CreatedArtifact where
  toJSON CreatedArtifact' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            Prelude.Just ("Name" Data..= name)
          ]
      )
