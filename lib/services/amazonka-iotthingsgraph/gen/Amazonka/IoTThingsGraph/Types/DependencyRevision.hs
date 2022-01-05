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
-- Module      : Amazonka.IoTThingsGraph.Types.DependencyRevision
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTThingsGraph.Types.DependencyRevision where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that contains the ID and revision number of a workflow or
-- system that is part of a deployment.
--
-- /See:/ 'newDependencyRevision' smart constructor.
data DependencyRevision = DependencyRevision'
  { -- | The revision number of the workflow or system.
    revisionNumber :: Prelude.Maybe Prelude.Integer,
    -- | The ID of the workflow or system.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DependencyRevision' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revisionNumber', 'dependencyRevision_revisionNumber' - The revision number of the workflow or system.
--
-- 'id', 'dependencyRevision_id' - The ID of the workflow or system.
newDependencyRevision ::
  DependencyRevision
newDependencyRevision =
  DependencyRevision'
    { revisionNumber =
        Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The revision number of the workflow or system.
dependencyRevision_revisionNumber :: Lens.Lens' DependencyRevision (Prelude.Maybe Prelude.Integer)
dependencyRevision_revisionNumber = Lens.lens (\DependencyRevision' {revisionNumber} -> revisionNumber) (\s@DependencyRevision' {} a -> s {revisionNumber = a} :: DependencyRevision)

-- | The ID of the workflow or system.
dependencyRevision_id :: Lens.Lens' DependencyRevision (Prelude.Maybe Prelude.Text)
dependencyRevision_id = Lens.lens (\DependencyRevision' {id} -> id) (\s@DependencyRevision' {} a -> s {id = a} :: DependencyRevision)

instance Core.FromJSON DependencyRevision where
  parseJSON =
    Core.withObject
      "DependencyRevision"
      ( \x ->
          DependencyRevision'
            Prelude.<$> (x Core..:? "revisionNumber")
            Prelude.<*> (x Core..:? "id")
      )

instance Prelude.Hashable DependencyRevision where
  hashWithSalt _salt DependencyRevision' {..} =
    _salt `Prelude.hashWithSalt` revisionNumber
      `Prelude.hashWithSalt` id

instance Prelude.NFData DependencyRevision where
  rnf DependencyRevision' {..} =
    Prelude.rnf revisionNumber
      `Prelude.seq` Prelude.rnf id
