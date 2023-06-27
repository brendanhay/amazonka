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
-- Module      : Amazonka.WorkSpaces.Types.RelatedWorkspaceProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.RelatedWorkspaceProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkSpaces.Types.StandbyWorkspaceRelationshipType
import Amazonka.WorkSpaces.Types.WorkspaceState

-- | Describes the related WorkSpace. The related WorkSpace could be a
-- standby WorkSpace or primary WorkSpace related to the specified
-- WorkSpace.
--
-- /See:/ 'newRelatedWorkspaceProperties' smart constructor.
data RelatedWorkspaceProperties = RelatedWorkspaceProperties'
  { -- | The Region of the related WorkSpace.
    region :: Prelude.Maybe Prelude.Text,
    -- | Indicates the state of the WorkSpace.
    state :: Prelude.Maybe WorkspaceState,
    -- | Indicates the type of WorkSpace.
    type' :: Prelude.Maybe StandbyWorkspaceRelationshipType,
    -- | The identifier of the related WorkSpace.
    workspaceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RelatedWorkspaceProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'region', 'relatedWorkspaceProperties_region' - The Region of the related WorkSpace.
--
-- 'state', 'relatedWorkspaceProperties_state' - Indicates the state of the WorkSpace.
--
-- 'type'', 'relatedWorkspaceProperties_type' - Indicates the type of WorkSpace.
--
-- 'workspaceId', 'relatedWorkspaceProperties_workspaceId' - The identifier of the related WorkSpace.
newRelatedWorkspaceProperties ::
  RelatedWorkspaceProperties
newRelatedWorkspaceProperties =
  RelatedWorkspaceProperties'
    { region =
        Prelude.Nothing,
      state = Prelude.Nothing,
      type' = Prelude.Nothing,
      workspaceId = Prelude.Nothing
    }

-- | The Region of the related WorkSpace.
relatedWorkspaceProperties_region :: Lens.Lens' RelatedWorkspaceProperties (Prelude.Maybe Prelude.Text)
relatedWorkspaceProperties_region = Lens.lens (\RelatedWorkspaceProperties' {region} -> region) (\s@RelatedWorkspaceProperties' {} a -> s {region = a} :: RelatedWorkspaceProperties)

-- | Indicates the state of the WorkSpace.
relatedWorkspaceProperties_state :: Lens.Lens' RelatedWorkspaceProperties (Prelude.Maybe WorkspaceState)
relatedWorkspaceProperties_state = Lens.lens (\RelatedWorkspaceProperties' {state} -> state) (\s@RelatedWorkspaceProperties' {} a -> s {state = a} :: RelatedWorkspaceProperties)

-- | Indicates the type of WorkSpace.
relatedWorkspaceProperties_type :: Lens.Lens' RelatedWorkspaceProperties (Prelude.Maybe StandbyWorkspaceRelationshipType)
relatedWorkspaceProperties_type = Lens.lens (\RelatedWorkspaceProperties' {type'} -> type') (\s@RelatedWorkspaceProperties' {} a -> s {type' = a} :: RelatedWorkspaceProperties)

-- | The identifier of the related WorkSpace.
relatedWorkspaceProperties_workspaceId :: Lens.Lens' RelatedWorkspaceProperties (Prelude.Maybe Prelude.Text)
relatedWorkspaceProperties_workspaceId = Lens.lens (\RelatedWorkspaceProperties' {workspaceId} -> workspaceId) (\s@RelatedWorkspaceProperties' {} a -> s {workspaceId = a} :: RelatedWorkspaceProperties)

instance Data.FromJSON RelatedWorkspaceProperties where
  parseJSON =
    Data.withObject
      "RelatedWorkspaceProperties"
      ( \x ->
          RelatedWorkspaceProperties'
            Prelude.<$> (x Data..:? "Region")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "WorkspaceId")
      )

instance Prelude.Hashable RelatedWorkspaceProperties where
  hashWithSalt _salt RelatedWorkspaceProperties' {..} =
    _salt
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData RelatedWorkspaceProperties where
  rnf RelatedWorkspaceProperties' {..} =
    Prelude.rnf region
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf workspaceId
