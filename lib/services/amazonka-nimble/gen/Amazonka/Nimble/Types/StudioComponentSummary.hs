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
-- Module      : Amazonka.Nimble.Types.StudioComponentSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.StudioComponentSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Nimble.Types.StudioComponentSubtype
import Amazonka.Nimble.Types.StudioComponentType
import qualified Amazonka.Prelude as Prelude

-- |
--
-- /See:/ 'newStudioComponentSummary' smart constructor.
data StudioComponentSummary = StudioComponentSummary'
  { -- | The unique identifier for a studio component resource.
    studioComponentId :: Prelude.Maybe Prelude.Text,
    -- | The Unix epoch timestamp in seconds for when the resource was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The user ID of the user that created the studio component.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The user ID of the user that most recently updated the resource.
    updatedBy :: Prelude.Maybe Prelude.Text,
    -- | The specific subtype of a studio component.
    subtype :: Prelude.Maybe StudioComponentSubtype,
    -- | The name for the studio component.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Unix epoch timestamp in seconds for when the resource was updated.
    updatedAt :: Prelude.Maybe Core.POSIX,
    -- | The type of the studio component.
    type' :: Prelude.Maybe StudioComponentType,
    -- | The description.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StudioComponentSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'studioComponentId', 'studioComponentSummary_studioComponentId' - The unique identifier for a studio component resource.
--
-- 'createdAt', 'studioComponentSummary_createdAt' - The Unix epoch timestamp in seconds for when the resource was created.
--
-- 'createdBy', 'studioComponentSummary_createdBy' - The user ID of the user that created the studio component.
--
-- 'updatedBy', 'studioComponentSummary_updatedBy' - The user ID of the user that most recently updated the resource.
--
-- 'subtype', 'studioComponentSummary_subtype' - The specific subtype of a studio component.
--
-- 'name', 'studioComponentSummary_name' - The name for the studio component.
--
-- 'updatedAt', 'studioComponentSummary_updatedAt' - The Unix epoch timestamp in seconds for when the resource was updated.
--
-- 'type'', 'studioComponentSummary_type' - The type of the studio component.
--
-- 'description', 'studioComponentSummary_description' - The description.
newStudioComponentSummary ::
  StudioComponentSummary
newStudioComponentSummary =
  StudioComponentSummary'
    { studioComponentId =
        Prelude.Nothing,
      createdAt = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      updatedBy = Prelude.Nothing,
      subtype = Prelude.Nothing,
      name = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      type' = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The unique identifier for a studio component resource.
studioComponentSummary_studioComponentId :: Lens.Lens' StudioComponentSummary (Prelude.Maybe Prelude.Text)
studioComponentSummary_studioComponentId = Lens.lens (\StudioComponentSummary' {studioComponentId} -> studioComponentId) (\s@StudioComponentSummary' {} a -> s {studioComponentId = a} :: StudioComponentSummary)

-- | The Unix epoch timestamp in seconds for when the resource was created.
studioComponentSummary_createdAt :: Lens.Lens' StudioComponentSummary (Prelude.Maybe Prelude.UTCTime)
studioComponentSummary_createdAt = Lens.lens (\StudioComponentSummary' {createdAt} -> createdAt) (\s@StudioComponentSummary' {} a -> s {createdAt = a} :: StudioComponentSummary) Prelude.. Lens.mapping Core._Time

-- | The user ID of the user that created the studio component.
studioComponentSummary_createdBy :: Lens.Lens' StudioComponentSummary (Prelude.Maybe Prelude.Text)
studioComponentSummary_createdBy = Lens.lens (\StudioComponentSummary' {createdBy} -> createdBy) (\s@StudioComponentSummary' {} a -> s {createdBy = a} :: StudioComponentSummary)

-- | The user ID of the user that most recently updated the resource.
studioComponentSummary_updatedBy :: Lens.Lens' StudioComponentSummary (Prelude.Maybe Prelude.Text)
studioComponentSummary_updatedBy = Lens.lens (\StudioComponentSummary' {updatedBy} -> updatedBy) (\s@StudioComponentSummary' {} a -> s {updatedBy = a} :: StudioComponentSummary)

-- | The specific subtype of a studio component.
studioComponentSummary_subtype :: Lens.Lens' StudioComponentSummary (Prelude.Maybe StudioComponentSubtype)
studioComponentSummary_subtype = Lens.lens (\StudioComponentSummary' {subtype} -> subtype) (\s@StudioComponentSummary' {} a -> s {subtype = a} :: StudioComponentSummary)

-- | The name for the studio component.
studioComponentSummary_name :: Lens.Lens' StudioComponentSummary (Prelude.Maybe Prelude.Text)
studioComponentSummary_name = Lens.lens (\StudioComponentSummary' {name} -> name) (\s@StudioComponentSummary' {} a -> s {name = a} :: StudioComponentSummary)

-- | The Unix epoch timestamp in seconds for when the resource was updated.
studioComponentSummary_updatedAt :: Lens.Lens' StudioComponentSummary (Prelude.Maybe Prelude.UTCTime)
studioComponentSummary_updatedAt = Lens.lens (\StudioComponentSummary' {updatedAt} -> updatedAt) (\s@StudioComponentSummary' {} a -> s {updatedAt = a} :: StudioComponentSummary) Prelude.. Lens.mapping Core._Time

-- | The type of the studio component.
studioComponentSummary_type :: Lens.Lens' StudioComponentSummary (Prelude.Maybe StudioComponentType)
studioComponentSummary_type = Lens.lens (\StudioComponentSummary' {type'} -> type') (\s@StudioComponentSummary' {} a -> s {type' = a} :: StudioComponentSummary)

-- | The description.
studioComponentSummary_description :: Lens.Lens' StudioComponentSummary (Prelude.Maybe Prelude.Text)
studioComponentSummary_description = Lens.lens (\StudioComponentSummary' {description} -> description) (\s@StudioComponentSummary' {} a -> s {description = a} :: StudioComponentSummary)

instance Core.FromJSON StudioComponentSummary where
  parseJSON =
    Core.withObject
      "StudioComponentSummary"
      ( \x ->
          StudioComponentSummary'
            Prelude.<$> (x Core..:? "studioComponentId")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "createdBy")
            Prelude.<*> (x Core..:? "updatedBy")
            Prelude.<*> (x Core..:? "subtype")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "updatedAt")
            Prelude.<*> (x Core..:? "type")
            Prelude.<*> (x Core..:? "description")
      )

instance Prelude.Hashable StudioComponentSummary

instance Prelude.NFData StudioComponentSummary
