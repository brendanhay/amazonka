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
-- Module      : Amazonka.SageMaker.Types.QueryFilters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.QueryFilters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.LineageType

-- | A set of filters to narrow the set of lineage entities connected to the
-- @StartArn@(s) returned by the @QueryLineage@ API action.
--
-- /See:/ 'newQueryFilters' smart constructor.
data QueryFilters = QueryFilters'
  { -- | Filter the lineage entities connected to the @StartArn@(s) after the
    -- last modified date.
    modifiedAfter :: Prelude.Maybe Core.POSIX,
    -- | Filter the lineage entities connected to the @StartArn@(s) by a set if
    -- property key value pairs. If multiple pairs are provided, an entity is
    -- included in the results if it matches any of the provided pairs.
    properties :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Filter the lineage entities connected to the @StartArn@(s) by created
    -- date.
    createdBefore :: Prelude.Maybe Core.POSIX,
    -- | Filter the lineage entities connected to the @StartArn@ by type. For
    -- example: @DataSet@, @Model@, @Endpoint@, or @ModelDeployment@.
    types :: Prelude.Maybe [Prelude.Text],
    -- | Filter the lineage entities connected to the @StartArn@(s) by the type
    -- of the lineage entity.
    lineageTypes :: Prelude.Maybe [LineageType],
    -- | Filter the lineage entities connected to the @StartArn@(s) after the
    -- create date.
    createdAfter :: Prelude.Maybe Core.POSIX,
    -- | Filter the lineage entities connected to the @StartArn@(s) before the
    -- last modified date.
    modifiedBefore :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueryFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modifiedAfter', 'queryFilters_modifiedAfter' - Filter the lineage entities connected to the @StartArn@(s) after the
-- last modified date.
--
-- 'properties', 'queryFilters_properties' - Filter the lineage entities connected to the @StartArn@(s) by a set if
-- property key value pairs. If multiple pairs are provided, an entity is
-- included in the results if it matches any of the provided pairs.
--
-- 'createdBefore', 'queryFilters_createdBefore' - Filter the lineage entities connected to the @StartArn@(s) by created
-- date.
--
-- 'types', 'queryFilters_types' - Filter the lineage entities connected to the @StartArn@ by type. For
-- example: @DataSet@, @Model@, @Endpoint@, or @ModelDeployment@.
--
-- 'lineageTypes', 'queryFilters_lineageTypes' - Filter the lineage entities connected to the @StartArn@(s) by the type
-- of the lineage entity.
--
-- 'createdAfter', 'queryFilters_createdAfter' - Filter the lineage entities connected to the @StartArn@(s) after the
-- create date.
--
-- 'modifiedBefore', 'queryFilters_modifiedBefore' - Filter the lineage entities connected to the @StartArn@(s) before the
-- last modified date.
newQueryFilters ::
  QueryFilters
newQueryFilters =
  QueryFilters'
    { modifiedAfter = Prelude.Nothing,
      properties = Prelude.Nothing,
      createdBefore = Prelude.Nothing,
      types = Prelude.Nothing,
      lineageTypes = Prelude.Nothing,
      createdAfter = Prelude.Nothing,
      modifiedBefore = Prelude.Nothing
    }

-- | Filter the lineage entities connected to the @StartArn@(s) after the
-- last modified date.
queryFilters_modifiedAfter :: Lens.Lens' QueryFilters (Prelude.Maybe Prelude.UTCTime)
queryFilters_modifiedAfter = Lens.lens (\QueryFilters' {modifiedAfter} -> modifiedAfter) (\s@QueryFilters' {} a -> s {modifiedAfter = a} :: QueryFilters) Prelude.. Lens.mapping Core._Time

-- | Filter the lineage entities connected to the @StartArn@(s) by a set if
-- property key value pairs. If multiple pairs are provided, an entity is
-- included in the results if it matches any of the provided pairs.
queryFilters_properties :: Lens.Lens' QueryFilters (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
queryFilters_properties = Lens.lens (\QueryFilters' {properties} -> properties) (\s@QueryFilters' {} a -> s {properties = a} :: QueryFilters) Prelude.. Lens.mapping Lens.coerced

-- | Filter the lineage entities connected to the @StartArn@(s) by created
-- date.
queryFilters_createdBefore :: Lens.Lens' QueryFilters (Prelude.Maybe Prelude.UTCTime)
queryFilters_createdBefore = Lens.lens (\QueryFilters' {createdBefore} -> createdBefore) (\s@QueryFilters' {} a -> s {createdBefore = a} :: QueryFilters) Prelude.. Lens.mapping Core._Time

-- | Filter the lineage entities connected to the @StartArn@ by type. For
-- example: @DataSet@, @Model@, @Endpoint@, or @ModelDeployment@.
queryFilters_types :: Lens.Lens' QueryFilters (Prelude.Maybe [Prelude.Text])
queryFilters_types = Lens.lens (\QueryFilters' {types} -> types) (\s@QueryFilters' {} a -> s {types = a} :: QueryFilters) Prelude.. Lens.mapping Lens.coerced

-- | Filter the lineage entities connected to the @StartArn@(s) by the type
-- of the lineage entity.
queryFilters_lineageTypes :: Lens.Lens' QueryFilters (Prelude.Maybe [LineageType])
queryFilters_lineageTypes = Lens.lens (\QueryFilters' {lineageTypes} -> lineageTypes) (\s@QueryFilters' {} a -> s {lineageTypes = a} :: QueryFilters) Prelude.. Lens.mapping Lens.coerced

-- | Filter the lineage entities connected to the @StartArn@(s) after the
-- create date.
queryFilters_createdAfter :: Lens.Lens' QueryFilters (Prelude.Maybe Prelude.UTCTime)
queryFilters_createdAfter = Lens.lens (\QueryFilters' {createdAfter} -> createdAfter) (\s@QueryFilters' {} a -> s {createdAfter = a} :: QueryFilters) Prelude.. Lens.mapping Core._Time

-- | Filter the lineage entities connected to the @StartArn@(s) before the
-- last modified date.
queryFilters_modifiedBefore :: Lens.Lens' QueryFilters (Prelude.Maybe Prelude.UTCTime)
queryFilters_modifiedBefore = Lens.lens (\QueryFilters' {modifiedBefore} -> modifiedBefore) (\s@QueryFilters' {} a -> s {modifiedBefore = a} :: QueryFilters) Prelude.. Lens.mapping Core._Time

instance Prelude.Hashable QueryFilters where
  hashWithSalt _salt QueryFilters' {..} =
    _salt `Prelude.hashWithSalt` modifiedAfter
      `Prelude.hashWithSalt` properties
      `Prelude.hashWithSalt` createdBefore
      `Prelude.hashWithSalt` types
      `Prelude.hashWithSalt` lineageTypes
      `Prelude.hashWithSalt` createdAfter
      `Prelude.hashWithSalt` modifiedBefore

instance Prelude.NFData QueryFilters where
  rnf QueryFilters' {..} =
    Prelude.rnf modifiedAfter
      `Prelude.seq` Prelude.rnf properties
      `Prelude.seq` Prelude.rnf createdBefore
      `Prelude.seq` Prelude.rnf types
      `Prelude.seq` Prelude.rnf lineageTypes
      `Prelude.seq` Prelude.rnf createdAfter
      `Prelude.seq` Prelude.rnf modifiedBefore

instance Core.ToJSON QueryFilters where
  toJSON QueryFilters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ModifiedAfter" Core..=) Prelude.<$> modifiedAfter,
            ("Properties" Core..=) Prelude.<$> properties,
            ("CreatedBefore" Core..=) Prelude.<$> createdBefore,
            ("Types" Core..=) Prelude.<$> types,
            ("LineageTypes" Core..=) Prelude.<$> lineageTypes,
            ("CreatedAfter" Core..=) Prelude.<$> createdAfter,
            ("ModifiedBefore" Core..=)
              Prelude.<$> modifiedBefore
          ]
      )
