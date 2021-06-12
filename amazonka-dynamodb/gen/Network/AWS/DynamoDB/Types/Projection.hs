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
-- Module      : Network.AWS.DynamoDB.Types.Projection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.Projection where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.ProjectionType
import qualified Network.AWS.Lens as Lens

-- | Represents attributes that are copied (projected) from the table into an
-- index. These are in addition to the primary key attributes and index key
-- attributes, which are automatically projected.
--
-- /See:/ 'newProjection' smart constructor.
data Projection = Projection'
  { -- | Represents the non-key attribute names which will be projected into the
    -- index.
    --
    -- For local secondary indexes, the total count of @NonKeyAttributes@
    -- summed across all of the local secondary indexes, must not exceed 20. If
    -- you project the same attribute into two different indexes, this counts
    -- as two distinct attributes when determining the total.
    nonKeyAttributes :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The set of attributes that are projected into the index:
    --
    -- -   @KEYS_ONLY@ - Only the index and primary keys are projected into the
    --     index.
    --
    -- -   @INCLUDE@ - In addition to the attributes described in @KEYS_ONLY@,
    --     the secondary index will include other non-key attributes that you
    --     specify.
    --
    -- -   @ALL@ - All of the table attributes are projected into the index.
    projectionType :: Core.Maybe ProjectionType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Projection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nonKeyAttributes', 'projection_nonKeyAttributes' - Represents the non-key attribute names which will be projected into the
-- index.
--
-- For local secondary indexes, the total count of @NonKeyAttributes@
-- summed across all of the local secondary indexes, must not exceed 20. If
-- you project the same attribute into two different indexes, this counts
-- as two distinct attributes when determining the total.
--
-- 'projectionType', 'projection_projectionType' - The set of attributes that are projected into the index:
--
-- -   @KEYS_ONLY@ - Only the index and primary keys are projected into the
--     index.
--
-- -   @INCLUDE@ - In addition to the attributes described in @KEYS_ONLY@,
--     the secondary index will include other non-key attributes that you
--     specify.
--
-- -   @ALL@ - All of the table attributes are projected into the index.
newProjection ::
  Projection
newProjection =
  Projection'
    { nonKeyAttributes = Core.Nothing,
      projectionType = Core.Nothing
    }

-- | Represents the non-key attribute names which will be projected into the
-- index.
--
-- For local secondary indexes, the total count of @NonKeyAttributes@
-- summed across all of the local secondary indexes, must not exceed 20. If
-- you project the same attribute into two different indexes, this counts
-- as two distinct attributes when determining the total.
projection_nonKeyAttributes :: Lens.Lens' Projection (Core.Maybe (Core.NonEmpty Core.Text))
projection_nonKeyAttributes = Lens.lens (\Projection' {nonKeyAttributes} -> nonKeyAttributes) (\s@Projection' {} a -> s {nonKeyAttributes = a} :: Projection) Core.. Lens.mapping Lens._Coerce

-- | The set of attributes that are projected into the index:
--
-- -   @KEYS_ONLY@ - Only the index and primary keys are projected into the
--     index.
--
-- -   @INCLUDE@ - In addition to the attributes described in @KEYS_ONLY@,
--     the secondary index will include other non-key attributes that you
--     specify.
--
-- -   @ALL@ - All of the table attributes are projected into the index.
projection_projectionType :: Lens.Lens' Projection (Core.Maybe ProjectionType)
projection_projectionType = Lens.lens (\Projection' {projectionType} -> projectionType) (\s@Projection' {} a -> s {projectionType = a} :: Projection)

instance Core.FromJSON Projection where
  parseJSON =
    Core.withObject
      "Projection"
      ( \x ->
          Projection'
            Core.<$> (x Core..:? "NonKeyAttributes")
            Core.<*> (x Core..:? "ProjectionType")
      )

instance Core.Hashable Projection

instance Core.NFData Projection

instance Core.ToJSON Projection where
  toJSON Projection' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NonKeyAttributes" Core..=)
              Core.<$> nonKeyAttributes,
            ("ProjectionType" Core..=) Core.<$> projectionType
          ]
      )
