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
-- Module      : Amazonka.IotTwinMaker.Types.Relationship
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.Relationship where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that specifies a relationship with another component type.
--
-- /See:/ 'newRelationship' smart constructor.
data Relationship = Relationship'
  { -- | The ID of the target component type associated with this relationship.
    targetComponentTypeId :: Prelude.Maybe Prelude.Text,
    -- | The type of the relationship.
    relationshipType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Relationship' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetComponentTypeId', 'relationship_targetComponentTypeId' - The ID of the target component type associated with this relationship.
--
-- 'relationshipType', 'relationship_relationshipType' - The type of the relationship.
newRelationship ::
  Relationship
newRelationship =
  Relationship'
    { targetComponentTypeId =
        Prelude.Nothing,
      relationshipType = Prelude.Nothing
    }

-- | The ID of the target component type associated with this relationship.
relationship_targetComponentTypeId :: Lens.Lens' Relationship (Prelude.Maybe Prelude.Text)
relationship_targetComponentTypeId = Lens.lens (\Relationship' {targetComponentTypeId} -> targetComponentTypeId) (\s@Relationship' {} a -> s {targetComponentTypeId = a} :: Relationship)

-- | The type of the relationship.
relationship_relationshipType :: Lens.Lens' Relationship (Prelude.Maybe Prelude.Text)
relationship_relationshipType = Lens.lens (\Relationship' {relationshipType} -> relationshipType) (\s@Relationship' {} a -> s {relationshipType = a} :: Relationship)

instance Data.FromJSON Relationship where
  parseJSON =
    Data.withObject
      "Relationship"
      ( \x ->
          Relationship'
            Prelude.<$> (x Data..:? "targetComponentTypeId")
            Prelude.<*> (x Data..:? "relationshipType")
      )

instance Prelude.Hashable Relationship where
  hashWithSalt _salt Relationship' {..} =
    _salt `Prelude.hashWithSalt` targetComponentTypeId
      `Prelude.hashWithSalt` relationshipType

instance Prelude.NFData Relationship where
  rnf Relationship' {..} =
    Prelude.rnf targetComponentTypeId
      `Prelude.seq` Prelude.rnf relationshipType

instance Data.ToJSON Relationship where
  toJSON Relationship' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("targetComponentTypeId" Data..=)
              Prelude.<$> targetComponentTypeId,
            ("relationshipType" Data..=)
              Prelude.<$> relationshipType
          ]
      )
