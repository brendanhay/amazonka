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
-- Module      : Amazonka.IotTwinMaker.Types.RelationshipValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.RelationshipValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A value that associates a component and an entity.
--
-- /See:/ 'newRelationshipValue' smart constructor.
data RelationshipValue = RelationshipValue'
  { -- | The name of the target component associated with the relationship value.
    targetComponentName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the target entity associated with this relationship value.
    targetEntityId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RelationshipValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetComponentName', 'relationshipValue_targetComponentName' - The name of the target component associated with the relationship value.
--
-- 'targetEntityId', 'relationshipValue_targetEntityId' - The ID of the target entity associated with this relationship value.
newRelationshipValue ::
  RelationshipValue
newRelationshipValue =
  RelationshipValue'
    { targetComponentName =
        Prelude.Nothing,
      targetEntityId = Prelude.Nothing
    }

-- | The name of the target component associated with the relationship value.
relationshipValue_targetComponentName :: Lens.Lens' RelationshipValue (Prelude.Maybe Prelude.Text)
relationshipValue_targetComponentName = Lens.lens (\RelationshipValue' {targetComponentName} -> targetComponentName) (\s@RelationshipValue' {} a -> s {targetComponentName = a} :: RelationshipValue)

-- | The ID of the target entity associated with this relationship value.
relationshipValue_targetEntityId :: Lens.Lens' RelationshipValue (Prelude.Maybe Prelude.Text)
relationshipValue_targetEntityId = Lens.lens (\RelationshipValue' {targetEntityId} -> targetEntityId) (\s@RelationshipValue' {} a -> s {targetEntityId = a} :: RelationshipValue)

instance Data.FromJSON RelationshipValue where
  parseJSON =
    Data.withObject
      "RelationshipValue"
      ( \x ->
          RelationshipValue'
            Prelude.<$> (x Data..:? "targetComponentName")
            Prelude.<*> (x Data..:? "targetEntityId")
      )

instance Prelude.Hashable RelationshipValue where
  hashWithSalt _salt RelationshipValue' {..} =
    _salt
      `Prelude.hashWithSalt` targetComponentName
      `Prelude.hashWithSalt` targetEntityId

instance Prelude.NFData RelationshipValue where
  rnf RelationshipValue' {..} =
    Prelude.rnf targetComponentName
      `Prelude.seq` Prelude.rnf targetEntityId

instance Data.ToJSON RelationshipValue where
  toJSON RelationshipValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("targetComponentName" Data..=)
              Prelude.<$> targetComponentName,
            ("targetEntityId" Data..=)
              Prelude.<$> targetEntityId
          ]
      )
