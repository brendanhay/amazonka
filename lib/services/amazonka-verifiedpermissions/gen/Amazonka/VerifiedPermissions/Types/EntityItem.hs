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
-- Module      : Amazonka.VerifiedPermissions.Types.EntityItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VerifiedPermissions.Types.EntityItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VerifiedPermissions.Types.AttributeValue
import Amazonka.VerifiedPermissions.Types.EntityIdentifier

-- | Contains information about an entity that can be referenced in a Cedar
-- policy.
--
-- This data type is used as one of the fields in the
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_EntitiesDefinition.html EntitiesDefinition>
-- structure.
--
-- @{ \"id\": { \"entityType\": \"Photo\", \"entityId\": \"VacationPhoto94.jpg\" }, \"Attributes\": {}, \"Parents\": [ { \"entityType\": \"Album\", \"entityId\": \"alice_folder\" } ] }@
--
-- /See:/ 'newEntityItem' smart constructor.
data EntityItem = EntityItem'
  { -- | A list of attributes for the entity.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue),
    -- | The parents in the hierarchy that contains the entity.
    parents :: Prelude.Maybe [EntityIdentifier],
    -- | The identifier of the entity.
    identifier :: EntityIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EntityItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'entityItem_attributes' - A list of attributes for the entity.
--
-- 'parents', 'entityItem_parents' - The parents in the hierarchy that contains the entity.
--
-- 'identifier', 'entityItem_identifier' - The identifier of the entity.
newEntityItem ::
  -- | 'identifier'
  EntityIdentifier ->
  EntityItem
newEntityItem pIdentifier_ =
  EntityItem'
    { attributes = Prelude.Nothing,
      parents = Prelude.Nothing,
      identifier = pIdentifier_
    }

-- | A list of attributes for the entity.
entityItem_attributes :: Lens.Lens' EntityItem (Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue))
entityItem_attributes = Lens.lens (\EntityItem' {attributes} -> attributes) (\s@EntityItem' {} a -> s {attributes = a} :: EntityItem) Prelude.. Lens.mapping Lens.coerced

-- | The parents in the hierarchy that contains the entity.
entityItem_parents :: Lens.Lens' EntityItem (Prelude.Maybe [EntityIdentifier])
entityItem_parents = Lens.lens (\EntityItem' {parents} -> parents) (\s@EntityItem' {} a -> s {parents = a} :: EntityItem) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the entity.
entityItem_identifier :: Lens.Lens' EntityItem EntityIdentifier
entityItem_identifier = Lens.lens (\EntityItem' {identifier} -> identifier) (\s@EntityItem' {} a -> s {identifier = a} :: EntityItem)

instance Prelude.Hashable EntityItem where
  hashWithSalt _salt EntityItem' {..} =
    _salt
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` parents
      `Prelude.hashWithSalt` identifier

instance Prelude.NFData EntityItem where
  rnf EntityItem' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf parents
      `Prelude.seq` Prelude.rnf identifier

instance Data.ToJSON EntityItem where
  toJSON EntityItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("attributes" Data..=) Prelude.<$> attributes,
            ("parents" Data..=) Prelude.<$> parents,
            Prelude.Just ("identifier" Data..= identifier)
          ]
      )
