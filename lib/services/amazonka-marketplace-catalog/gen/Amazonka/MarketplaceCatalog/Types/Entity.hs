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
-- Module      : Amazonka.MarketplaceCatalog.Types.Entity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MarketplaceCatalog.Types.Entity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An entity contains data that describes your product, its supported
-- features, and how it can be used or launched by your customer.
--
-- /See:/ 'newEntity' smart constructor.
data Entity = Entity'
  { -- | The identifier for the entity.
    identifier :: Prelude.Maybe Prelude.Text,
    -- | The type of entity.
    type' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Entity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identifier', 'entity_identifier' - The identifier for the entity.
--
-- 'type'', 'entity_type' - The type of entity.
newEntity ::
  -- | 'type''
  Prelude.Text ->
  Entity
newEntity pType_ =
  Entity'
    { identifier = Prelude.Nothing,
      type' = pType_
    }

-- | The identifier for the entity.
entity_identifier :: Lens.Lens' Entity (Prelude.Maybe Prelude.Text)
entity_identifier = Lens.lens (\Entity' {identifier} -> identifier) (\s@Entity' {} a -> s {identifier = a} :: Entity)

-- | The type of entity.
entity_type :: Lens.Lens' Entity Prelude.Text
entity_type = Lens.lens (\Entity' {type'} -> type') (\s@Entity' {} a -> s {type' = a} :: Entity)

instance Data.FromJSON Entity where
  parseJSON =
    Data.withObject
      "Entity"
      ( \x ->
          Entity'
            Prelude.<$> (x Data..:? "Identifier")
            Prelude.<*> (x Data..: "Type")
      )

instance Prelude.Hashable Entity where
  hashWithSalt _salt Entity' {..} =
    _salt `Prelude.hashWithSalt` identifier
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Entity where
  rnf Entity' {..} =
    Prelude.rnf identifier
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON Entity where
  toJSON Entity' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Identifier" Data..=) Prelude.<$> identifier,
            Prelude.Just ("Type" Data..= type')
          ]
      )
