{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Shield.Types.Contributor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.Contributor where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A contributor to the attack and their contribution.
--
-- /See:/ 'newContributor' smart constructor.
data Contributor = Contributor'
  { -- | The name of the contributor. This is dependent on the
    -- @AttackPropertyIdentifier@. For example, if the
    -- @AttackPropertyIdentifier@ is @SOURCE_COUNTRY@, the @Name@ could be
    -- @United States@.
    name :: Prelude.Maybe Prelude.Text,
    -- | The contribution of this contributor expressed in Protection units. For
    -- example @10,000@.
    value :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Contributor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'contributor_name' - The name of the contributor. This is dependent on the
-- @AttackPropertyIdentifier@. For example, if the
-- @AttackPropertyIdentifier@ is @SOURCE_COUNTRY@, the @Name@ could be
-- @United States@.
--
-- 'value', 'contributor_value' - The contribution of this contributor expressed in Protection units. For
-- example @10,000@.
newContributor ::
  Contributor
newContributor =
  Contributor'
    { name = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the contributor. This is dependent on the
-- @AttackPropertyIdentifier@. For example, if the
-- @AttackPropertyIdentifier@ is @SOURCE_COUNTRY@, the @Name@ could be
-- @United States@.
contributor_name :: Lens.Lens' Contributor (Prelude.Maybe Prelude.Text)
contributor_name = Lens.lens (\Contributor' {name} -> name) (\s@Contributor' {} a -> s {name = a} :: Contributor)

-- | The contribution of this contributor expressed in Protection units. For
-- example @10,000@.
contributor_value :: Lens.Lens' Contributor (Prelude.Maybe Prelude.Integer)
contributor_value = Lens.lens (\Contributor' {value} -> value) (\s@Contributor' {} a -> s {value = a} :: Contributor)

instance Prelude.FromJSON Contributor where
  parseJSON =
    Prelude.withObject
      "Contributor"
      ( \x ->
          Contributor'
            Prelude.<$> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "Value")
      )

instance Prelude.Hashable Contributor

instance Prelude.NFData Contributor
