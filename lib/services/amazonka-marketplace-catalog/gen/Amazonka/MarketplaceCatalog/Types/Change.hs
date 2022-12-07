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
-- Module      : Amazonka.MarketplaceCatalog.Types.Change
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MarketplaceCatalog.Types.Change where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MarketplaceCatalog.Types.Entity
import Amazonka.MarketplaceCatalog.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | An object that contains the @ChangeType@, @Details@, and @Entity@.
--
-- /See:/ 'newChange' smart constructor.
data Change = Change'
  { -- | Optional name for the change.
    changeName :: Prelude.Maybe Prelude.Text,
    -- | The tags associated with the change.
    entityTags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | Change types are single string values that describe your intention for
    -- the change. Each change type is unique for each @EntityType@ provided in
    -- the change\'s scope.
    changeType :: Prelude.Text,
    -- | The entity to be changed.
    entity :: Entity,
    -- | This object contains details specific to the change type of the
    -- requested change.
    details :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Change' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeName', 'change_changeName' - Optional name for the change.
--
-- 'entityTags', 'change_entityTags' - The tags associated with the change.
--
-- 'changeType', 'change_changeType' - Change types are single string values that describe your intention for
-- the change. Each change type is unique for each @EntityType@ provided in
-- the change\'s scope.
--
-- 'entity', 'change_entity' - The entity to be changed.
--
-- 'details', 'change_details' - This object contains details specific to the change type of the
-- requested change.
newChange ::
  -- | 'changeType'
  Prelude.Text ->
  -- | 'entity'
  Entity ->
  -- | 'details'
  Prelude.Text ->
  Change
newChange pChangeType_ pEntity_ pDetails_ =
  Change'
    { changeName = Prelude.Nothing,
      entityTags = Prelude.Nothing,
      changeType = pChangeType_,
      entity = pEntity_,
      details = pDetails_
    }

-- | Optional name for the change.
change_changeName :: Lens.Lens' Change (Prelude.Maybe Prelude.Text)
change_changeName = Lens.lens (\Change' {changeName} -> changeName) (\s@Change' {} a -> s {changeName = a} :: Change)

-- | The tags associated with the change.
change_entityTags :: Lens.Lens' Change (Prelude.Maybe (Prelude.NonEmpty Tag))
change_entityTags = Lens.lens (\Change' {entityTags} -> entityTags) (\s@Change' {} a -> s {entityTags = a} :: Change) Prelude.. Lens.mapping Lens.coerced

-- | Change types are single string values that describe your intention for
-- the change. Each change type is unique for each @EntityType@ provided in
-- the change\'s scope.
change_changeType :: Lens.Lens' Change Prelude.Text
change_changeType = Lens.lens (\Change' {changeType} -> changeType) (\s@Change' {} a -> s {changeType = a} :: Change)

-- | The entity to be changed.
change_entity :: Lens.Lens' Change Entity
change_entity = Lens.lens (\Change' {entity} -> entity) (\s@Change' {} a -> s {entity = a} :: Change)

-- | This object contains details specific to the change type of the
-- requested change.
change_details :: Lens.Lens' Change Prelude.Text
change_details = Lens.lens (\Change' {details} -> details) (\s@Change' {} a -> s {details = a} :: Change)

instance Prelude.Hashable Change where
  hashWithSalt _salt Change' {..} =
    _salt `Prelude.hashWithSalt` changeName
      `Prelude.hashWithSalt` entityTags
      `Prelude.hashWithSalt` changeType
      `Prelude.hashWithSalt` entity
      `Prelude.hashWithSalt` details

instance Prelude.NFData Change where
  rnf Change' {..} =
    Prelude.rnf changeName
      `Prelude.seq` Prelude.rnf entityTags
      `Prelude.seq` Prelude.rnf changeType
      `Prelude.seq` Prelude.rnf entity
      `Prelude.seq` Prelude.rnf details

instance Data.ToJSON Change where
  toJSON Change' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ChangeName" Data..=) Prelude.<$> changeName,
            ("EntityTags" Data..=) Prelude.<$> entityTags,
            Prelude.Just ("ChangeType" Data..= changeType),
            Prelude.Just ("Entity" Data..= entity),
            Prelude.Just ("Details" Data..= details)
          ]
      )
