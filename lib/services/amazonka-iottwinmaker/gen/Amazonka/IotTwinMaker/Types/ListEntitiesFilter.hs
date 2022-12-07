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
-- Module      : Amazonka.IotTwinMaker.Types.ListEntitiesFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.ListEntitiesFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that filters items in a list of entities.
--
-- /See:/ 'newListEntitiesFilter' smart constructor.
data ListEntitiesFilter = ListEntitiesFilter'
  { -- | The parent of the entities in the list.
    parentEntityId :: Prelude.Maybe Prelude.Text,
    -- | The external-Id property of a component. The external-Id property is the
    -- primary key of an external storage system.
    externalId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the component type in the entities in the list.
    componentTypeId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEntitiesFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parentEntityId', 'listEntitiesFilter_parentEntityId' - The parent of the entities in the list.
--
-- 'externalId', 'listEntitiesFilter_externalId' - The external-Id property of a component. The external-Id property is the
-- primary key of an external storage system.
--
-- 'componentTypeId', 'listEntitiesFilter_componentTypeId' - The ID of the component type in the entities in the list.
newListEntitiesFilter ::
  ListEntitiesFilter
newListEntitiesFilter =
  ListEntitiesFilter'
    { parentEntityId =
        Prelude.Nothing,
      externalId = Prelude.Nothing,
      componentTypeId = Prelude.Nothing
    }

-- | The parent of the entities in the list.
listEntitiesFilter_parentEntityId :: Lens.Lens' ListEntitiesFilter (Prelude.Maybe Prelude.Text)
listEntitiesFilter_parentEntityId = Lens.lens (\ListEntitiesFilter' {parentEntityId} -> parentEntityId) (\s@ListEntitiesFilter' {} a -> s {parentEntityId = a} :: ListEntitiesFilter)

-- | The external-Id property of a component. The external-Id property is the
-- primary key of an external storage system.
listEntitiesFilter_externalId :: Lens.Lens' ListEntitiesFilter (Prelude.Maybe Prelude.Text)
listEntitiesFilter_externalId = Lens.lens (\ListEntitiesFilter' {externalId} -> externalId) (\s@ListEntitiesFilter' {} a -> s {externalId = a} :: ListEntitiesFilter)

-- | The ID of the component type in the entities in the list.
listEntitiesFilter_componentTypeId :: Lens.Lens' ListEntitiesFilter (Prelude.Maybe Prelude.Text)
listEntitiesFilter_componentTypeId = Lens.lens (\ListEntitiesFilter' {componentTypeId} -> componentTypeId) (\s@ListEntitiesFilter' {} a -> s {componentTypeId = a} :: ListEntitiesFilter)

instance Prelude.Hashable ListEntitiesFilter where
  hashWithSalt _salt ListEntitiesFilter' {..} =
    _salt `Prelude.hashWithSalt` parentEntityId
      `Prelude.hashWithSalt` externalId
      `Prelude.hashWithSalt` componentTypeId

instance Prelude.NFData ListEntitiesFilter where
  rnf ListEntitiesFilter' {..} =
    Prelude.rnf parentEntityId
      `Prelude.seq` Prelude.rnf externalId
      `Prelude.seq` Prelude.rnf componentTypeId

instance Data.ToJSON ListEntitiesFilter where
  toJSON ListEntitiesFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("parentEntityId" Data..=)
              Prelude.<$> parentEntityId,
            ("externalId" Data..=) Prelude.<$> externalId,
            ("componentTypeId" Data..=)
              Prelude.<$> componentTypeId
          ]
      )
