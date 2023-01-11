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
-- Module      : Amazonka.IotTwinMaker.Types.PropertyGroupRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.PropertyGroupRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types.GroupType
import qualified Amazonka.Prelude as Prelude

-- |
--
-- /See:/ 'newPropertyGroupRequest' smart constructor.
data PropertyGroupRequest = PropertyGroupRequest'
  { -- | The group type.
    groupType :: Prelude.Maybe GroupType,
    -- | The names of properties.
    propertyNames :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PropertyGroupRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupType', 'propertyGroupRequest_groupType' - The group type.
--
-- 'propertyNames', 'propertyGroupRequest_propertyNames' - The names of properties.
newPropertyGroupRequest ::
  PropertyGroupRequest
newPropertyGroupRequest =
  PropertyGroupRequest'
    { groupType = Prelude.Nothing,
      propertyNames = Prelude.Nothing
    }

-- | The group type.
propertyGroupRequest_groupType :: Lens.Lens' PropertyGroupRequest (Prelude.Maybe GroupType)
propertyGroupRequest_groupType = Lens.lens (\PropertyGroupRequest' {groupType} -> groupType) (\s@PropertyGroupRequest' {} a -> s {groupType = a} :: PropertyGroupRequest)

-- | The names of properties.
propertyGroupRequest_propertyNames :: Lens.Lens' PropertyGroupRequest (Prelude.Maybe [Prelude.Text])
propertyGroupRequest_propertyNames = Lens.lens (\PropertyGroupRequest' {propertyNames} -> propertyNames) (\s@PropertyGroupRequest' {} a -> s {propertyNames = a} :: PropertyGroupRequest) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable PropertyGroupRequest where
  hashWithSalt _salt PropertyGroupRequest' {..} =
    _salt `Prelude.hashWithSalt` groupType
      `Prelude.hashWithSalt` propertyNames

instance Prelude.NFData PropertyGroupRequest where
  rnf PropertyGroupRequest' {..} =
    Prelude.rnf groupType
      `Prelude.seq` Prelude.rnf propertyNames

instance Data.ToJSON PropertyGroupRequest where
  toJSON PropertyGroupRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("groupType" Data..=) Prelude.<$> groupType,
            ("propertyNames" Data..=) Prelude.<$> propertyNames
          ]
      )
