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
-- Module      : Amazonka.IotTwinMaker.Types.PropertyGroupResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.PropertyGroupResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types.GroupType
import qualified Amazonka.Prelude as Prelude

-- | The property group response
--
-- /See:/ 'newPropertyGroupResponse' smart constructor.
data PropertyGroupResponse = PropertyGroupResponse'
  { -- | The group types.
    groupType :: GroupType,
    -- | The names of properties.
    propertyNames :: [Prelude.Text],
    -- | A Boolean value that specifies whether the property group is inherited
    -- from a parent entity
    isInherited :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PropertyGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupType', 'propertyGroupResponse_groupType' - The group types.
--
-- 'propertyNames', 'propertyGroupResponse_propertyNames' - The names of properties.
--
-- 'isInherited', 'propertyGroupResponse_isInherited' - A Boolean value that specifies whether the property group is inherited
-- from a parent entity
newPropertyGroupResponse ::
  -- | 'groupType'
  GroupType ->
  -- | 'isInherited'
  Prelude.Bool ->
  PropertyGroupResponse
newPropertyGroupResponse pGroupType_ pIsInherited_ =
  PropertyGroupResponse'
    { groupType = pGroupType_,
      propertyNames = Prelude.mempty,
      isInherited = pIsInherited_
    }

-- | The group types.
propertyGroupResponse_groupType :: Lens.Lens' PropertyGroupResponse GroupType
propertyGroupResponse_groupType = Lens.lens (\PropertyGroupResponse' {groupType} -> groupType) (\s@PropertyGroupResponse' {} a -> s {groupType = a} :: PropertyGroupResponse)

-- | The names of properties.
propertyGroupResponse_propertyNames :: Lens.Lens' PropertyGroupResponse [Prelude.Text]
propertyGroupResponse_propertyNames = Lens.lens (\PropertyGroupResponse' {propertyNames} -> propertyNames) (\s@PropertyGroupResponse' {} a -> s {propertyNames = a} :: PropertyGroupResponse) Prelude.. Lens.coerced

-- | A Boolean value that specifies whether the property group is inherited
-- from a parent entity
propertyGroupResponse_isInherited :: Lens.Lens' PropertyGroupResponse Prelude.Bool
propertyGroupResponse_isInherited = Lens.lens (\PropertyGroupResponse' {isInherited} -> isInherited) (\s@PropertyGroupResponse' {} a -> s {isInherited = a} :: PropertyGroupResponse)

instance Data.FromJSON PropertyGroupResponse where
  parseJSON =
    Data.withObject
      "PropertyGroupResponse"
      ( \x ->
          PropertyGroupResponse'
            Prelude.<$> (x Data..: "groupType")
            Prelude.<*> (x Data..:? "propertyNames" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "isInherited")
      )

instance Prelude.Hashable PropertyGroupResponse where
  hashWithSalt _salt PropertyGroupResponse' {..} =
    _salt `Prelude.hashWithSalt` groupType
      `Prelude.hashWithSalt` propertyNames
      `Prelude.hashWithSalt` isInherited

instance Prelude.NFData PropertyGroupResponse where
  rnf PropertyGroupResponse' {..} =
    Prelude.rnf groupType
      `Prelude.seq` Prelude.rnf propertyNames
      `Prelude.seq` Prelude.rnf isInherited
