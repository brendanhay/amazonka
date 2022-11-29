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
-- Module      : Amazonka.IotTwinMaker.Types.ComponentPropertyGroupResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.ComponentPropertyGroupResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IotTwinMaker.Types.GroupType
import qualified Amazonka.Prelude as Prelude

-- | The component property group response.
--
-- /See:/ 'newComponentPropertyGroupResponse' smart constructor.
data ComponentPropertyGroupResponse = ComponentPropertyGroupResponse'
  { -- | The group type.
    groupType :: GroupType,
    -- | The names of properties
    propertyNames :: [Prelude.Text],
    -- | A Boolean value that specifies whether the property group is inherited
    -- from a parent entity
    isInherited :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComponentPropertyGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupType', 'componentPropertyGroupResponse_groupType' - The group type.
--
-- 'propertyNames', 'componentPropertyGroupResponse_propertyNames' - The names of properties
--
-- 'isInherited', 'componentPropertyGroupResponse_isInherited' - A Boolean value that specifies whether the property group is inherited
-- from a parent entity
newComponentPropertyGroupResponse ::
  -- | 'groupType'
  GroupType ->
  -- | 'isInherited'
  Prelude.Bool ->
  ComponentPropertyGroupResponse
newComponentPropertyGroupResponse
  pGroupType_
  pIsInherited_ =
    ComponentPropertyGroupResponse'
      { groupType =
          pGroupType_,
        propertyNames = Prelude.mempty,
        isInherited = pIsInherited_
      }

-- | The group type.
componentPropertyGroupResponse_groupType :: Lens.Lens' ComponentPropertyGroupResponse GroupType
componentPropertyGroupResponse_groupType = Lens.lens (\ComponentPropertyGroupResponse' {groupType} -> groupType) (\s@ComponentPropertyGroupResponse' {} a -> s {groupType = a} :: ComponentPropertyGroupResponse)

-- | The names of properties
componentPropertyGroupResponse_propertyNames :: Lens.Lens' ComponentPropertyGroupResponse [Prelude.Text]
componentPropertyGroupResponse_propertyNames = Lens.lens (\ComponentPropertyGroupResponse' {propertyNames} -> propertyNames) (\s@ComponentPropertyGroupResponse' {} a -> s {propertyNames = a} :: ComponentPropertyGroupResponse) Prelude.. Lens.coerced

-- | A Boolean value that specifies whether the property group is inherited
-- from a parent entity
componentPropertyGroupResponse_isInherited :: Lens.Lens' ComponentPropertyGroupResponse Prelude.Bool
componentPropertyGroupResponse_isInherited = Lens.lens (\ComponentPropertyGroupResponse' {isInherited} -> isInherited) (\s@ComponentPropertyGroupResponse' {} a -> s {isInherited = a} :: ComponentPropertyGroupResponse)

instance Core.FromJSON ComponentPropertyGroupResponse where
  parseJSON =
    Core.withObject
      "ComponentPropertyGroupResponse"
      ( \x ->
          ComponentPropertyGroupResponse'
            Prelude.<$> (x Core..: "groupType")
            Prelude.<*> (x Core..:? "propertyNames" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "isInherited")
      )

instance
  Prelude.Hashable
    ComponentPropertyGroupResponse
  where
  hashWithSalt
    _salt
    ComponentPropertyGroupResponse' {..} =
      _salt `Prelude.hashWithSalt` groupType
        `Prelude.hashWithSalt` propertyNames
        `Prelude.hashWithSalt` isInherited

instance
  Prelude.NFData
    ComponentPropertyGroupResponse
  where
  rnf ComponentPropertyGroupResponse' {..} =
    Prelude.rnf groupType
      `Prelude.seq` Prelude.rnf propertyNames
      `Prelude.seq` Prelude.rnf isInherited
