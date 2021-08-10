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
-- Module      : Network.AWS.IoT.Types.AddThingsToThingGroupParams
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AddThingsToThingGroupParams where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Parameters used when defining a mitigation action that move a set of
-- things to a thing group.
--
-- /See:/ 'newAddThingsToThingGroupParams' smart constructor.
data AddThingsToThingGroupParams = AddThingsToThingGroupParams'
  { -- | Specifies if this mitigation action can move the things that triggered
    -- the mitigation action even if they are part of one or more dynamic thing
    -- groups.
    overrideDynamicGroups :: Prelude.Maybe Prelude.Bool,
    -- | The list of groups to which you want to add the things that triggered
    -- the mitigation action. You can add a thing to a maximum of 10 groups,
    -- but you can\'t add a thing to more than one group in the same hierarchy.
    thingGroupNames :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddThingsToThingGroupParams' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'overrideDynamicGroups', 'addThingsToThingGroupParams_overrideDynamicGroups' - Specifies if this mitigation action can move the things that triggered
-- the mitigation action even if they are part of one or more dynamic thing
-- groups.
--
-- 'thingGroupNames', 'addThingsToThingGroupParams_thingGroupNames' - The list of groups to which you want to add the things that triggered
-- the mitigation action. You can add a thing to a maximum of 10 groups,
-- but you can\'t add a thing to more than one group in the same hierarchy.
newAddThingsToThingGroupParams ::
  -- | 'thingGroupNames'
  Prelude.NonEmpty Prelude.Text ->
  AddThingsToThingGroupParams
newAddThingsToThingGroupParams pThingGroupNames_ =
  AddThingsToThingGroupParams'
    { overrideDynamicGroups =
        Prelude.Nothing,
      thingGroupNames =
        Lens._Coerce Lens.# pThingGroupNames_
    }

-- | Specifies if this mitigation action can move the things that triggered
-- the mitigation action even if they are part of one or more dynamic thing
-- groups.
addThingsToThingGroupParams_overrideDynamicGroups :: Lens.Lens' AddThingsToThingGroupParams (Prelude.Maybe Prelude.Bool)
addThingsToThingGroupParams_overrideDynamicGroups = Lens.lens (\AddThingsToThingGroupParams' {overrideDynamicGroups} -> overrideDynamicGroups) (\s@AddThingsToThingGroupParams' {} a -> s {overrideDynamicGroups = a} :: AddThingsToThingGroupParams)

-- | The list of groups to which you want to add the things that triggered
-- the mitigation action. You can add a thing to a maximum of 10 groups,
-- but you can\'t add a thing to more than one group in the same hierarchy.
addThingsToThingGroupParams_thingGroupNames :: Lens.Lens' AddThingsToThingGroupParams (Prelude.NonEmpty Prelude.Text)
addThingsToThingGroupParams_thingGroupNames = Lens.lens (\AddThingsToThingGroupParams' {thingGroupNames} -> thingGroupNames) (\s@AddThingsToThingGroupParams' {} a -> s {thingGroupNames = a} :: AddThingsToThingGroupParams) Prelude.. Lens._Coerce

instance Core.FromJSON AddThingsToThingGroupParams where
  parseJSON =
    Core.withObject
      "AddThingsToThingGroupParams"
      ( \x ->
          AddThingsToThingGroupParams'
            Prelude.<$> (x Core..:? "overrideDynamicGroups")
            Prelude.<*> (x Core..: "thingGroupNames")
      )

instance Prelude.Hashable AddThingsToThingGroupParams

instance Prelude.NFData AddThingsToThingGroupParams

instance Core.ToJSON AddThingsToThingGroupParams where
  toJSON AddThingsToThingGroupParams' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("overrideDynamicGroups" Core..=)
              Prelude.<$> overrideDynamicGroups,
            Prelude.Just
              ("thingGroupNames" Core..= thingGroupNames)
          ]
      )
