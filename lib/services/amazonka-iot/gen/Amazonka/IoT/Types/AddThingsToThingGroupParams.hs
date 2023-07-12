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
-- Module      : Amazonka.IoT.Types.AddThingsToThingGroupParams
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.AddThingsToThingGroupParams where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
        Lens.coerced Lens.# pThingGroupNames_
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
addThingsToThingGroupParams_thingGroupNames = Lens.lens (\AddThingsToThingGroupParams' {thingGroupNames} -> thingGroupNames) (\s@AddThingsToThingGroupParams' {} a -> s {thingGroupNames = a} :: AddThingsToThingGroupParams) Prelude.. Lens.coerced

instance Data.FromJSON AddThingsToThingGroupParams where
  parseJSON =
    Data.withObject
      "AddThingsToThingGroupParams"
      ( \x ->
          AddThingsToThingGroupParams'
            Prelude.<$> (x Data..:? "overrideDynamicGroups")
            Prelude.<*> (x Data..: "thingGroupNames")
      )

instance Prelude.Hashable AddThingsToThingGroupParams where
  hashWithSalt _salt AddThingsToThingGroupParams' {..} =
    _salt
      `Prelude.hashWithSalt` overrideDynamicGroups
      `Prelude.hashWithSalt` thingGroupNames

instance Prelude.NFData AddThingsToThingGroupParams where
  rnf AddThingsToThingGroupParams' {..} =
    Prelude.rnf overrideDynamicGroups
      `Prelude.seq` Prelude.rnf thingGroupNames

instance Data.ToJSON AddThingsToThingGroupParams where
  toJSON AddThingsToThingGroupParams' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("overrideDynamicGroups" Data..=)
              Prelude.<$> overrideDynamicGroups,
            Prelude.Just
              ("thingGroupNames" Data..= thingGroupNames)
          ]
      )
