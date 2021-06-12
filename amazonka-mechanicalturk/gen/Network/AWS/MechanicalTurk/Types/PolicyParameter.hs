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
-- Module      : Network.AWS.MechanicalTurk.Types.PolicyParameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.PolicyParameter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types.ParameterMapEntry

-- | Name of the parameter from the Review policy.
--
-- /See:/ 'newPolicyParameter' smart constructor.
data PolicyParameter = PolicyParameter'
  { -- | Name of the parameter from the list of Review Polices.
    key :: Core.Maybe Core.Text,
    -- | The list of values of the Parameter
    values :: Core.Maybe [Core.Text],
    -- | List of ParameterMapEntry objects.
    mapEntries :: Core.Maybe [ParameterMapEntry]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PolicyParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'policyParameter_key' - Name of the parameter from the list of Review Polices.
--
-- 'values', 'policyParameter_values' - The list of values of the Parameter
--
-- 'mapEntries', 'policyParameter_mapEntries' - List of ParameterMapEntry objects.
newPolicyParameter ::
  PolicyParameter
newPolicyParameter =
  PolicyParameter'
    { key = Core.Nothing,
      values = Core.Nothing,
      mapEntries = Core.Nothing
    }

-- | Name of the parameter from the list of Review Polices.
policyParameter_key :: Lens.Lens' PolicyParameter (Core.Maybe Core.Text)
policyParameter_key = Lens.lens (\PolicyParameter' {key} -> key) (\s@PolicyParameter' {} a -> s {key = a} :: PolicyParameter)

-- | The list of values of the Parameter
policyParameter_values :: Lens.Lens' PolicyParameter (Core.Maybe [Core.Text])
policyParameter_values = Lens.lens (\PolicyParameter' {values} -> values) (\s@PolicyParameter' {} a -> s {values = a} :: PolicyParameter) Core.. Lens.mapping Lens._Coerce

-- | List of ParameterMapEntry objects.
policyParameter_mapEntries :: Lens.Lens' PolicyParameter (Core.Maybe [ParameterMapEntry])
policyParameter_mapEntries = Lens.lens (\PolicyParameter' {mapEntries} -> mapEntries) (\s@PolicyParameter' {} a -> s {mapEntries = a} :: PolicyParameter) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON PolicyParameter where
  parseJSON =
    Core.withObject
      "PolicyParameter"
      ( \x ->
          PolicyParameter'
            Core.<$> (x Core..:? "Key")
            Core.<*> (x Core..:? "Values" Core..!= Core.mempty)
            Core.<*> (x Core..:? "MapEntries" Core..!= Core.mempty)
      )

instance Core.Hashable PolicyParameter

instance Core.NFData PolicyParameter

instance Core.ToJSON PolicyParameter where
  toJSON PolicyParameter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Key" Core..=) Core.<$> key,
            ("Values" Core..=) Core.<$> values,
            ("MapEntries" Core..=) Core.<$> mapEntries
          ]
      )
