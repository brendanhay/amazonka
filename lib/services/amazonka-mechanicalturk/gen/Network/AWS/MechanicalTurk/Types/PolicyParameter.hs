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
import qualified Network.AWS.Prelude as Prelude

-- | Name of the parameter from the Review policy.
--
-- /See:/ 'newPolicyParameter' smart constructor.
data PolicyParameter = PolicyParameter'
  { -- | The list of values of the Parameter
    values :: Prelude.Maybe [Prelude.Text],
    -- | List of ParameterMapEntry objects.
    mapEntries :: Prelude.Maybe [ParameterMapEntry],
    -- | Name of the parameter from the list of Review Polices.
    key :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PolicyParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'policyParameter_values' - The list of values of the Parameter
--
-- 'mapEntries', 'policyParameter_mapEntries' - List of ParameterMapEntry objects.
--
-- 'key', 'policyParameter_key' - Name of the parameter from the list of Review Polices.
newPolicyParameter ::
  PolicyParameter
newPolicyParameter =
  PolicyParameter'
    { values = Prelude.Nothing,
      mapEntries = Prelude.Nothing,
      key = Prelude.Nothing
    }

-- | The list of values of the Parameter
policyParameter_values :: Lens.Lens' PolicyParameter (Prelude.Maybe [Prelude.Text])
policyParameter_values = Lens.lens (\PolicyParameter' {values} -> values) (\s@PolicyParameter' {} a -> s {values = a} :: PolicyParameter) Prelude.. Lens.mapping Lens.coerced

-- | List of ParameterMapEntry objects.
policyParameter_mapEntries :: Lens.Lens' PolicyParameter (Prelude.Maybe [ParameterMapEntry])
policyParameter_mapEntries = Lens.lens (\PolicyParameter' {mapEntries} -> mapEntries) (\s@PolicyParameter' {} a -> s {mapEntries = a} :: PolicyParameter) Prelude.. Lens.mapping Lens.coerced

-- | Name of the parameter from the list of Review Polices.
policyParameter_key :: Lens.Lens' PolicyParameter (Prelude.Maybe Prelude.Text)
policyParameter_key = Lens.lens (\PolicyParameter' {key} -> key) (\s@PolicyParameter' {} a -> s {key = a} :: PolicyParameter)

instance Core.FromJSON PolicyParameter where
  parseJSON =
    Core.withObject
      "PolicyParameter"
      ( \x ->
          PolicyParameter'
            Prelude.<$> (x Core..:? "Values" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "MapEntries" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Key")
      )

instance Prelude.Hashable PolicyParameter

instance Prelude.NFData PolicyParameter

instance Core.ToJSON PolicyParameter where
  toJSON PolicyParameter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Values" Core..=) Prelude.<$> values,
            ("MapEntries" Core..=) Prelude.<$> mapEntries,
            ("Key" Core..=) Prelude.<$> key
          ]
      )
