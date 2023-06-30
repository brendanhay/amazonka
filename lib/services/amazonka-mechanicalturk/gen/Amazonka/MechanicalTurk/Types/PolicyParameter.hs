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
-- Module      : Amazonka.MechanicalTurk.Types.PolicyParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MechanicalTurk.Types.PolicyParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MechanicalTurk.Types.ParameterMapEntry
import qualified Amazonka.Prelude as Prelude

-- | Name of the parameter from the Review policy.
--
-- /See:/ 'newPolicyParameter' smart constructor.
data PolicyParameter = PolicyParameter'
  { -- | Name of the parameter from the list of Review Polices.
    key :: Prelude.Maybe Prelude.Text,
    -- | List of ParameterMapEntry objects.
    mapEntries :: Prelude.Maybe [ParameterMapEntry],
    -- | The list of values of the Parameter
    values :: Prelude.Maybe [Prelude.Text]
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
-- 'key', 'policyParameter_key' - Name of the parameter from the list of Review Polices.
--
-- 'mapEntries', 'policyParameter_mapEntries' - List of ParameterMapEntry objects.
--
-- 'values', 'policyParameter_values' - The list of values of the Parameter
newPolicyParameter ::
  PolicyParameter
newPolicyParameter =
  PolicyParameter'
    { key = Prelude.Nothing,
      mapEntries = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | Name of the parameter from the list of Review Polices.
policyParameter_key :: Lens.Lens' PolicyParameter (Prelude.Maybe Prelude.Text)
policyParameter_key = Lens.lens (\PolicyParameter' {key} -> key) (\s@PolicyParameter' {} a -> s {key = a} :: PolicyParameter)

-- | List of ParameterMapEntry objects.
policyParameter_mapEntries :: Lens.Lens' PolicyParameter (Prelude.Maybe [ParameterMapEntry])
policyParameter_mapEntries = Lens.lens (\PolicyParameter' {mapEntries} -> mapEntries) (\s@PolicyParameter' {} a -> s {mapEntries = a} :: PolicyParameter) Prelude.. Lens.mapping Lens.coerced

-- | The list of values of the Parameter
policyParameter_values :: Lens.Lens' PolicyParameter (Prelude.Maybe [Prelude.Text])
policyParameter_values = Lens.lens (\PolicyParameter' {values} -> values) (\s@PolicyParameter' {} a -> s {values = a} :: PolicyParameter) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON PolicyParameter where
  parseJSON =
    Data.withObject
      "PolicyParameter"
      ( \x ->
          PolicyParameter'
            Prelude.<$> (x Data..:? "Key")
            Prelude.<*> (x Data..:? "MapEntries" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Values" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable PolicyParameter where
  hashWithSalt _salt PolicyParameter' {..} =
    _salt
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` mapEntries
      `Prelude.hashWithSalt` values

instance Prelude.NFData PolicyParameter where
  rnf PolicyParameter' {..} =
    Prelude.rnf key
      `Prelude.seq` Prelude.rnf mapEntries
      `Prelude.seq` Prelude.rnf values

instance Data.ToJSON PolicyParameter where
  toJSON PolicyParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Key" Data..=) Prelude.<$> key,
            ("MapEntries" Data..=) Prelude.<$> mapEntries,
            ("Values" Data..=) Prelude.<$> values
          ]
      )
