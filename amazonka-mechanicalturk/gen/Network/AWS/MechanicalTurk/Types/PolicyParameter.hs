{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types.ParameterMapEntry
import qualified Network.AWS.Prelude as Prelude

-- | Name of the parameter from the Review policy.
--
-- /See:/ 'newPolicyParameter' smart constructor.
data PolicyParameter = PolicyParameter'
  { -- | Name of the parameter from the list of Review Polices.
    key :: Prelude.Maybe Prelude.Text,
    -- | The list of values of the Parameter
    values :: Prelude.Maybe [Prelude.Text],
    -- | List of ParameterMapEntry objects.
    mapEntries :: Prelude.Maybe [ParameterMapEntry]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { key = Prelude.Nothing,
      values = Prelude.Nothing,
      mapEntries = Prelude.Nothing
    }

-- | Name of the parameter from the list of Review Polices.
policyParameter_key :: Lens.Lens' PolicyParameter (Prelude.Maybe Prelude.Text)
policyParameter_key = Lens.lens (\PolicyParameter' {key} -> key) (\s@PolicyParameter' {} a -> s {key = a} :: PolicyParameter)

-- | The list of values of the Parameter
policyParameter_values :: Lens.Lens' PolicyParameter (Prelude.Maybe [Prelude.Text])
policyParameter_values = Lens.lens (\PolicyParameter' {values} -> values) (\s@PolicyParameter' {} a -> s {values = a} :: PolicyParameter) Prelude.. Lens.mapping Prelude._Coerce

-- | List of ParameterMapEntry objects.
policyParameter_mapEntries :: Lens.Lens' PolicyParameter (Prelude.Maybe [ParameterMapEntry])
policyParameter_mapEntries = Lens.lens (\PolicyParameter' {mapEntries} -> mapEntries) (\s@PolicyParameter' {} a -> s {mapEntries = a} :: PolicyParameter) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON PolicyParameter where
  parseJSON =
    Prelude.withObject
      "PolicyParameter"
      ( \x ->
          PolicyParameter'
            Prelude.<$> (x Prelude..:? "Key")
            Prelude.<*> (x Prelude..:? "Values" Prelude..!= Prelude.mempty)
            Prelude.<*> ( x Prelude..:? "MapEntries"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable PolicyParameter

instance Prelude.NFData PolicyParameter

instance Prelude.ToJSON PolicyParameter where
  toJSON PolicyParameter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Key" Prelude..=) Prelude.<$> key,
            ("Values" Prelude..=) Prelude.<$> values,
            ("MapEntries" Prelude..=) Prelude.<$> mapEntries
          ]
      )
