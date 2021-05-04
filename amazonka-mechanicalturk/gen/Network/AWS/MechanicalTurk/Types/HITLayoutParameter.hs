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
-- Module      : Network.AWS.MechanicalTurk.Types.HITLayoutParameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.HITLayoutParameter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The HITLayoutParameter data structure defines parameter values used with
-- a HITLayout. A HITLayout is a reusable Amazon Mechanical Turk project
-- template used to provide Human Intelligence Task (HIT) question data for
-- CreateHIT.
--
-- /See:/ 'newHITLayoutParameter' smart constructor.
data HITLayoutParameter = HITLayoutParameter'
  { -- | The name of the parameter in the HITLayout.
    name :: Prelude.Text,
    -- | The value substituted for the parameter referenced in the HITLayout.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'HITLayoutParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'hITLayoutParameter_name' - The name of the parameter in the HITLayout.
--
-- 'value', 'hITLayoutParameter_value' - The value substituted for the parameter referenced in the HITLayout.
newHITLayoutParameter ::
  -- | 'name'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  HITLayoutParameter
newHITLayoutParameter pName_ pValue_ =
  HITLayoutParameter' {name = pName_, value = pValue_}

-- | The name of the parameter in the HITLayout.
hITLayoutParameter_name :: Lens.Lens' HITLayoutParameter Prelude.Text
hITLayoutParameter_name = Lens.lens (\HITLayoutParameter' {name} -> name) (\s@HITLayoutParameter' {} a -> s {name = a} :: HITLayoutParameter)

-- | The value substituted for the parameter referenced in the HITLayout.
hITLayoutParameter_value :: Lens.Lens' HITLayoutParameter Prelude.Text
hITLayoutParameter_value = Lens.lens (\HITLayoutParameter' {value} -> value) (\s@HITLayoutParameter' {} a -> s {value = a} :: HITLayoutParameter)

instance Prelude.Hashable HITLayoutParameter

instance Prelude.NFData HITLayoutParameter

instance Prelude.ToJSON HITLayoutParameter where
  toJSON HITLayoutParameter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Prelude..= name),
            Prelude.Just ("Value" Prelude..= value)
          ]
      )
