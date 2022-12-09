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
-- Module      : Amazonka.QuickSight.Types.SetParameterValueConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SetParameterValueConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DestinationParameterValueConfiguration

-- | The configuration of adding parameters in action.
--
-- /See:/ 'newSetParameterValueConfiguration' smart constructor.
data SetParameterValueConfiguration = SetParameterValueConfiguration'
  { -- | The destination parameter name of the @SetParameterValueConfiguration@.
    destinationParameterName :: Prelude.Text,
    value :: DestinationParameterValueConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetParameterValueConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationParameterName', 'setParameterValueConfiguration_destinationParameterName' - The destination parameter name of the @SetParameterValueConfiguration@.
--
-- 'value', 'setParameterValueConfiguration_value' - Undocumented member.
newSetParameterValueConfiguration ::
  -- | 'destinationParameterName'
  Prelude.Text ->
  -- | 'value'
  DestinationParameterValueConfiguration ->
  SetParameterValueConfiguration
newSetParameterValueConfiguration
  pDestinationParameterName_
  pValue_ =
    SetParameterValueConfiguration'
      { destinationParameterName =
          pDestinationParameterName_,
        value = pValue_
      }

-- | The destination parameter name of the @SetParameterValueConfiguration@.
setParameterValueConfiguration_destinationParameterName :: Lens.Lens' SetParameterValueConfiguration Prelude.Text
setParameterValueConfiguration_destinationParameterName = Lens.lens (\SetParameterValueConfiguration' {destinationParameterName} -> destinationParameterName) (\s@SetParameterValueConfiguration' {} a -> s {destinationParameterName = a} :: SetParameterValueConfiguration)

-- | Undocumented member.
setParameterValueConfiguration_value :: Lens.Lens' SetParameterValueConfiguration DestinationParameterValueConfiguration
setParameterValueConfiguration_value = Lens.lens (\SetParameterValueConfiguration' {value} -> value) (\s@SetParameterValueConfiguration' {} a -> s {value = a} :: SetParameterValueConfiguration)

instance Data.FromJSON SetParameterValueConfiguration where
  parseJSON =
    Data.withObject
      "SetParameterValueConfiguration"
      ( \x ->
          SetParameterValueConfiguration'
            Prelude.<$> (x Data..: "DestinationParameterName")
            Prelude.<*> (x Data..: "Value")
      )

instance
  Prelude.Hashable
    SetParameterValueConfiguration
  where
  hashWithSalt
    _salt
    SetParameterValueConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` destinationParameterName
        `Prelude.hashWithSalt` value

instance
  Prelude.NFData
    SetParameterValueConfiguration
  where
  rnf SetParameterValueConfiguration' {..} =
    Prelude.rnf destinationParameterName
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON SetParameterValueConfiguration where
  toJSON SetParameterValueConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "DestinationParameterName"
                  Data..= destinationParameterName
              ),
            Prelude.Just ("Value" Data..= value)
          ]
      )
