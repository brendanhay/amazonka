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
-- Module      : Amazonka.IoTEvents.Types.InitializationConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.InitializationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the default alarm state. The configuration applies to all
-- alarms that were created based on this alarm model.
--
-- /See:/ 'newInitializationConfiguration' smart constructor.
data InitializationConfiguration = InitializationConfiguration'
  { -- | The value must be @TRUE@ or @FALSE@. If @FALSE@, all alarm instances
    -- created based on the alarm model are activated. The default value is
    -- @TRUE@.
    disabledOnInitialization :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InitializationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'disabledOnInitialization', 'initializationConfiguration_disabledOnInitialization' - The value must be @TRUE@ or @FALSE@. If @FALSE@, all alarm instances
-- created based on the alarm model are activated. The default value is
-- @TRUE@.
newInitializationConfiguration ::
  -- | 'disabledOnInitialization'
  Prelude.Bool ->
  InitializationConfiguration
newInitializationConfiguration
  pDisabledOnInitialization_ =
    InitializationConfiguration'
      { disabledOnInitialization =
          pDisabledOnInitialization_
      }

-- | The value must be @TRUE@ or @FALSE@. If @FALSE@, all alarm instances
-- created based on the alarm model are activated. The default value is
-- @TRUE@.
initializationConfiguration_disabledOnInitialization :: Lens.Lens' InitializationConfiguration Prelude.Bool
initializationConfiguration_disabledOnInitialization = Lens.lens (\InitializationConfiguration' {disabledOnInitialization} -> disabledOnInitialization) (\s@InitializationConfiguration' {} a -> s {disabledOnInitialization = a} :: InitializationConfiguration)

instance Data.FromJSON InitializationConfiguration where
  parseJSON =
    Data.withObject
      "InitializationConfiguration"
      ( \x ->
          InitializationConfiguration'
            Prelude.<$> (x Data..: "disabledOnInitialization")
      )

instance Prelude.Hashable InitializationConfiguration where
  hashWithSalt _salt InitializationConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` disabledOnInitialization

instance Prelude.NFData InitializationConfiguration where
  rnf InitializationConfiguration' {..} =
    Prelude.rnf disabledOnInitialization

instance Data.ToJSON InitializationConfiguration where
  toJSON InitializationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "disabledOnInitialization"
                  Data..= disabledOnInitialization
              )
          ]
      )
