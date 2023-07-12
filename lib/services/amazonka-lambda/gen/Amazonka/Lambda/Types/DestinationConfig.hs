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
-- Module      : Amazonka.Lambda.Types.DestinationConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.DestinationConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types.OnFailure
import Amazonka.Lambda.Types.OnSuccess
import qualified Amazonka.Prelude as Prelude

-- | A configuration object that specifies the destination of an event after
-- Lambda processes it.
--
-- /See:/ 'newDestinationConfig' smart constructor.
data DestinationConfig = DestinationConfig'
  { -- | The destination configuration for failed invocations.
    onFailure :: Prelude.Maybe OnFailure,
    -- | The destination configuration for successful invocations.
    onSuccess :: Prelude.Maybe OnSuccess
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DestinationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'onFailure', 'destinationConfig_onFailure' - The destination configuration for failed invocations.
--
-- 'onSuccess', 'destinationConfig_onSuccess' - The destination configuration for successful invocations.
newDestinationConfig ::
  DestinationConfig
newDestinationConfig =
  DestinationConfig'
    { onFailure = Prelude.Nothing,
      onSuccess = Prelude.Nothing
    }

-- | The destination configuration for failed invocations.
destinationConfig_onFailure :: Lens.Lens' DestinationConfig (Prelude.Maybe OnFailure)
destinationConfig_onFailure = Lens.lens (\DestinationConfig' {onFailure} -> onFailure) (\s@DestinationConfig' {} a -> s {onFailure = a} :: DestinationConfig)

-- | The destination configuration for successful invocations.
destinationConfig_onSuccess :: Lens.Lens' DestinationConfig (Prelude.Maybe OnSuccess)
destinationConfig_onSuccess = Lens.lens (\DestinationConfig' {onSuccess} -> onSuccess) (\s@DestinationConfig' {} a -> s {onSuccess = a} :: DestinationConfig)

instance Data.FromJSON DestinationConfig where
  parseJSON =
    Data.withObject
      "DestinationConfig"
      ( \x ->
          DestinationConfig'
            Prelude.<$> (x Data..:? "OnFailure")
            Prelude.<*> (x Data..:? "OnSuccess")
      )

instance Prelude.Hashable DestinationConfig where
  hashWithSalt _salt DestinationConfig' {..} =
    _salt
      `Prelude.hashWithSalt` onFailure
      `Prelude.hashWithSalt` onSuccess

instance Prelude.NFData DestinationConfig where
  rnf DestinationConfig' {..} =
    Prelude.rnf onFailure
      `Prelude.seq` Prelude.rnf onSuccess

instance Data.ToJSON DestinationConfig where
  toJSON DestinationConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("OnFailure" Data..=) Prelude.<$> onFailure,
            ("OnSuccess" Data..=) Prelude.<$> onSuccess
          ]
      )
