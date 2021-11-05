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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.DestinationConfig where

import qualified Amazonka.Core as Core
import Amazonka.Lambda.Types.OnFailure
import Amazonka.Lambda.Types.OnSuccess
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A configuration object that specifies the destination of an event after
-- Lambda processes it.
--
-- /See:/ 'newDestinationConfig' smart constructor.
data DestinationConfig = DestinationConfig'
  { -- | The destination configuration for successful invocations.
    onSuccess :: Prelude.Maybe OnSuccess,
    -- | The destination configuration for failed invocations.
    onFailure :: Prelude.Maybe OnFailure
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
-- 'onSuccess', 'destinationConfig_onSuccess' - The destination configuration for successful invocations.
--
-- 'onFailure', 'destinationConfig_onFailure' - The destination configuration for failed invocations.
newDestinationConfig ::
  DestinationConfig
newDestinationConfig =
  DestinationConfig'
    { onSuccess = Prelude.Nothing,
      onFailure = Prelude.Nothing
    }

-- | The destination configuration for successful invocations.
destinationConfig_onSuccess :: Lens.Lens' DestinationConfig (Prelude.Maybe OnSuccess)
destinationConfig_onSuccess = Lens.lens (\DestinationConfig' {onSuccess} -> onSuccess) (\s@DestinationConfig' {} a -> s {onSuccess = a} :: DestinationConfig)

-- | The destination configuration for failed invocations.
destinationConfig_onFailure :: Lens.Lens' DestinationConfig (Prelude.Maybe OnFailure)
destinationConfig_onFailure = Lens.lens (\DestinationConfig' {onFailure} -> onFailure) (\s@DestinationConfig' {} a -> s {onFailure = a} :: DestinationConfig)

instance Core.FromJSON DestinationConfig where
  parseJSON =
    Core.withObject
      "DestinationConfig"
      ( \x ->
          DestinationConfig'
            Prelude.<$> (x Core..:? "OnSuccess")
            Prelude.<*> (x Core..:? "OnFailure")
      )

instance Prelude.Hashable DestinationConfig

instance Prelude.NFData DestinationConfig

instance Core.ToJSON DestinationConfig where
  toJSON DestinationConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("OnSuccess" Core..=) Prelude.<$> onSuccess,
            ("OnFailure" Core..=) Prelude.<$> onFailure
          ]
      )
