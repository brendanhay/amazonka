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
-- Module      : Network.AWS.Lambda.Types.DestinationConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.DestinationConfig where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types.OnFailure
import Network.AWS.Lambda.Types.OnSuccess
import qualified Network.AWS.Lens as Lens

-- | A configuration object that specifies the destination of an event after
-- Lambda processes it.
--
-- /See:/ 'newDestinationConfig' smart constructor.
data DestinationConfig = DestinationConfig'
  { -- | The destination configuration for failed invocations.
    onFailure :: Core.Maybe OnFailure,
    -- | The destination configuration for successful invocations.
    onSuccess :: Core.Maybe OnSuccess
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { onFailure = Core.Nothing,
      onSuccess = Core.Nothing
    }

-- | The destination configuration for failed invocations.
destinationConfig_onFailure :: Lens.Lens' DestinationConfig (Core.Maybe OnFailure)
destinationConfig_onFailure = Lens.lens (\DestinationConfig' {onFailure} -> onFailure) (\s@DestinationConfig' {} a -> s {onFailure = a} :: DestinationConfig)

-- | The destination configuration for successful invocations.
destinationConfig_onSuccess :: Lens.Lens' DestinationConfig (Core.Maybe OnSuccess)
destinationConfig_onSuccess = Lens.lens (\DestinationConfig' {onSuccess} -> onSuccess) (\s@DestinationConfig' {} a -> s {onSuccess = a} :: DestinationConfig)

instance Core.FromJSON DestinationConfig where
  parseJSON =
    Core.withObject
      "DestinationConfig"
      ( \x ->
          DestinationConfig'
            Core.<$> (x Core..:? "OnFailure")
            Core.<*> (x Core..:? "OnSuccess")
      )

instance Core.Hashable DestinationConfig

instance Core.NFData DestinationConfig

instance Core.ToJSON DestinationConfig where
  toJSON DestinationConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("OnFailure" Core..=) Core.<$> onFailure,
            ("OnSuccess" Core..=) Core.<$> onSuccess
          ]
      )
