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
-- Module      : Network.AWS.APIGateway.Types.ThrottleSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.ThrottleSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The API request rate limits.
--
-- /See:/ 'newThrottleSettings' smart constructor.
data ThrottleSettings = ThrottleSettings'
  { -- | The API request burst limit, the maximum rate limit over a time ranging
    -- from one to a few seconds, depending upon whether the underlying token
    -- bucket is at its full capacity.
    burstLimit :: Core.Maybe Core.Int,
    -- | The API request steady-state rate limit.
    rateLimit :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ThrottleSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'burstLimit', 'throttleSettings_burstLimit' - The API request burst limit, the maximum rate limit over a time ranging
-- from one to a few seconds, depending upon whether the underlying token
-- bucket is at its full capacity.
--
-- 'rateLimit', 'throttleSettings_rateLimit' - The API request steady-state rate limit.
newThrottleSettings ::
  ThrottleSettings
newThrottleSettings =
  ThrottleSettings'
    { burstLimit = Core.Nothing,
      rateLimit = Core.Nothing
    }

-- | The API request burst limit, the maximum rate limit over a time ranging
-- from one to a few seconds, depending upon whether the underlying token
-- bucket is at its full capacity.
throttleSettings_burstLimit :: Lens.Lens' ThrottleSettings (Core.Maybe Core.Int)
throttleSettings_burstLimit = Lens.lens (\ThrottleSettings' {burstLimit} -> burstLimit) (\s@ThrottleSettings' {} a -> s {burstLimit = a} :: ThrottleSettings)

-- | The API request steady-state rate limit.
throttleSettings_rateLimit :: Lens.Lens' ThrottleSettings (Core.Maybe Core.Double)
throttleSettings_rateLimit = Lens.lens (\ThrottleSettings' {rateLimit} -> rateLimit) (\s@ThrottleSettings' {} a -> s {rateLimit = a} :: ThrottleSettings)

instance Core.FromJSON ThrottleSettings where
  parseJSON =
    Core.withObject
      "ThrottleSettings"
      ( \x ->
          ThrottleSettings'
            Core.<$> (x Core..:? "burstLimit")
            Core.<*> (x Core..:? "rateLimit")
      )

instance Core.Hashable ThrottleSettings

instance Core.NFData ThrottleSettings

instance Core.ToJSON ThrottleSettings where
  toJSON ThrottleSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("burstLimit" Core..=) Core.<$> burstLimit,
            ("rateLimit" Core..=) Core.<$> rateLimit
          ]
      )
