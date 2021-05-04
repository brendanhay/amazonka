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
-- Module      : Network.AWS.APIGateway.Types.ThrottleSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.ThrottleSettings where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The API request rate limits.
--
-- /See:/ 'newThrottleSettings' smart constructor.
data ThrottleSettings = ThrottleSettings'
  { -- | The API request burst limit, the maximum rate limit over a time ranging
    -- from one to a few seconds, depending upon whether the underlying token
    -- bucket is at its full capacity.
    burstLimit :: Prelude.Maybe Prelude.Int,
    -- | The API request steady-state rate limit.
    rateLimit :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { burstLimit = Prelude.Nothing,
      rateLimit = Prelude.Nothing
    }

-- | The API request burst limit, the maximum rate limit over a time ranging
-- from one to a few seconds, depending upon whether the underlying token
-- bucket is at its full capacity.
throttleSettings_burstLimit :: Lens.Lens' ThrottleSettings (Prelude.Maybe Prelude.Int)
throttleSettings_burstLimit = Lens.lens (\ThrottleSettings' {burstLimit} -> burstLimit) (\s@ThrottleSettings' {} a -> s {burstLimit = a} :: ThrottleSettings)

-- | The API request steady-state rate limit.
throttleSettings_rateLimit :: Lens.Lens' ThrottleSettings (Prelude.Maybe Prelude.Double)
throttleSettings_rateLimit = Lens.lens (\ThrottleSettings' {rateLimit} -> rateLimit) (\s@ThrottleSettings' {} a -> s {rateLimit = a} :: ThrottleSettings)

instance Prelude.FromJSON ThrottleSettings where
  parseJSON =
    Prelude.withObject
      "ThrottleSettings"
      ( \x ->
          ThrottleSettings'
            Prelude.<$> (x Prelude..:? "burstLimit")
            Prelude.<*> (x Prelude..:? "rateLimit")
      )

instance Prelude.Hashable ThrottleSettings

instance Prelude.NFData ThrottleSettings

instance Prelude.ToJSON ThrottleSettings where
  toJSON ThrottleSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("burstLimit" Prelude..=) Prelude.<$> burstLimit,
            ("rateLimit" Prelude..=) Prelude.<$> rateLimit
          ]
      )
