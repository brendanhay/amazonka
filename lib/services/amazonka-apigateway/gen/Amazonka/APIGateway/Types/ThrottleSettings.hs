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
-- Module      : Amazonka.APIGateway.Types.ThrottleSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.ThrottleSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The API request rate limits.
--
-- /See:/ 'newThrottleSettings' smart constructor.
data ThrottleSettings = ThrottleSettings'
  { -- | The API target request rate limit.
    rateLimit :: Prelude.Maybe Prelude.Double,
    -- | The API target request burst rate limit. This allows more requests
    -- through for a period of time than the target rate limit.
    burstLimit :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ThrottleSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rateLimit', 'throttleSettings_rateLimit' - The API target request rate limit.
--
-- 'burstLimit', 'throttleSettings_burstLimit' - The API target request burst rate limit. This allows more requests
-- through for a period of time than the target rate limit.
newThrottleSettings ::
  ThrottleSettings
newThrottleSettings =
  ThrottleSettings'
    { rateLimit = Prelude.Nothing,
      burstLimit = Prelude.Nothing
    }

-- | The API target request rate limit.
throttleSettings_rateLimit :: Lens.Lens' ThrottleSettings (Prelude.Maybe Prelude.Double)
throttleSettings_rateLimit = Lens.lens (\ThrottleSettings' {rateLimit} -> rateLimit) (\s@ThrottleSettings' {} a -> s {rateLimit = a} :: ThrottleSettings)

-- | The API target request burst rate limit. This allows more requests
-- through for a period of time than the target rate limit.
throttleSettings_burstLimit :: Lens.Lens' ThrottleSettings (Prelude.Maybe Prelude.Int)
throttleSettings_burstLimit = Lens.lens (\ThrottleSettings' {burstLimit} -> burstLimit) (\s@ThrottleSettings' {} a -> s {burstLimit = a} :: ThrottleSettings)

instance Data.FromJSON ThrottleSettings where
  parseJSON =
    Data.withObject
      "ThrottleSettings"
      ( \x ->
          ThrottleSettings'
            Prelude.<$> (x Data..:? "rateLimit")
            Prelude.<*> (x Data..:? "burstLimit")
      )

instance Prelude.Hashable ThrottleSettings where
  hashWithSalt _salt ThrottleSettings' {..} =
    _salt `Prelude.hashWithSalt` rateLimit
      `Prelude.hashWithSalt` burstLimit

instance Prelude.NFData ThrottleSettings where
  rnf ThrottleSettings' {..} =
    Prelude.rnf rateLimit
      `Prelude.seq` Prelude.rnf burstLimit

instance Data.ToJSON ThrottleSettings where
  toJSON ThrottleSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("rateLimit" Data..=) Prelude.<$> rateLimit,
            ("burstLimit" Data..=) Prelude.<$> burstLimit
          ]
      )
