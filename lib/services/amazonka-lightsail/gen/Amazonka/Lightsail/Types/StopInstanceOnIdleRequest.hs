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
-- Module      : Amazonka.Lightsail.Types.StopInstanceOnIdleRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.StopInstanceOnIdleRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a request to create or edit the @StopInstanceOnIdle@ add-on.
--
-- This add-on only applies to Lightsail for Research resources.
--
-- /See:/ 'newStopInstanceOnIdleRequest' smart constructor.
data StopInstanceOnIdleRequest = StopInstanceOnIdleRequest'
  { -- | The amount of idle time in minutes after which your virtual computer
    -- will automatically stop.
    duration :: Prelude.Maybe Prelude.Text,
    -- | The value to compare with the duration.
    threshold :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopInstanceOnIdleRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'duration', 'stopInstanceOnIdleRequest_duration' - The amount of idle time in minutes after which your virtual computer
-- will automatically stop.
--
-- 'threshold', 'stopInstanceOnIdleRequest_threshold' - The value to compare with the duration.
newStopInstanceOnIdleRequest ::
  StopInstanceOnIdleRequest
newStopInstanceOnIdleRequest =
  StopInstanceOnIdleRequest'
    { duration =
        Prelude.Nothing,
      threshold = Prelude.Nothing
    }

-- | The amount of idle time in minutes after which your virtual computer
-- will automatically stop.
stopInstanceOnIdleRequest_duration :: Lens.Lens' StopInstanceOnIdleRequest (Prelude.Maybe Prelude.Text)
stopInstanceOnIdleRequest_duration = Lens.lens (\StopInstanceOnIdleRequest' {duration} -> duration) (\s@StopInstanceOnIdleRequest' {} a -> s {duration = a} :: StopInstanceOnIdleRequest)

-- | The value to compare with the duration.
stopInstanceOnIdleRequest_threshold :: Lens.Lens' StopInstanceOnIdleRequest (Prelude.Maybe Prelude.Text)
stopInstanceOnIdleRequest_threshold = Lens.lens (\StopInstanceOnIdleRequest' {threshold} -> threshold) (\s@StopInstanceOnIdleRequest' {} a -> s {threshold = a} :: StopInstanceOnIdleRequest)

instance Prelude.Hashable StopInstanceOnIdleRequest where
  hashWithSalt _salt StopInstanceOnIdleRequest' {..} =
    _salt
      `Prelude.hashWithSalt` duration
      `Prelude.hashWithSalt` threshold

instance Prelude.NFData StopInstanceOnIdleRequest where
  rnf StopInstanceOnIdleRequest' {..} =
    Prelude.rnf duration
      `Prelude.seq` Prelude.rnf threshold

instance Data.ToJSON StopInstanceOnIdleRequest where
  toJSON StopInstanceOnIdleRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("duration" Data..=) Prelude.<$> duration,
            ("threshold" Data..=) Prelude.<$> threshold
          ]
      )
