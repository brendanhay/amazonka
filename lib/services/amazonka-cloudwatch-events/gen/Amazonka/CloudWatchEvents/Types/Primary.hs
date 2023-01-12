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
-- Module      : Amazonka.CloudWatchEvents.Types.Primary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.Primary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The primary Region of the endpoint.
--
-- /See:/ 'newPrimary' smart constructor.
data Primary = Primary'
  { -- | The ARN of the health check used by the endpoint to determine whether
    -- failover is triggered.
    healthCheck :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Primary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'healthCheck', 'primary_healthCheck' - The ARN of the health check used by the endpoint to determine whether
-- failover is triggered.
newPrimary ::
  -- | 'healthCheck'
  Prelude.Text ->
  Primary
newPrimary pHealthCheck_ =
  Primary' {healthCheck = pHealthCheck_}

-- | The ARN of the health check used by the endpoint to determine whether
-- failover is triggered.
primary_healthCheck :: Lens.Lens' Primary Prelude.Text
primary_healthCheck = Lens.lens (\Primary' {healthCheck} -> healthCheck) (\s@Primary' {} a -> s {healthCheck = a} :: Primary)

instance Data.FromJSON Primary where
  parseJSON =
    Data.withObject
      "Primary"
      ( \x ->
          Primary' Prelude.<$> (x Data..: "HealthCheck")
      )

instance Prelude.Hashable Primary where
  hashWithSalt _salt Primary' {..} =
    _salt `Prelude.hashWithSalt` healthCheck

instance Prelude.NFData Primary where
  rnf Primary' {..} = Prelude.rnf healthCheck

instance Data.ToJSON Primary where
  toJSON Primary' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("HealthCheck" Data..= healthCheck)]
      )
