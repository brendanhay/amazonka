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
-- Module      : Amazonka.EC2.Types.SpotFleetMonitoring
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SpotFleetMonitoring where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes whether monitoring is enabled.
--
-- /See:/ 'newSpotFleetMonitoring' smart constructor.
data SpotFleetMonitoring = SpotFleetMonitoring'
  { -- | Enables monitoring for the instance.
    --
    -- Default: @false@
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SpotFleetMonitoring' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'spotFleetMonitoring_enabled' - Enables monitoring for the instance.
--
-- Default: @false@
newSpotFleetMonitoring ::
  SpotFleetMonitoring
newSpotFleetMonitoring =
  SpotFleetMonitoring' {enabled = Prelude.Nothing}

-- | Enables monitoring for the instance.
--
-- Default: @false@
spotFleetMonitoring_enabled :: Lens.Lens' SpotFleetMonitoring (Prelude.Maybe Prelude.Bool)
spotFleetMonitoring_enabled = Lens.lens (\SpotFleetMonitoring' {enabled} -> enabled) (\s@SpotFleetMonitoring' {} a -> s {enabled = a} :: SpotFleetMonitoring)

instance Data.FromXML SpotFleetMonitoring where
  parseXML x =
    SpotFleetMonitoring'
      Prelude.<$> (x Data..@? "enabled")

instance Prelude.Hashable SpotFleetMonitoring where
  hashWithSalt _salt SpotFleetMonitoring' {..} =
    _salt `Prelude.hashWithSalt` enabled

instance Prelude.NFData SpotFleetMonitoring where
  rnf SpotFleetMonitoring' {..} = Prelude.rnf enabled

instance Data.ToQuery SpotFleetMonitoring where
  toQuery SpotFleetMonitoring' {..} =
    Prelude.mconcat ["Enabled" Data.=: enabled]
