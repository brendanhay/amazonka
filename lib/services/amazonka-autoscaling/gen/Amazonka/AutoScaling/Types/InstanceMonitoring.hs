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
-- Module      : Amazonka.AutoScaling.Types.InstanceMonitoring
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.InstanceMonitoring where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes whether detailed monitoring is enabled for the Auto Scaling
-- instances.
--
-- /See:/ 'newInstanceMonitoring' smart constructor.
data InstanceMonitoring = InstanceMonitoring'
  { -- | If @true@, detailed monitoring is enabled. Otherwise, basic monitoring
    -- is enabled.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceMonitoring' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'instanceMonitoring_enabled' - If @true@, detailed monitoring is enabled. Otherwise, basic monitoring
-- is enabled.
newInstanceMonitoring ::
  InstanceMonitoring
newInstanceMonitoring =
  InstanceMonitoring' {enabled = Prelude.Nothing}

-- | If @true@, detailed monitoring is enabled. Otherwise, basic monitoring
-- is enabled.
instanceMonitoring_enabled :: Lens.Lens' InstanceMonitoring (Prelude.Maybe Prelude.Bool)
instanceMonitoring_enabled = Lens.lens (\InstanceMonitoring' {enabled} -> enabled) (\s@InstanceMonitoring' {} a -> s {enabled = a} :: InstanceMonitoring)

instance Data.FromXML InstanceMonitoring where
  parseXML x =
    InstanceMonitoring'
      Prelude.<$> (x Data..@? "Enabled")

instance Prelude.Hashable InstanceMonitoring where
  hashWithSalt _salt InstanceMonitoring' {..} =
    _salt `Prelude.hashWithSalt` enabled

instance Prelude.NFData InstanceMonitoring where
  rnf InstanceMonitoring' {..} = Prelude.rnf enabled

instance Data.ToQuery InstanceMonitoring where
  toQuery InstanceMonitoring' {..} =
    Prelude.mconcat ["Enabled" Data.=: enabled]
