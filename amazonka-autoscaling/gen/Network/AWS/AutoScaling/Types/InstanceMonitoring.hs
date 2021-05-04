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
-- Module      : Network.AWS.AutoScaling.Types.InstanceMonitoring
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.InstanceMonitoring where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes whether detailed monitoring is enabled for the Auto Scaling
-- instances.
--
-- /See:/ 'newInstanceMonitoring' smart constructor.
data InstanceMonitoring = InstanceMonitoring'
  { -- | If @true@, detailed monitoring is enabled. Otherwise, basic monitoring
    -- is enabled.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromXML InstanceMonitoring where
  parseXML x =
    InstanceMonitoring'
      Prelude.<$> (x Prelude..@? "Enabled")

instance Prelude.Hashable InstanceMonitoring

instance Prelude.NFData InstanceMonitoring

instance Prelude.ToQuery InstanceMonitoring where
  toQuery InstanceMonitoring' {..} =
    Prelude.mconcat ["Enabled" Prelude.=: enabled]
