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
-- Module      : Amazonka.EC2.Types.InstanceMonitoring
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceMonitoring where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.Monitoring
import qualified Amazonka.Prelude as Prelude

-- | Describes the monitoring of an instance.
--
-- /See:/ 'newInstanceMonitoring' smart constructor.
data InstanceMonitoring = InstanceMonitoring'
  { -- | The monitoring for the instance.
    monitoring :: Prelude.Maybe Monitoring,
    -- | The ID of the instance.
    instanceId :: Prelude.Maybe Prelude.Text
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
-- 'monitoring', 'instanceMonitoring_monitoring' - The monitoring for the instance.
--
-- 'instanceId', 'instanceMonitoring_instanceId' - The ID of the instance.
newInstanceMonitoring ::
  InstanceMonitoring
newInstanceMonitoring =
  InstanceMonitoring'
    { monitoring = Prelude.Nothing,
      instanceId = Prelude.Nothing
    }

-- | The monitoring for the instance.
instanceMonitoring_monitoring :: Lens.Lens' InstanceMonitoring (Prelude.Maybe Monitoring)
instanceMonitoring_monitoring = Lens.lens (\InstanceMonitoring' {monitoring} -> monitoring) (\s@InstanceMonitoring' {} a -> s {monitoring = a} :: InstanceMonitoring)

-- | The ID of the instance.
instanceMonitoring_instanceId :: Lens.Lens' InstanceMonitoring (Prelude.Maybe Prelude.Text)
instanceMonitoring_instanceId = Lens.lens (\InstanceMonitoring' {instanceId} -> instanceId) (\s@InstanceMonitoring' {} a -> s {instanceId = a} :: InstanceMonitoring)

instance Data.FromXML InstanceMonitoring where
  parseXML x =
    InstanceMonitoring'
      Prelude.<$> (x Data..@? "monitoring")
      Prelude.<*> (x Data..@? "instanceId")

instance Prelude.Hashable InstanceMonitoring where
  hashWithSalt _salt InstanceMonitoring' {..} =
    _salt `Prelude.hashWithSalt` monitoring
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData InstanceMonitoring where
  rnf InstanceMonitoring' {..} =
    Prelude.rnf monitoring
      `Prelude.seq` Prelude.rnf instanceId
