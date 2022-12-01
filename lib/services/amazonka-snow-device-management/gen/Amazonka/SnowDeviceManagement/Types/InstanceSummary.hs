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
-- Module      : Amazonka.SnowDeviceManagement.Types.InstanceSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SnowDeviceManagement.Types.InstanceSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SnowDeviceManagement.Types.Instance

-- | The details about the instance.
--
-- /See:/ 'newInstanceSummary' smart constructor.
data InstanceSummary = InstanceSummary'
  { -- | When the instance summary was last updated.
    lastUpdatedAt :: Prelude.Maybe Core.POSIX,
    -- | A structure containing details about the instance.
    instance' :: Prelude.Maybe Instance
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdatedAt', 'instanceSummary_lastUpdatedAt' - When the instance summary was last updated.
--
-- 'instance'', 'instanceSummary_instance' - A structure containing details about the instance.
newInstanceSummary ::
  InstanceSummary
newInstanceSummary =
  InstanceSummary'
    { lastUpdatedAt = Prelude.Nothing,
      instance' = Prelude.Nothing
    }

-- | When the instance summary was last updated.
instanceSummary_lastUpdatedAt :: Lens.Lens' InstanceSummary (Prelude.Maybe Prelude.UTCTime)
instanceSummary_lastUpdatedAt = Lens.lens (\InstanceSummary' {lastUpdatedAt} -> lastUpdatedAt) (\s@InstanceSummary' {} a -> s {lastUpdatedAt = a} :: InstanceSummary) Prelude.. Lens.mapping Core._Time

-- | A structure containing details about the instance.
instanceSummary_instance :: Lens.Lens' InstanceSummary (Prelude.Maybe Instance)
instanceSummary_instance = Lens.lens (\InstanceSummary' {instance'} -> instance') (\s@InstanceSummary' {} a -> s {instance' = a} :: InstanceSummary)

instance Core.FromJSON InstanceSummary where
  parseJSON =
    Core.withObject
      "InstanceSummary"
      ( \x ->
          InstanceSummary'
            Prelude.<$> (x Core..:? "lastUpdatedAt")
            Prelude.<*> (x Core..:? "instance")
      )

instance Prelude.Hashable InstanceSummary where
  hashWithSalt _salt InstanceSummary' {..} =
    _salt `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` instance'

instance Prelude.NFData InstanceSummary where
  rnf InstanceSummary' {..} =
    Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf instance'
