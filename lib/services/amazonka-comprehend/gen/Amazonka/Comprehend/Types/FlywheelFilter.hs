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
-- Module      : Amazonka.Comprehend.Types.FlywheelFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.FlywheelFilter where

import Amazonka.Comprehend.Types.FlywheelStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Filter the flywheels based on creation time or flywheel status.
--
-- /See:/ 'newFlywheelFilter' smart constructor.
data FlywheelFilter = FlywheelFilter'
  { -- | Filter the flywheels to include flywheels created after the specified
    -- time.
    creationTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | Filter the flywheels to include flywheels created before the specified
    -- time.
    creationTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | Filter the flywheels based on the flywheel status.
    status :: Prelude.Maybe FlywheelStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FlywheelFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimeAfter', 'flywheelFilter_creationTimeAfter' - Filter the flywheels to include flywheels created after the specified
-- time.
--
-- 'creationTimeBefore', 'flywheelFilter_creationTimeBefore' - Filter the flywheels to include flywheels created before the specified
-- time.
--
-- 'status', 'flywheelFilter_status' - Filter the flywheels based on the flywheel status.
newFlywheelFilter ::
  FlywheelFilter
newFlywheelFilter =
  FlywheelFilter'
    { creationTimeAfter =
        Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Filter the flywheels to include flywheels created after the specified
-- time.
flywheelFilter_creationTimeAfter :: Lens.Lens' FlywheelFilter (Prelude.Maybe Prelude.UTCTime)
flywheelFilter_creationTimeAfter = Lens.lens (\FlywheelFilter' {creationTimeAfter} -> creationTimeAfter) (\s@FlywheelFilter' {} a -> s {creationTimeAfter = a} :: FlywheelFilter) Prelude.. Lens.mapping Data._Time

-- | Filter the flywheels to include flywheels created before the specified
-- time.
flywheelFilter_creationTimeBefore :: Lens.Lens' FlywheelFilter (Prelude.Maybe Prelude.UTCTime)
flywheelFilter_creationTimeBefore = Lens.lens (\FlywheelFilter' {creationTimeBefore} -> creationTimeBefore) (\s@FlywheelFilter' {} a -> s {creationTimeBefore = a} :: FlywheelFilter) Prelude.. Lens.mapping Data._Time

-- | Filter the flywheels based on the flywheel status.
flywheelFilter_status :: Lens.Lens' FlywheelFilter (Prelude.Maybe FlywheelStatus)
flywheelFilter_status = Lens.lens (\FlywheelFilter' {status} -> status) (\s@FlywheelFilter' {} a -> s {status = a} :: FlywheelFilter)

instance Prelude.Hashable FlywheelFilter where
  hashWithSalt _salt FlywheelFilter' {..} =
    _salt
      `Prelude.hashWithSalt` creationTimeAfter
      `Prelude.hashWithSalt` creationTimeBefore
      `Prelude.hashWithSalt` status

instance Prelude.NFData FlywheelFilter where
  rnf FlywheelFilter' {..} =
    Prelude.rnf creationTimeAfter
      `Prelude.seq` Prelude.rnf creationTimeBefore
      `Prelude.seq` Prelude.rnf status

instance Data.ToJSON FlywheelFilter where
  toJSON FlywheelFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CreationTimeAfter" Data..=)
              Prelude.<$> creationTimeAfter,
            ("CreationTimeBefore" Data..=)
              Prelude.<$> creationTimeBefore,
            ("Status" Data..=) Prelude.<$> status
          ]
      )
