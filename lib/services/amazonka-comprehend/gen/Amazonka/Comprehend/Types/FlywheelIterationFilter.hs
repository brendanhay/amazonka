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
-- Module      : Amazonka.Comprehend.Types.FlywheelIterationFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.FlywheelIterationFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Filter the flywheel iterations based on creation time.
--
-- /See:/ 'newFlywheelIterationFilter' smart constructor.
data FlywheelIterationFilter = FlywheelIterationFilter'
  { -- | Filter the flywheel iterations to include iterations created after the
    -- specified time.
    creationTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | Filter the flywheel iterations to include iterations created before the
    -- specified time.
    creationTimeBefore :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FlywheelIterationFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimeAfter', 'flywheelIterationFilter_creationTimeAfter' - Filter the flywheel iterations to include iterations created after the
-- specified time.
--
-- 'creationTimeBefore', 'flywheelIterationFilter_creationTimeBefore' - Filter the flywheel iterations to include iterations created before the
-- specified time.
newFlywheelIterationFilter ::
  FlywheelIterationFilter
newFlywheelIterationFilter =
  FlywheelIterationFilter'
    { creationTimeAfter =
        Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing
    }

-- | Filter the flywheel iterations to include iterations created after the
-- specified time.
flywheelIterationFilter_creationTimeAfter :: Lens.Lens' FlywheelIterationFilter (Prelude.Maybe Prelude.UTCTime)
flywheelIterationFilter_creationTimeAfter = Lens.lens (\FlywheelIterationFilter' {creationTimeAfter} -> creationTimeAfter) (\s@FlywheelIterationFilter' {} a -> s {creationTimeAfter = a} :: FlywheelIterationFilter) Prelude.. Lens.mapping Data._Time

-- | Filter the flywheel iterations to include iterations created before the
-- specified time.
flywheelIterationFilter_creationTimeBefore :: Lens.Lens' FlywheelIterationFilter (Prelude.Maybe Prelude.UTCTime)
flywheelIterationFilter_creationTimeBefore = Lens.lens (\FlywheelIterationFilter' {creationTimeBefore} -> creationTimeBefore) (\s@FlywheelIterationFilter' {} a -> s {creationTimeBefore = a} :: FlywheelIterationFilter) Prelude.. Lens.mapping Data._Time

instance Prelude.Hashable FlywheelIterationFilter where
  hashWithSalt _salt FlywheelIterationFilter' {..} =
    _salt
      `Prelude.hashWithSalt` creationTimeAfter
      `Prelude.hashWithSalt` creationTimeBefore

instance Prelude.NFData FlywheelIterationFilter where
  rnf FlywheelIterationFilter' {..} =
    Prelude.rnf creationTimeAfter
      `Prelude.seq` Prelude.rnf creationTimeBefore

instance Data.ToJSON FlywheelIterationFilter where
  toJSON FlywheelIterationFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CreationTimeAfter" Data..=)
              Prelude.<$> creationTimeAfter,
            ("CreationTimeBefore" Data..=)
              Prelude.<$> creationTimeBefore
          ]
      )
