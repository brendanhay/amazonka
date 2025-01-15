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
-- Module      : Amazonka.Omics.Types.ActivateReadSetFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.ActivateReadSetFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.ReadSetActivationJobStatus
import qualified Amazonka.Prelude as Prelude

-- | A read set activation job filter.
--
-- /See:/ 'newActivateReadSetFilter' smart constructor.
data ActivateReadSetFilter = ActivateReadSetFilter'
  { -- | The filter\'s start date.
    createdAfter :: Prelude.Maybe Data.ISO8601,
    -- | The filter\'s end date.
    createdBefore :: Prelude.Maybe Data.ISO8601,
    -- | The filter\'s status.
    status :: Prelude.Maybe ReadSetActivationJobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActivateReadSetFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAfter', 'activateReadSetFilter_createdAfter' - The filter\'s start date.
--
-- 'createdBefore', 'activateReadSetFilter_createdBefore' - The filter\'s end date.
--
-- 'status', 'activateReadSetFilter_status' - The filter\'s status.
newActivateReadSetFilter ::
  ActivateReadSetFilter
newActivateReadSetFilter =
  ActivateReadSetFilter'
    { createdAfter =
        Prelude.Nothing,
      createdBefore = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The filter\'s start date.
activateReadSetFilter_createdAfter :: Lens.Lens' ActivateReadSetFilter (Prelude.Maybe Prelude.UTCTime)
activateReadSetFilter_createdAfter = Lens.lens (\ActivateReadSetFilter' {createdAfter} -> createdAfter) (\s@ActivateReadSetFilter' {} a -> s {createdAfter = a} :: ActivateReadSetFilter) Prelude.. Lens.mapping Data._Time

-- | The filter\'s end date.
activateReadSetFilter_createdBefore :: Lens.Lens' ActivateReadSetFilter (Prelude.Maybe Prelude.UTCTime)
activateReadSetFilter_createdBefore = Lens.lens (\ActivateReadSetFilter' {createdBefore} -> createdBefore) (\s@ActivateReadSetFilter' {} a -> s {createdBefore = a} :: ActivateReadSetFilter) Prelude.. Lens.mapping Data._Time

-- | The filter\'s status.
activateReadSetFilter_status :: Lens.Lens' ActivateReadSetFilter (Prelude.Maybe ReadSetActivationJobStatus)
activateReadSetFilter_status = Lens.lens (\ActivateReadSetFilter' {status} -> status) (\s@ActivateReadSetFilter' {} a -> s {status = a} :: ActivateReadSetFilter)

instance Prelude.Hashable ActivateReadSetFilter where
  hashWithSalt _salt ActivateReadSetFilter' {..} =
    _salt
      `Prelude.hashWithSalt` createdAfter
      `Prelude.hashWithSalt` createdBefore
      `Prelude.hashWithSalt` status

instance Prelude.NFData ActivateReadSetFilter where
  rnf ActivateReadSetFilter' {..} =
    Prelude.rnf createdAfter `Prelude.seq`
      Prelude.rnf createdBefore `Prelude.seq`
        Prelude.rnf status

instance Data.ToJSON ActivateReadSetFilter where
  toJSON ActivateReadSetFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("createdAfter" Data..=) Prelude.<$> createdAfter,
            ("createdBefore" Data..=) Prelude.<$> createdBefore,
            ("status" Data..=) Prelude.<$> status
          ]
      )
