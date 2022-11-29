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
-- Module      : Amazonka.S3.Types.RestoreRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.RestoreRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.GlacierJobParameters
import Amazonka.S3.Types.OutputLocation
import Amazonka.S3.Types.RestoreRequestType
import Amazonka.S3.Types.SelectParameters
import Amazonka.S3.Types.Tier

-- | Container for restore job parameters.
--
-- /See:/ 'newRestoreRequest' smart constructor.
data RestoreRequest = RestoreRequest'
  { -- | S3 Glacier related parameters pertaining to this job. Do not use with
    -- restores that specify @OutputLocation@.
    glacierJobParameters :: Prelude.Maybe GlacierJobParameters,
    -- | Type of restore request.
    type' :: Prelude.Maybe RestoreRequestType,
    -- | Lifetime of the active copy in days. Do not use with restores that
    -- specify @OutputLocation@.
    --
    -- The Days element is required for regular restores, and must not be
    -- provided for select requests.
    days :: Prelude.Maybe Prelude.Int,
    -- | Describes the location where the restore job\'s output is stored.
    outputLocation :: Prelude.Maybe OutputLocation,
    -- | Describes the parameters for Select job types.
    selectParameters :: Prelude.Maybe SelectParameters,
    -- | The optional description for the job.
    description :: Prelude.Maybe Prelude.Text,
    -- | Retrieval tier at which the restore will be processed.
    tier :: Prelude.Maybe Tier
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'glacierJobParameters', 'restoreRequest_glacierJobParameters' - S3 Glacier related parameters pertaining to this job. Do not use with
-- restores that specify @OutputLocation@.
--
-- 'type'', 'restoreRequest_type' - Type of restore request.
--
-- 'days', 'restoreRequest_days' - Lifetime of the active copy in days. Do not use with restores that
-- specify @OutputLocation@.
--
-- The Days element is required for regular restores, and must not be
-- provided for select requests.
--
-- 'outputLocation', 'restoreRequest_outputLocation' - Describes the location where the restore job\'s output is stored.
--
-- 'selectParameters', 'restoreRequest_selectParameters' - Describes the parameters for Select job types.
--
-- 'description', 'restoreRequest_description' - The optional description for the job.
--
-- 'tier', 'restoreRequest_tier' - Retrieval tier at which the restore will be processed.
newRestoreRequest ::
  RestoreRequest
newRestoreRequest =
  RestoreRequest'
    { glacierJobParameters =
        Prelude.Nothing,
      type' = Prelude.Nothing,
      days = Prelude.Nothing,
      outputLocation = Prelude.Nothing,
      selectParameters = Prelude.Nothing,
      description = Prelude.Nothing,
      tier = Prelude.Nothing
    }

-- | S3 Glacier related parameters pertaining to this job. Do not use with
-- restores that specify @OutputLocation@.
restoreRequest_glacierJobParameters :: Lens.Lens' RestoreRequest (Prelude.Maybe GlacierJobParameters)
restoreRequest_glacierJobParameters = Lens.lens (\RestoreRequest' {glacierJobParameters} -> glacierJobParameters) (\s@RestoreRequest' {} a -> s {glacierJobParameters = a} :: RestoreRequest)

-- | Type of restore request.
restoreRequest_type :: Lens.Lens' RestoreRequest (Prelude.Maybe RestoreRequestType)
restoreRequest_type = Lens.lens (\RestoreRequest' {type'} -> type') (\s@RestoreRequest' {} a -> s {type' = a} :: RestoreRequest)

-- | Lifetime of the active copy in days. Do not use with restores that
-- specify @OutputLocation@.
--
-- The Days element is required for regular restores, and must not be
-- provided for select requests.
restoreRequest_days :: Lens.Lens' RestoreRequest (Prelude.Maybe Prelude.Int)
restoreRequest_days = Lens.lens (\RestoreRequest' {days} -> days) (\s@RestoreRequest' {} a -> s {days = a} :: RestoreRequest)

-- | Describes the location where the restore job\'s output is stored.
restoreRequest_outputLocation :: Lens.Lens' RestoreRequest (Prelude.Maybe OutputLocation)
restoreRequest_outputLocation = Lens.lens (\RestoreRequest' {outputLocation} -> outputLocation) (\s@RestoreRequest' {} a -> s {outputLocation = a} :: RestoreRequest)

-- | Describes the parameters for Select job types.
restoreRequest_selectParameters :: Lens.Lens' RestoreRequest (Prelude.Maybe SelectParameters)
restoreRequest_selectParameters = Lens.lens (\RestoreRequest' {selectParameters} -> selectParameters) (\s@RestoreRequest' {} a -> s {selectParameters = a} :: RestoreRequest)

-- | The optional description for the job.
restoreRequest_description :: Lens.Lens' RestoreRequest (Prelude.Maybe Prelude.Text)
restoreRequest_description = Lens.lens (\RestoreRequest' {description} -> description) (\s@RestoreRequest' {} a -> s {description = a} :: RestoreRequest)

-- | Retrieval tier at which the restore will be processed.
restoreRequest_tier :: Lens.Lens' RestoreRequest (Prelude.Maybe Tier)
restoreRequest_tier = Lens.lens (\RestoreRequest' {tier} -> tier) (\s@RestoreRequest' {} a -> s {tier = a} :: RestoreRequest)

instance Prelude.Hashable RestoreRequest where
  hashWithSalt _salt RestoreRequest' {..} =
    _salt `Prelude.hashWithSalt` glacierJobParameters
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` days
      `Prelude.hashWithSalt` outputLocation
      `Prelude.hashWithSalt` selectParameters
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tier

instance Prelude.NFData RestoreRequest where
  rnf RestoreRequest' {..} =
    Prelude.rnf glacierJobParameters
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf days
      `Prelude.seq` Prelude.rnf outputLocation
      `Prelude.seq` Prelude.rnf selectParameters
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tier

instance Core.ToXML RestoreRequest where
  toXML RestoreRequest' {..} =
    Prelude.mconcat
      [ "GlacierJobParameters" Core.@= glacierJobParameters,
        "Type" Core.@= type',
        "Days" Core.@= days,
        "OutputLocation" Core.@= outputLocation,
        "SelectParameters" Core.@= selectParameters,
        "Description" Core.@= description,
        "Tier" Core.@= tier
      ]
