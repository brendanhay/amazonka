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
-- Module      : Network.AWS.S3.Types.RestoreRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.RestoreRequest where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.GlacierJobParameters
import Network.AWS.S3.Types.OutputLocation
import Network.AWS.S3.Types.RestoreRequestType
import Network.AWS.S3.Types.SelectParameters
import Network.AWS.S3.Types.Tier

-- | Container for restore job parameters.
--
-- /See:/ 'newRestoreRequest' smart constructor.
data RestoreRequest = RestoreRequest'
  { -- | Lifetime of the active copy in days. Do not use with restores that
    -- specify @OutputLocation@.
    --
    -- The Days element is required for regular restores, and must not be
    -- provided for select requests.
    days :: Core.Maybe Core.Int,
    -- | Describes the parameters for Select job types.
    selectParameters :: Core.Maybe SelectParameters,
    -- | The optional description for the job.
    description :: Core.Maybe Core.Text,
    -- | Type of restore request.
    type' :: Core.Maybe RestoreRequestType,
    -- | Describes the location where the restore job\'s output is stored.
    outputLocation :: Core.Maybe OutputLocation,
    -- | Retrieval tier at which the restore will be processed.
    tier :: Core.Maybe Tier,
    -- | S3 Glacier related parameters pertaining to this job. Do not use with
    -- restores that specify @OutputLocation@.
    glacierJobParameters :: Core.Maybe GlacierJobParameters
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'RestoreRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'days', 'restoreRequest_days' - Lifetime of the active copy in days. Do not use with restores that
-- specify @OutputLocation@.
--
-- The Days element is required for regular restores, and must not be
-- provided for select requests.
--
-- 'selectParameters', 'restoreRequest_selectParameters' - Describes the parameters for Select job types.
--
-- 'description', 'restoreRequest_description' - The optional description for the job.
--
-- 'type'', 'restoreRequest_type' - Type of restore request.
--
-- 'outputLocation', 'restoreRequest_outputLocation' - Describes the location where the restore job\'s output is stored.
--
-- 'tier', 'restoreRequest_tier' - Retrieval tier at which the restore will be processed.
--
-- 'glacierJobParameters', 'restoreRequest_glacierJobParameters' - S3 Glacier related parameters pertaining to this job. Do not use with
-- restores that specify @OutputLocation@.
newRestoreRequest ::
  RestoreRequest
newRestoreRequest =
  RestoreRequest'
    { days = Core.Nothing,
      selectParameters = Core.Nothing,
      description = Core.Nothing,
      type' = Core.Nothing,
      outputLocation = Core.Nothing,
      tier = Core.Nothing,
      glacierJobParameters = Core.Nothing
    }

-- | Lifetime of the active copy in days. Do not use with restores that
-- specify @OutputLocation@.
--
-- The Days element is required for regular restores, and must not be
-- provided for select requests.
restoreRequest_days :: Lens.Lens' RestoreRequest (Core.Maybe Core.Int)
restoreRequest_days = Lens.lens (\RestoreRequest' {days} -> days) (\s@RestoreRequest' {} a -> s {days = a} :: RestoreRequest)

-- | Describes the parameters for Select job types.
restoreRequest_selectParameters :: Lens.Lens' RestoreRequest (Core.Maybe SelectParameters)
restoreRequest_selectParameters = Lens.lens (\RestoreRequest' {selectParameters} -> selectParameters) (\s@RestoreRequest' {} a -> s {selectParameters = a} :: RestoreRequest)

-- | The optional description for the job.
restoreRequest_description :: Lens.Lens' RestoreRequest (Core.Maybe Core.Text)
restoreRequest_description = Lens.lens (\RestoreRequest' {description} -> description) (\s@RestoreRequest' {} a -> s {description = a} :: RestoreRequest)

-- | Type of restore request.
restoreRequest_type :: Lens.Lens' RestoreRequest (Core.Maybe RestoreRequestType)
restoreRequest_type = Lens.lens (\RestoreRequest' {type'} -> type') (\s@RestoreRequest' {} a -> s {type' = a} :: RestoreRequest)

-- | Describes the location where the restore job\'s output is stored.
restoreRequest_outputLocation :: Lens.Lens' RestoreRequest (Core.Maybe OutputLocation)
restoreRequest_outputLocation = Lens.lens (\RestoreRequest' {outputLocation} -> outputLocation) (\s@RestoreRequest' {} a -> s {outputLocation = a} :: RestoreRequest)

-- | Retrieval tier at which the restore will be processed.
restoreRequest_tier :: Lens.Lens' RestoreRequest (Core.Maybe Tier)
restoreRequest_tier = Lens.lens (\RestoreRequest' {tier} -> tier) (\s@RestoreRequest' {} a -> s {tier = a} :: RestoreRequest)

-- | S3 Glacier related parameters pertaining to this job. Do not use with
-- restores that specify @OutputLocation@.
restoreRequest_glacierJobParameters :: Lens.Lens' RestoreRequest (Core.Maybe GlacierJobParameters)
restoreRequest_glacierJobParameters = Lens.lens (\RestoreRequest' {glacierJobParameters} -> glacierJobParameters) (\s@RestoreRequest' {} a -> s {glacierJobParameters = a} :: RestoreRequest)

instance Core.Hashable RestoreRequest

instance Core.NFData RestoreRequest

instance Core.ToXML RestoreRequest where
  toXML RestoreRequest' {..} =
    Core.mconcat
      [ "Days" Core.@= days,
        "SelectParameters" Core.@= selectParameters,
        "Description" Core.@= description,
        "Type" Core.@= type',
        "OutputLocation" Core.@= outputLocation,
        "Tier" Core.@= tier,
        "GlacierJobParameters" Core.@= glacierJobParameters
      ]
