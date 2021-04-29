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
-- Module      : Network.AWS.S3.Types.RestoreRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.RestoreRequest where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    days :: Prelude.Maybe Prelude.Int,
    -- | Describes the parameters for Select job types.
    selectParameters :: Prelude.Maybe SelectParameters,
    -- | The optional description for the job.
    description :: Prelude.Maybe Prelude.Text,
    -- | Type of restore request.
    type' :: Prelude.Maybe RestoreRequestType,
    -- | Describes the location where the restore job\'s output is stored.
    outputLocation :: Prelude.Maybe OutputLocation,
    -- | Retrieval tier at which the restore will be processed.
    tier :: Prelude.Maybe Tier,
    -- | S3 Glacier related parameters pertaining to this job. Do not use with
    -- restores that specify @OutputLocation@.
    glacierJobParameters :: Prelude.Maybe GlacierJobParameters
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { days = Prelude.Nothing,
      selectParameters = Prelude.Nothing,
      description = Prelude.Nothing,
      type' = Prelude.Nothing,
      outputLocation = Prelude.Nothing,
      tier = Prelude.Nothing,
      glacierJobParameters = Prelude.Nothing
    }

-- | Lifetime of the active copy in days. Do not use with restores that
-- specify @OutputLocation@.
--
-- The Days element is required for regular restores, and must not be
-- provided for select requests.
restoreRequest_days :: Lens.Lens' RestoreRequest (Prelude.Maybe Prelude.Int)
restoreRequest_days = Lens.lens (\RestoreRequest' {days} -> days) (\s@RestoreRequest' {} a -> s {days = a} :: RestoreRequest)

-- | Describes the parameters for Select job types.
restoreRequest_selectParameters :: Lens.Lens' RestoreRequest (Prelude.Maybe SelectParameters)
restoreRequest_selectParameters = Lens.lens (\RestoreRequest' {selectParameters} -> selectParameters) (\s@RestoreRequest' {} a -> s {selectParameters = a} :: RestoreRequest)

-- | The optional description for the job.
restoreRequest_description :: Lens.Lens' RestoreRequest (Prelude.Maybe Prelude.Text)
restoreRequest_description = Lens.lens (\RestoreRequest' {description} -> description) (\s@RestoreRequest' {} a -> s {description = a} :: RestoreRequest)

-- | Type of restore request.
restoreRequest_type :: Lens.Lens' RestoreRequest (Prelude.Maybe RestoreRequestType)
restoreRequest_type = Lens.lens (\RestoreRequest' {type'} -> type') (\s@RestoreRequest' {} a -> s {type' = a} :: RestoreRequest)

-- | Describes the location where the restore job\'s output is stored.
restoreRequest_outputLocation :: Lens.Lens' RestoreRequest (Prelude.Maybe OutputLocation)
restoreRequest_outputLocation = Lens.lens (\RestoreRequest' {outputLocation} -> outputLocation) (\s@RestoreRequest' {} a -> s {outputLocation = a} :: RestoreRequest)

-- | Retrieval tier at which the restore will be processed.
restoreRequest_tier :: Lens.Lens' RestoreRequest (Prelude.Maybe Tier)
restoreRequest_tier = Lens.lens (\RestoreRequest' {tier} -> tier) (\s@RestoreRequest' {} a -> s {tier = a} :: RestoreRequest)

-- | S3 Glacier related parameters pertaining to this job. Do not use with
-- restores that specify @OutputLocation@.
restoreRequest_glacierJobParameters :: Lens.Lens' RestoreRequest (Prelude.Maybe GlacierJobParameters)
restoreRequest_glacierJobParameters = Lens.lens (\RestoreRequest' {glacierJobParameters} -> glacierJobParameters) (\s@RestoreRequest' {} a -> s {glacierJobParameters = a} :: RestoreRequest)

instance Prelude.Hashable RestoreRequest

instance Prelude.NFData RestoreRequest

instance Prelude.ToXML RestoreRequest where
  toXML RestoreRequest' {..} =
    Prelude.mconcat
      [ "Days" Prelude.@= days,
        "SelectParameters" Prelude.@= selectParameters,
        "Description" Prelude.@= description,
        "Type" Prelude.@= type',
        "OutputLocation" Prelude.@= outputLocation,
        "Tier" Prelude.@= tier,
        "GlacierJobParameters"
          Prelude.@= glacierJobParameters
      ]
