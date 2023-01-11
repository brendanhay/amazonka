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
-- Module      : Amazonka.Glacier.Types.JobParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glacier.Types.JobParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glacier.Types.InventoryRetrievalJobInput
import Amazonka.Glacier.Types.OutputLocation
import Amazonka.Glacier.Types.SelectParameters
import qualified Amazonka.Prelude as Prelude

-- | Provides options for defining a job.
--
-- /See:/ 'newJobParameters' smart constructor.
data JobParameters = JobParameters'
  { -- | The ID of the archive that you want to retrieve. This field is required
    -- only if @Type@ is set to @select@ or @archive-retrieval@code>. An error
    -- occurs if you specify this request parameter for an inventory retrieval
    -- job request.
    archiveId :: Prelude.Maybe Prelude.Text,
    -- | The optional description for the job. The description must be less than
    -- or equal to 1,024 bytes. The allowable characters are 7-bit ASCII
    -- without control codes-specifically, ASCII values 32-126 decimal or
    -- 0x20-0x7E hexadecimal.
    description :: Prelude.Maybe Prelude.Text,
    -- | When initiating a job to retrieve a vault inventory, you can optionally
    -- add this parameter to your request to specify the output format. If you
    -- are initiating an inventory job and do not specify a Format field, JSON
    -- is the default format. Valid values are \"CSV\" and \"JSON\".
    format :: Prelude.Maybe Prelude.Text,
    -- | Input parameters used for range inventory retrieval.
    inventoryRetrievalParameters :: Prelude.Maybe InventoryRetrievalJobInput,
    -- | Contains information about the location where the select job results are
    -- stored.
    outputLocation :: Prelude.Maybe OutputLocation,
    -- | The byte range to retrieve for an archive retrieval. in the form
    -- \"/StartByteValue/-/EndByteValue/\" If not specified, the whole archive
    -- is retrieved. If specified, the byte range must be megabyte (1024*1024)
    -- aligned which means that /StartByteValue/ must be divisible by 1 MB and
    -- /EndByteValue/ plus 1 must be divisible by 1 MB or be the end of the
    -- archive specified as the archive byte size value minus 1. If
    -- RetrievalByteRange is not megabyte aligned, this operation returns a 400
    -- response.
    --
    -- An error occurs if you specify this field for an inventory retrieval job
    -- request.
    retrievalByteRange :: Prelude.Maybe Prelude.Text,
    -- | The Amazon SNS topic ARN to which Amazon S3 Glacier sends a notification
    -- when the job is completed and the output is ready for you to download.
    -- The specified topic publishes the notification to its subscribers. The
    -- SNS topic must exist.
    sNSTopic :: Prelude.Maybe Prelude.Text,
    -- | Contains the parameters that define a job.
    selectParameters :: Prelude.Maybe SelectParameters,
    -- | The tier to use for a select or an archive retrieval job. Valid values
    -- are @Expedited@, @Standard@, or @Bulk@. @Standard@ is the default.
    tier :: Prelude.Maybe Prelude.Text,
    -- | The job type. You can initiate a job to perform a select query on an
    -- archive, retrieve an archive, or get an inventory of a vault. Valid
    -- values are \"select\", \"archive-retrieval\" and
    -- \"inventory-retrieval\".
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'archiveId', 'jobParameters_archiveId' - The ID of the archive that you want to retrieve. This field is required
-- only if @Type@ is set to @select@ or @archive-retrieval@code>. An error
-- occurs if you specify this request parameter for an inventory retrieval
-- job request.
--
-- 'description', 'jobParameters_description' - The optional description for the job. The description must be less than
-- or equal to 1,024 bytes. The allowable characters are 7-bit ASCII
-- without control codes-specifically, ASCII values 32-126 decimal or
-- 0x20-0x7E hexadecimal.
--
-- 'format', 'jobParameters_format' - When initiating a job to retrieve a vault inventory, you can optionally
-- add this parameter to your request to specify the output format. If you
-- are initiating an inventory job and do not specify a Format field, JSON
-- is the default format. Valid values are \"CSV\" and \"JSON\".
--
-- 'inventoryRetrievalParameters', 'jobParameters_inventoryRetrievalParameters' - Input parameters used for range inventory retrieval.
--
-- 'outputLocation', 'jobParameters_outputLocation' - Contains information about the location where the select job results are
-- stored.
--
-- 'retrievalByteRange', 'jobParameters_retrievalByteRange' - The byte range to retrieve for an archive retrieval. in the form
-- \"/StartByteValue/-/EndByteValue/\" If not specified, the whole archive
-- is retrieved. If specified, the byte range must be megabyte (1024*1024)
-- aligned which means that /StartByteValue/ must be divisible by 1 MB and
-- /EndByteValue/ plus 1 must be divisible by 1 MB or be the end of the
-- archive specified as the archive byte size value minus 1. If
-- RetrievalByteRange is not megabyte aligned, this operation returns a 400
-- response.
--
-- An error occurs if you specify this field for an inventory retrieval job
-- request.
--
-- 'sNSTopic', 'jobParameters_sNSTopic' - The Amazon SNS topic ARN to which Amazon S3 Glacier sends a notification
-- when the job is completed and the output is ready for you to download.
-- The specified topic publishes the notification to its subscribers. The
-- SNS topic must exist.
--
-- 'selectParameters', 'jobParameters_selectParameters' - Contains the parameters that define a job.
--
-- 'tier', 'jobParameters_tier' - The tier to use for a select or an archive retrieval job. Valid values
-- are @Expedited@, @Standard@, or @Bulk@. @Standard@ is the default.
--
-- 'type'', 'jobParameters_type' - The job type. You can initiate a job to perform a select query on an
-- archive, retrieve an archive, or get an inventory of a vault. Valid
-- values are \"select\", \"archive-retrieval\" and
-- \"inventory-retrieval\".
newJobParameters ::
  JobParameters
newJobParameters =
  JobParameters'
    { archiveId = Prelude.Nothing,
      description = Prelude.Nothing,
      format = Prelude.Nothing,
      inventoryRetrievalParameters = Prelude.Nothing,
      outputLocation = Prelude.Nothing,
      retrievalByteRange = Prelude.Nothing,
      sNSTopic = Prelude.Nothing,
      selectParameters = Prelude.Nothing,
      tier = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The ID of the archive that you want to retrieve. This field is required
-- only if @Type@ is set to @select@ or @archive-retrieval@code>. An error
-- occurs if you specify this request parameter for an inventory retrieval
-- job request.
jobParameters_archiveId :: Lens.Lens' JobParameters (Prelude.Maybe Prelude.Text)
jobParameters_archiveId = Lens.lens (\JobParameters' {archiveId} -> archiveId) (\s@JobParameters' {} a -> s {archiveId = a} :: JobParameters)

-- | The optional description for the job. The description must be less than
-- or equal to 1,024 bytes. The allowable characters are 7-bit ASCII
-- without control codes-specifically, ASCII values 32-126 decimal or
-- 0x20-0x7E hexadecimal.
jobParameters_description :: Lens.Lens' JobParameters (Prelude.Maybe Prelude.Text)
jobParameters_description = Lens.lens (\JobParameters' {description} -> description) (\s@JobParameters' {} a -> s {description = a} :: JobParameters)

-- | When initiating a job to retrieve a vault inventory, you can optionally
-- add this parameter to your request to specify the output format. If you
-- are initiating an inventory job and do not specify a Format field, JSON
-- is the default format. Valid values are \"CSV\" and \"JSON\".
jobParameters_format :: Lens.Lens' JobParameters (Prelude.Maybe Prelude.Text)
jobParameters_format = Lens.lens (\JobParameters' {format} -> format) (\s@JobParameters' {} a -> s {format = a} :: JobParameters)

-- | Input parameters used for range inventory retrieval.
jobParameters_inventoryRetrievalParameters :: Lens.Lens' JobParameters (Prelude.Maybe InventoryRetrievalJobInput)
jobParameters_inventoryRetrievalParameters = Lens.lens (\JobParameters' {inventoryRetrievalParameters} -> inventoryRetrievalParameters) (\s@JobParameters' {} a -> s {inventoryRetrievalParameters = a} :: JobParameters)

-- | Contains information about the location where the select job results are
-- stored.
jobParameters_outputLocation :: Lens.Lens' JobParameters (Prelude.Maybe OutputLocation)
jobParameters_outputLocation = Lens.lens (\JobParameters' {outputLocation} -> outputLocation) (\s@JobParameters' {} a -> s {outputLocation = a} :: JobParameters)

-- | The byte range to retrieve for an archive retrieval. in the form
-- \"/StartByteValue/-/EndByteValue/\" If not specified, the whole archive
-- is retrieved. If specified, the byte range must be megabyte (1024*1024)
-- aligned which means that /StartByteValue/ must be divisible by 1 MB and
-- /EndByteValue/ plus 1 must be divisible by 1 MB or be the end of the
-- archive specified as the archive byte size value minus 1. If
-- RetrievalByteRange is not megabyte aligned, this operation returns a 400
-- response.
--
-- An error occurs if you specify this field for an inventory retrieval job
-- request.
jobParameters_retrievalByteRange :: Lens.Lens' JobParameters (Prelude.Maybe Prelude.Text)
jobParameters_retrievalByteRange = Lens.lens (\JobParameters' {retrievalByteRange} -> retrievalByteRange) (\s@JobParameters' {} a -> s {retrievalByteRange = a} :: JobParameters)

-- | The Amazon SNS topic ARN to which Amazon S3 Glacier sends a notification
-- when the job is completed and the output is ready for you to download.
-- The specified topic publishes the notification to its subscribers. The
-- SNS topic must exist.
jobParameters_sNSTopic :: Lens.Lens' JobParameters (Prelude.Maybe Prelude.Text)
jobParameters_sNSTopic = Lens.lens (\JobParameters' {sNSTopic} -> sNSTopic) (\s@JobParameters' {} a -> s {sNSTopic = a} :: JobParameters)

-- | Contains the parameters that define a job.
jobParameters_selectParameters :: Lens.Lens' JobParameters (Prelude.Maybe SelectParameters)
jobParameters_selectParameters = Lens.lens (\JobParameters' {selectParameters} -> selectParameters) (\s@JobParameters' {} a -> s {selectParameters = a} :: JobParameters)

-- | The tier to use for a select or an archive retrieval job. Valid values
-- are @Expedited@, @Standard@, or @Bulk@. @Standard@ is the default.
jobParameters_tier :: Lens.Lens' JobParameters (Prelude.Maybe Prelude.Text)
jobParameters_tier = Lens.lens (\JobParameters' {tier} -> tier) (\s@JobParameters' {} a -> s {tier = a} :: JobParameters)

-- | The job type. You can initiate a job to perform a select query on an
-- archive, retrieve an archive, or get an inventory of a vault. Valid
-- values are \"select\", \"archive-retrieval\" and
-- \"inventory-retrieval\".
jobParameters_type :: Lens.Lens' JobParameters (Prelude.Maybe Prelude.Text)
jobParameters_type = Lens.lens (\JobParameters' {type'} -> type') (\s@JobParameters' {} a -> s {type' = a} :: JobParameters)

instance Prelude.Hashable JobParameters where
  hashWithSalt _salt JobParameters' {..} =
    _salt `Prelude.hashWithSalt` archiveId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` inventoryRetrievalParameters
      `Prelude.hashWithSalt` outputLocation
      `Prelude.hashWithSalt` retrievalByteRange
      `Prelude.hashWithSalt` sNSTopic
      `Prelude.hashWithSalt` selectParameters
      `Prelude.hashWithSalt` tier
      `Prelude.hashWithSalt` type'

instance Prelude.NFData JobParameters where
  rnf JobParameters' {..} =
    Prelude.rnf archiveId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf inventoryRetrievalParameters
      `Prelude.seq` Prelude.rnf outputLocation
      `Prelude.seq` Prelude.rnf retrievalByteRange
      `Prelude.seq` Prelude.rnf sNSTopic
      `Prelude.seq` Prelude.rnf selectParameters
      `Prelude.seq` Prelude.rnf tier
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON JobParameters where
  toJSON JobParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ArchiveId" Data..=) Prelude.<$> archiveId,
            ("Description" Data..=) Prelude.<$> description,
            ("Format" Data..=) Prelude.<$> format,
            ("InventoryRetrievalParameters" Data..=)
              Prelude.<$> inventoryRetrievalParameters,
            ("OutputLocation" Data..=)
              Prelude.<$> outputLocation,
            ("RetrievalByteRange" Data..=)
              Prelude.<$> retrievalByteRange,
            ("SNSTopic" Data..=) Prelude.<$> sNSTopic,
            ("SelectParameters" Data..=)
              Prelude.<$> selectParameters,
            ("Tier" Data..=) Prelude.<$> tier,
            ("Type" Data..=) Prelude.<$> type'
          ]
      )
