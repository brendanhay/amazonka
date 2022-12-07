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
-- Module      : Amazonka.DataExchange.Types.JobEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.JobEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types.JobError
import Amazonka.DataExchange.Types.ResponseDetails
import Amazonka.DataExchange.Types.State
import Amazonka.DataExchange.Types.Type
import qualified Amazonka.Prelude as Prelude

-- | AWS Data Exchange Jobs are asynchronous import or export operations used
-- to create or copy assets. A data set owner can both import and export as
-- they see fit. Someone with an entitlement to a data set can only export.
-- Jobs are deleted 90 days after they are created.
--
-- /See:/ 'newJobEntry' smart constructor.
data JobEntry = JobEntry'
  { -- | Errors for jobs.
    errors :: Prelude.Maybe [JobError],
    -- | The ARN for the job.
    arn :: Prelude.Text,
    -- | The date and time that the job was created, in ISO 8601 format.
    createdAt :: Data.POSIX,
    -- | Details of the operation to be performed by the job, such as export
    -- destination details or import source details.
    details :: ResponseDetails,
    -- | The unique identifier for the job.
    id :: Prelude.Text,
    -- | The state of the job.
    state :: State,
    -- | The job type.
    type' :: Type,
    -- | The date and time that the job was last updated, in ISO 8601 format.
    updatedAt :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errors', 'jobEntry_errors' - Errors for jobs.
--
-- 'arn', 'jobEntry_arn' - The ARN for the job.
--
-- 'createdAt', 'jobEntry_createdAt' - The date and time that the job was created, in ISO 8601 format.
--
-- 'details', 'jobEntry_details' - Details of the operation to be performed by the job, such as export
-- destination details or import source details.
--
-- 'id', 'jobEntry_id' - The unique identifier for the job.
--
-- 'state', 'jobEntry_state' - The state of the job.
--
-- 'type'', 'jobEntry_type' - The job type.
--
-- 'updatedAt', 'jobEntry_updatedAt' - The date and time that the job was last updated, in ISO 8601 format.
newJobEntry ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'details'
  ResponseDetails ->
  -- | 'id'
  Prelude.Text ->
  -- | 'state'
  State ->
  -- | 'type''
  Type ->
  -- | 'updatedAt'
  Prelude.UTCTime ->
  JobEntry
newJobEntry
  pArn_
  pCreatedAt_
  pDetails_
  pId_
  pState_
  pType_
  pUpdatedAt_ =
    JobEntry'
      { errors = Prelude.Nothing,
        arn = pArn_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        details = pDetails_,
        id = pId_,
        state = pState_,
        type' = pType_,
        updatedAt = Data._Time Lens.# pUpdatedAt_
      }

-- | Errors for jobs.
jobEntry_errors :: Lens.Lens' JobEntry (Prelude.Maybe [JobError])
jobEntry_errors = Lens.lens (\JobEntry' {errors} -> errors) (\s@JobEntry' {} a -> s {errors = a} :: JobEntry) Prelude.. Lens.mapping Lens.coerced

-- | The ARN for the job.
jobEntry_arn :: Lens.Lens' JobEntry Prelude.Text
jobEntry_arn = Lens.lens (\JobEntry' {arn} -> arn) (\s@JobEntry' {} a -> s {arn = a} :: JobEntry)

-- | The date and time that the job was created, in ISO 8601 format.
jobEntry_createdAt :: Lens.Lens' JobEntry Prelude.UTCTime
jobEntry_createdAt = Lens.lens (\JobEntry' {createdAt} -> createdAt) (\s@JobEntry' {} a -> s {createdAt = a} :: JobEntry) Prelude.. Data._Time

-- | Details of the operation to be performed by the job, such as export
-- destination details or import source details.
jobEntry_details :: Lens.Lens' JobEntry ResponseDetails
jobEntry_details = Lens.lens (\JobEntry' {details} -> details) (\s@JobEntry' {} a -> s {details = a} :: JobEntry)

-- | The unique identifier for the job.
jobEntry_id :: Lens.Lens' JobEntry Prelude.Text
jobEntry_id = Lens.lens (\JobEntry' {id} -> id) (\s@JobEntry' {} a -> s {id = a} :: JobEntry)

-- | The state of the job.
jobEntry_state :: Lens.Lens' JobEntry State
jobEntry_state = Lens.lens (\JobEntry' {state} -> state) (\s@JobEntry' {} a -> s {state = a} :: JobEntry)

-- | The job type.
jobEntry_type :: Lens.Lens' JobEntry Type
jobEntry_type = Lens.lens (\JobEntry' {type'} -> type') (\s@JobEntry' {} a -> s {type' = a} :: JobEntry)

-- | The date and time that the job was last updated, in ISO 8601 format.
jobEntry_updatedAt :: Lens.Lens' JobEntry Prelude.UTCTime
jobEntry_updatedAt = Lens.lens (\JobEntry' {updatedAt} -> updatedAt) (\s@JobEntry' {} a -> s {updatedAt = a} :: JobEntry) Prelude.. Data._Time

instance Data.FromJSON JobEntry where
  parseJSON =
    Data.withObject
      "JobEntry"
      ( \x ->
          JobEntry'
            Prelude.<$> (x Data..:? "Errors" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Arn")
            Prelude.<*> (x Data..: "CreatedAt")
            Prelude.<*> (x Data..: "Details")
            Prelude.<*> (x Data..: "Id")
            Prelude.<*> (x Data..: "State")
            Prelude.<*> (x Data..: "Type")
            Prelude.<*> (x Data..: "UpdatedAt")
      )

instance Prelude.Hashable JobEntry where
  hashWithSalt _salt JobEntry' {..} =
    _salt `Prelude.hashWithSalt` errors
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` details
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData JobEntry where
  rnf JobEntry' {..} =
    Prelude.rnf errors
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf details
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf updatedAt
