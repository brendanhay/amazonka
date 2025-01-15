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
-- Module      : Amazonka.IoTSiteWise.Types.JobSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.JobSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.JobStatus
import qualified Amazonka.Prelude as Prelude

-- | Contains a job summary information.
--
-- /See:/ 'newJobSummary' smart constructor.
data JobSummary = JobSummary'
  { -- | The ID of the job.
    id :: Prelude.Text,
    -- | The unique name that helps identify the job request.
    name :: Prelude.Text,
    -- | The status of the bulk import job can be one of following values.
    --
    -- -   @PENDING@ – IoT SiteWise is waiting for the current bulk import job
    --     to finish.
    --
    -- -   @CANCELLED@ – The bulk import job has been canceled.
    --
    -- -   @RUNNING@ – IoT SiteWise is processing your request to import your
    --     data from Amazon S3.
    --
    -- -   @COMPLETED@ – IoT SiteWise successfully completed your request to
    --     import data from Amazon S3.
    --
    -- -   @FAILED@ – IoT SiteWise couldn\'t process your request to import
    --     data from Amazon S3. You can use logs saved in the specified error
    --     report location in Amazon S3 to troubleshoot issues.
    --
    -- -   @COMPLETED_WITH_FAILURES@ – IoT SiteWise completed your request to
    --     import data from Amazon S3 with errors. You can use logs saved in
    --     the specified error report location in Amazon S3 to troubleshoot
    --     issues.
    status :: JobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'jobSummary_id' - The ID of the job.
--
-- 'name', 'jobSummary_name' - The unique name that helps identify the job request.
--
-- 'status', 'jobSummary_status' - The status of the bulk import job can be one of following values.
--
-- -   @PENDING@ – IoT SiteWise is waiting for the current bulk import job
--     to finish.
--
-- -   @CANCELLED@ – The bulk import job has been canceled.
--
-- -   @RUNNING@ – IoT SiteWise is processing your request to import your
--     data from Amazon S3.
--
-- -   @COMPLETED@ – IoT SiteWise successfully completed your request to
--     import data from Amazon S3.
--
-- -   @FAILED@ – IoT SiteWise couldn\'t process your request to import
--     data from Amazon S3. You can use logs saved in the specified error
--     report location in Amazon S3 to troubleshoot issues.
--
-- -   @COMPLETED_WITH_FAILURES@ – IoT SiteWise completed your request to
--     import data from Amazon S3 with errors. You can use logs saved in
--     the specified error report location in Amazon S3 to troubleshoot
--     issues.
newJobSummary ::
  -- | 'id'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'status'
  JobStatus ->
  JobSummary
newJobSummary pId_ pName_ pStatus_ =
  JobSummary'
    { id = pId_,
      name = pName_,
      status = pStatus_
    }

-- | The ID of the job.
jobSummary_id :: Lens.Lens' JobSummary Prelude.Text
jobSummary_id = Lens.lens (\JobSummary' {id} -> id) (\s@JobSummary' {} a -> s {id = a} :: JobSummary)

-- | The unique name that helps identify the job request.
jobSummary_name :: Lens.Lens' JobSummary Prelude.Text
jobSummary_name = Lens.lens (\JobSummary' {name} -> name) (\s@JobSummary' {} a -> s {name = a} :: JobSummary)

-- | The status of the bulk import job can be one of following values.
--
-- -   @PENDING@ – IoT SiteWise is waiting for the current bulk import job
--     to finish.
--
-- -   @CANCELLED@ – The bulk import job has been canceled.
--
-- -   @RUNNING@ – IoT SiteWise is processing your request to import your
--     data from Amazon S3.
--
-- -   @COMPLETED@ – IoT SiteWise successfully completed your request to
--     import data from Amazon S3.
--
-- -   @FAILED@ – IoT SiteWise couldn\'t process your request to import
--     data from Amazon S3. You can use logs saved in the specified error
--     report location in Amazon S3 to troubleshoot issues.
--
-- -   @COMPLETED_WITH_FAILURES@ – IoT SiteWise completed your request to
--     import data from Amazon S3 with errors. You can use logs saved in
--     the specified error report location in Amazon S3 to troubleshoot
--     issues.
jobSummary_status :: Lens.Lens' JobSummary JobStatus
jobSummary_status = Lens.lens (\JobSummary' {status} -> status) (\s@JobSummary' {} a -> s {status = a} :: JobSummary)

instance Data.FromJSON JobSummary where
  parseJSON =
    Data.withObject
      "JobSummary"
      ( \x ->
          JobSummary'
            Prelude.<$> (x Data..: "id")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "status")
      )

instance Prelude.Hashable JobSummary where
  hashWithSalt _salt JobSummary' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData JobSummary where
  rnf JobSummary' {..} =
    Prelude.rnf id `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf status
