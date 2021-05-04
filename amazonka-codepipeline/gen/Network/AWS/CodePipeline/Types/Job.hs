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
-- Module      : Network.AWS.CodePipeline.Types.Job
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.Job where

import Network.AWS.CodePipeline.Types.JobData
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents information about a job.
--
-- /See:/ 'newJob' smart constructor.
data Job = Job'
  { -- | The ID of the AWS account to use when performing the job.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | A system-generated random number that AWS CodePipeline uses to ensure
    -- that the job is being worked on by only one job worker. Use this number
    -- in an AcknowledgeJob request.
    nonce :: Prelude.Maybe Prelude.Text,
    -- | Other data about a job.
    data' :: Prelude.Maybe JobData,
    -- | The unique system-generated ID of the job.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Job' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'job_accountId' - The ID of the AWS account to use when performing the job.
--
-- 'nonce', 'job_nonce' - A system-generated random number that AWS CodePipeline uses to ensure
-- that the job is being worked on by only one job worker. Use this number
-- in an AcknowledgeJob request.
--
-- 'data'', 'job_data' - Other data about a job.
--
-- 'id', 'job_id' - The unique system-generated ID of the job.
newJob ::
  Job
newJob =
  Job'
    { accountId = Prelude.Nothing,
      nonce = Prelude.Nothing,
      data' = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The ID of the AWS account to use when performing the job.
job_accountId :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_accountId = Lens.lens (\Job' {accountId} -> accountId) (\s@Job' {} a -> s {accountId = a} :: Job)

-- | A system-generated random number that AWS CodePipeline uses to ensure
-- that the job is being worked on by only one job worker. Use this number
-- in an AcknowledgeJob request.
job_nonce :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_nonce = Lens.lens (\Job' {nonce} -> nonce) (\s@Job' {} a -> s {nonce = a} :: Job)

-- | Other data about a job.
job_data :: Lens.Lens' Job (Prelude.Maybe JobData)
job_data = Lens.lens (\Job' {data'} -> data') (\s@Job' {} a -> s {data' = a} :: Job)

-- | The unique system-generated ID of the job.
job_id :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_id = Lens.lens (\Job' {id} -> id) (\s@Job' {} a -> s {id = a} :: Job)

instance Prelude.FromJSON Job where
  parseJSON =
    Prelude.withObject
      "Job"
      ( \x ->
          Job'
            Prelude.<$> (x Prelude..:? "accountId")
            Prelude.<*> (x Prelude..:? "nonce")
            Prelude.<*> (x Prelude..:? "data")
            Prelude.<*> (x Prelude..:? "id")
      )

instance Prelude.Hashable Job

instance Prelude.NFData Job
