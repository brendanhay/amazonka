{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Signer.StartSigningJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a signing job to be performed on the code provided. Signing
-- jobs are viewable by the @ListSigningJobs@ operation for two years after
-- they are performed. Note the following requirements:
--
-- -   You must create an Amazon S3 source bucket. For more information,
--     see
--     <http://docs.aws.amazon.com/AmazonS3/latest/gsg/CreatingABucket.html Create a Bucket>
--     in the /Amazon S3 Getting Started Guide/.
--
-- -   Your S3 source bucket must be version enabled.
--
-- -   You must create an S3 destination bucket. Code signing uses your S3
--     destination bucket to write your signed code.
--
-- -   You specify the name of the source and destination buckets when
--     calling the @StartSigningJob@ operation.
--
-- -   You must also specify a request token that identifies your request
--     to code signing.
--
-- You can call the DescribeSigningJob and the ListSigningJobs actions
-- after you call @StartSigningJob@.
--
-- For a Java example that shows how to use this action, see
-- <http://docs.aws.amazon.com/acm/latest/userguide/>
module Amazonka.Signer.StartSigningJob
  ( -- * Creating a Request
    StartSigningJob (..),
    newStartSigningJob,

    -- * Request Lenses
    startSigningJob_profileOwner,
    startSigningJob_source,
    startSigningJob_destination,
    startSigningJob_profileName,
    startSigningJob_clientRequestToken,

    -- * Destructuring the Response
    StartSigningJobResponse (..),
    newStartSigningJobResponse,

    -- * Response Lenses
    startSigningJobResponse_jobId,
    startSigningJobResponse_jobOwner,
    startSigningJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Signer.Types

-- | /See:/ 'newStartSigningJob' smart constructor.
data StartSigningJob = StartSigningJob'
  { -- | The AWS account ID of the signing profile owner.
    profileOwner :: Prelude.Maybe Prelude.Text,
    -- | The S3 bucket that contains the object to sign or a BLOB that contains
    -- your raw code.
    source :: Source,
    -- | The S3 bucket in which to save your signed object. The destination
    -- contains the name of your bucket and an optional prefix.
    destination :: Destination,
    -- | The name of the signing profile.
    profileName :: Prelude.Text,
    -- | String that identifies the signing request. All calls after the first
    -- that use this token return the same response as the first call.
    clientRequestToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSigningJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profileOwner', 'startSigningJob_profileOwner' - The AWS account ID of the signing profile owner.
--
-- 'source', 'startSigningJob_source' - The S3 bucket that contains the object to sign or a BLOB that contains
-- your raw code.
--
-- 'destination', 'startSigningJob_destination' - The S3 bucket in which to save your signed object. The destination
-- contains the name of your bucket and an optional prefix.
--
-- 'profileName', 'startSigningJob_profileName' - The name of the signing profile.
--
-- 'clientRequestToken', 'startSigningJob_clientRequestToken' - String that identifies the signing request. All calls after the first
-- that use this token return the same response as the first call.
newStartSigningJob ::
  -- | 'source'
  Source ->
  -- | 'destination'
  Destination ->
  -- | 'profileName'
  Prelude.Text ->
  -- | 'clientRequestToken'
  Prelude.Text ->
  StartSigningJob
newStartSigningJob
  pSource_
  pDestination_
  pProfileName_
  pClientRequestToken_ =
    StartSigningJob'
      { profileOwner = Prelude.Nothing,
        source = pSource_,
        destination = pDestination_,
        profileName = pProfileName_,
        clientRequestToken = pClientRequestToken_
      }

-- | The AWS account ID of the signing profile owner.
startSigningJob_profileOwner :: Lens.Lens' StartSigningJob (Prelude.Maybe Prelude.Text)
startSigningJob_profileOwner = Lens.lens (\StartSigningJob' {profileOwner} -> profileOwner) (\s@StartSigningJob' {} a -> s {profileOwner = a} :: StartSigningJob)

-- | The S3 bucket that contains the object to sign or a BLOB that contains
-- your raw code.
startSigningJob_source :: Lens.Lens' StartSigningJob Source
startSigningJob_source = Lens.lens (\StartSigningJob' {source} -> source) (\s@StartSigningJob' {} a -> s {source = a} :: StartSigningJob)

-- | The S3 bucket in which to save your signed object. The destination
-- contains the name of your bucket and an optional prefix.
startSigningJob_destination :: Lens.Lens' StartSigningJob Destination
startSigningJob_destination = Lens.lens (\StartSigningJob' {destination} -> destination) (\s@StartSigningJob' {} a -> s {destination = a} :: StartSigningJob)

-- | The name of the signing profile.
startSigningJob_profileName :: Lens.Lens' StartSigningJob Prelude.Text
startSigningJob_profileName = Lens.lens (\StartSigningJob' {profileName} -> profileName) (\s@StartSigningJob' {} a -> s {profileName = a} :: StartSigningJob)

-- | String that identifies the signing request. All calls after the first
-- that use this token return the same response as the first call.
startSigningJob_clientRequestToken :: Lens.Lens' StartSigningJob Prelude.Text
startSigningJob_clientRequestToken = Lens.lens (\StartSigningJob' {clientRequestToken} -> clientRequestToken) (\s@StartSigningJob' {} a -> s {clientRequestToken = a} :: StartSigningJob)

instance Core.AWSRequest StartSigningJob where
  type
    AWSResponse StartSigningJob =
      StartSigningJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartSigningJobResponse'
            Prelude.<$> (x Data..?> "jobId")
            Prelude.<*> (x Data..?> "jobOwner")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartSigningJob where
  hashWithSalt _salt StartSigningJob' {..} =
    _salt `Prelude.hashWithSalt` profileOwner
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` profileName
      `Prelude.hashWithSalt` clientRequestToken

instance Prelude.NFData StartSigningJob where
  rnf StartSigningJob' {..} =
    Prelude.rnf profileOwner
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf destination
      `Prelude.seq` Prelude.rnf profileName
      `Prelude.seq` Prelude.rnf clientRequestToken

instance Data.ToHeaders StartSigningJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartSigningJob where
  toJSON StartSigningJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("profileOwner" Data..=) Prelude.<$> profileOwner,
            Prelude.Just ("source" Data..= source),
            Prelude.Just ("destination" Data..= destination),
            Prelude.Just ("profileName" Data..= profileName),
            Prelude.Just
              ("clientRequestToken" Data..= clientRequestToken)
          ]
      )

instance Data.ToPath StartSigningJob where
  toPath = Prelude.const "/signing-jobs"

instance Data.ToQuery StartSigningJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartSigningJobResponse' smart constructor.
data StartSigningJobResponse = StartSigningJobResponse'
  { -- | The ID of your signing job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The AWS account ID of the signing job owner.
    jobOwner :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSigningJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'startSigningJobResponse_jobId' - The ID of your signing job.
--
-- 'jobOwner', 'startSigningJobResponse_jobOwner' - The AWS account ID of the signing job owner.
--
-- 'httpStatus', 'startSigningJobResponse_httpStatus' - The response's http status code.
newStartSigningJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartSigningJobResponse
newStartSigningJobResponse pHttpStatus_ =
  StartSigningJobResponse'
    { jobId = Prelude.Nothing,
      jobOwner = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of your signing job.
startSigningJobResponse_jobId :: Lens.Lens' StartSigningJobResponse (Prelude.Maybe Prelude.Text)
startSigningJobResponse_jobId = Lens.lens (\StartSigningJobResponse' {jobId} -> jobId) (\s@StartSigningJobResponse' {} a -> s {jobId = a} :: StartSigningJobResponse)

-- | The AWS account ID of the signing job owner.
startSigningJobResponse_jobOwner :: Lens.Lens' StartSigningJobResponse (Prelude.Maybe Prelude.Text)
startSigningJobResponse_jobOwner = Lens.lens (\StartSigningJobResponse' {jobOwner} -> jobOwner) (\s@StartSigningJobResponse' {} a -> s {jobOwner = a} :: StartSigningJobResponse)

-- | The response's http status code.
startSigningJobResponse_httpStatus :: Lens.Lens' StartSigningJobResponse Prelude.Int
startSigningJobResponse_httpStatus = Lens.lens (\StartSigningJobResponse' {httpStatus} -> httpStatus) (\s@StartSigningJobResponse' {} a -> s {httpStatus = a} :: StartSigningJobResponse)

instance Prelude.NFData StartSigningJobResponse where
  rnf StartSigningJobResponse' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf jobOwner
      `Prelude.seq` Prelude.rnf httpStatus
