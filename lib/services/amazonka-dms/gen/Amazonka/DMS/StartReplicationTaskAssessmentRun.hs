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
-- Module      : Amazonka.DMS.StartReplicationTaskAssessmentRun
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a new premigration assessment run for one or more individual
-- assessments of a migration task.
--
-- The assessments that you can specify depend on the source and target
-- database engine and the migration type defined for the given task. To
-- run this operation, your migration task must already be created. After
-- you run this operation, you can review the status of each individual
-- assessment. You can also run the migration task manually after the
-- assessment run and its individual assessments complete.
module Amazonka.DMS.StartReplicationTaskAssessmentRun
  ( -- * Creating a Request
    StartReplicationTaskAssessmentRun (..),
    newStartReplicationTaskAssessmentRun,

    -- * Request Lenses
    startReplicationTaskAssessmentRun_exclude,
    startReplicationTaskAssessmentRun_includeOnly,
    startReplicationTaskAssessmentRun_resultEncryptionMode,
    startReplicationTaskAssessmentRun_resultKmsKeyArn,
    startReplicationTaskAssessmentRun_resultLocationFolder,
    startReplicationTaskAssessmentRun_replicationTaskArn,
    startReplicationTaskAssessmentRun_serviceAccessRoleArn,
    startReplicationTaskAssessmentRun_resultLocationBucket,
    startReplicationTaskAssessmentRun_assessmentRunName,

    -- * Destructuring the Response
    StartReplicationTaskAssessmentRunResponse (..),
    newStartReplicationTaskAssessmentRunResponse,

    -- * Response Lenses
    startReplicationTaskAssessmentRunResponse_replicationTaskAssessmentRun,
    startReplicationTaskAssessmentRunResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newStartReplicationTaskAssessmentRun' smart constructor.
data StartReplicationTaskAssessmentRun = StartReplicationTaskAssessmentRun'
  { -- | Space-separated list of names for specific individual assessments that
    -- you want to exclude. These names come from the default list of
    -- individual assessments that DMS supports for the associated migration
    -- task. This task is specified by @ReplicationTaskArn@.
    --
    -- You can\'t set a value for @Exclude@ if you also set a value for
    -- @IncludeOnly@ in the API operation.
    --
    -- To identify the names of the default individual assessments that DMS
    -- supports for the associated migration task, run the
    -- @DescribeApplicableIndividualAssessments@ operation using its own
    -- @ReplicationTaskArn@ request parameter.
    exclude :: Prelude.Maybe [Prelude.Text],
    -- | Space-separated list of names for specific individual assessments that
    -- you want to include. These names come from the default list of
    -- individual assessments that DMS supports for the associated migration
    -- task. This task is specified by @ReplicationTaskArn@.
    --
    -- You can\'t set a value for @IncludeOnly@ if you also set a value for
    -- @Exclude@ in the API operation.
    --
    -- To identify the names of the default individual assessments that DMS
    -- supports for the associated migration task, run the
    -- @DescribeApplicableIndividualAssessments@ operation using its own
    -- @ReplicationTaskArn@ request parameter.
    includeOnly :: Prelude.Maybe [Prelude.Text],
    -- | Encryption mode that you can specify to encrypt the results of this
    -- assessment run. If you don\'t specify this request parameter, DMS stores
    -- the assessment run results without encryption. You can specify one of
    -- the options following:
    --
    -- -   @\"SSE_S3\"@ – The server-side encryption provided as a default by
    --     Amazon S3.
    --
    -- -   @\"SSE_KMS\"@ – Key Management Service (KMS) encryption. This
    --     encryption can use either a custom KMS encryption key that you
    --     specify or the default KMS encryption key that DMS provides.
    resultEncryptionMode :: Prelude.Maybe Prelude.Text,
    -- | ARN of a custom KMS encryption key that you specify when you set
    -- @ResultEncryptionMode@ to @\"SSE_KMS@\".
    resultKmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | Folder within an Amazon S3 bucket where you want DMS to store the
    -- results of this assessment run.
    resultLocationFolder :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) of the migration task associated with the
    -- premigration assessment run that you want to start.
    replicationTaskArn :: Prelude.Text,
    -- | ARN of the service role needed to start the assessment run. The role
    -- must allow the @iam:PassRole@ action.
    serviceAccessRoleArn :: Prelude.Text,
    -- | Amazon S3 bucket where you want DMS to store the results of this
    -- assessment run.
    resultLocationBucket :: Prelude.Text,
    -- | Unique name to identify the assessment run.
    assessmentRunName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartReplicationTaskAssessmentRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exclude', 'startReplicationTaskAssessmentRun_exclude' - Space-separated list of names for specific individual assessments that
-- you want to exclude. These names come from the default list of
-- individual assessments that DMS supports for the associated migration
-- task. This task is specified by @ReplicationTaskArn@.
--
-- You can\'t set a value for @Exclude@ if you also set a value for
-- @IncludeOnly@ in the API operation.
--
-- To identify the names of the default individual assessments that DMS
-- supports for the associated migration task, run the
-- @DescribeApplicableIndividualAssessments@ operation using its own
-- @ReplicationTaskArn@ request parameter.
--
-- 'includeOnly', 'startReplicationTaskAssessmentRun_includeOnly' - Space-separated list of names for specific individual assessments that
-- you want to include. These names come from the default list of
-- individual assessments that DMS supports for the associated migration
-- task. This task is specified by @ReplicationTaskArn@.
--
-- You can\'t set a value for @IncludeOnly@ if you also set a value for
-- @Exclude@ in the API operation.
--
-- To identify the names of the default individual assessments that DMS
-- supports for the associated migration task, run the
-- @DescribeApplicableIndividualAssessments@ operation using its own
-- @ReplicationTaskArn@ request parameter.
--
-- 'resultEncryptionMode', 'startReplicationTaskAssessmentRun_resultEncryptionMode' - Encryption mode that you can specify to encrypt the results of this
-- assessment run. If you don\'t specify this request parameter, DMS stores
-- the assessment run results without encryption. You can specify one of
-- the options following:
--
-- -   @\"SSE_S3\"@ – The server-side encryption provided as a default by
--     Amazon S3.
--
-- -   @\"SSE_KMS\"@ – Key Management Service (KMS) encryption. This
--     encryption can use either a custom KMS encryption key that you
--     specify or the default KMS encryption key that DMS provides.
--
-- 'resultKmsKeyArn', 'startReplicationTaskAssessmentRun_resultKmsKeyArn' - ARN of a custom KMS encryption key that you specify when you set
-- @ResultEncryptionMode@ to @\"SSE_KMS@\".
--
-- 'resultLocationFolder', 'startReplicationTaskAssessmentRun_resultLocationFolder' - Folder within an Amazon S3 bucket where you want DMS to store the
-- results of this assessment run.
--
-- 'replicationTaskArn', 'startReplicationTaskAssessmentRun_replicationTaskArn' - Amazon Resource Name (ARN) of the migration task associated with the
-- premigration assessment run that you want to start.
--
-- 'serviceAccessRoleArn', 'startReplicationTaskAssessmentRun_serviceAccessRoleArn' - ARN of the service role needed to start the assessment run. The role
-- must allow the @iam:PassRole@ action.
--
-- 'resultLocationBucket', 'startReplicationTaskAssessmentRun_resultLocationBucket' - Amazon S3 bucket where you want DMS to store the results of this
-- assessment run.
--
-- 'assessmentRunName', 'startReplicationTaskAssessmentRun_assessmentRunName' - Unique name to identify the assessment run.
newStartReplicationTaskAssessmentRun ::
  -- | 'replicationTaskArn'
  Prelude.Text ->
  -- | 'serviceAccessRoleArn'
  Prelude.Text ->
  -- | 'resultLocationBucket'
  Prelude.Text ->
  -- | 'assessmentRunName'
  Prelude.Text ->
  StartReplicationTaskAssessmentRun
newStartReplicationTaskAssessmentRun
  pReplicationTaskArn_
  pServiceAccessRoleArn_
  pResultLocationBucket_
  pAssessmentRunName_ =
    StartReplicationTaskAssessmentRun'
      { exclude =
          Prelude.Nothing,
        includeOnly = Prelude.Nothing,
        resultEncryptionMode = Prelude.Nothing,
        resultKmsKeyArn = Prelude.Nothing,
        resultLocationFolder = Prelude.Nothing,
        replicationTaskArn =
          pReplicationTaskArn_,
        serviceAccessRoleArn =
          pServiceAccessRoleArn_,
        resultLocationBucket =
          pResultLocationBucket_,
        assessmentRunName = pAssessmentRunName_
      }

-- | Space-separated list of names for specific individual assessments that
-- you want to exclude. These names come from the default list of
-- individual assessments that DMS supports for the associated migration
-- task. This task is specified by @ReplicationTaskArn@.
--
-- You can\'t set a value for @Exclude@ if you also set a value for
-- @IncludeOnly@ in the API operation.
--
-- To identify the names of the default individual assessments that DMS
-- supports for the associated migration task, run the
-- @DescribeApplicableIndividualAssessments@ operation using its own
-- @ReplicationTaskArn@ request parameter.
startReplicationTaskAssessmentRun_exclude :: Lens.Lens' StartReplicationTaskAssessmentRun (Prelude.Maybe [Prelude.Text])
startReplicationTaskAssessmentRun_exclude = Lens.lens (\StartReplicationTaskAssessmentRun' {exclude} -> exclude) (\s@StartReplicationTaskAssessmentRun' {} a -> s {exclude = a} :: StartReplicationTaskAssessmentRun) Prelude.. Lens.mapping Lens.coerced

-- | Space-separated list of names for specific individual assessments that
-- you want to include. These names come from the default list of
-- individual assessments that DMS supports for the associated migration
-- task. This task is specified by @ReplicationTaskArn@.
--
-- You can\'t set a value for @IncludeOnly@ if you also set a value for
-- @Exclude@ in the API operation.
--
-- To identify the names of the default individual assessments that DMS
-- supports for the associated migration task, run the
-- @DescribeApplicableIndividualAssessments@ operation using its own
-- @ReplicationTaskArn@ request parameter.
startReplicationTaskAssessmentRun_includeOnly :: Lens.Lens' StartReplicationTaskAssessmentRun (Prelude.Maybe [Prelude.Text])
startReplicationTaskAssessmentRun_includeOnly = Lens.lens (\StartReplicationTaskAssessmentRun' {includeOnly} -> includeOnly) (\s@StartReplicationTaskAssessmentRun' {} a -> s {includeOnly = a} :: StartReplicationTaskAssessmentRun) Prelude.. Lens.mapping Lens.coerced

-- | Encryption mode that you can specify to encrypt the results of this
-- assessment run. If you don\'t specify this request parameter, DMS stores
-- the assessment run results without encryption. You can specify one of
-- the options following:
--
-- -   @\"SSE_S3\"@ – The server-side encryption provided as a default by
--     Amazon S3.
--
-- -   @\"SSE_KMS\"@ – Key Management Service (KMS) encryption. This
--     encryption can use either a custom KMS encryption key that you
--     specify or the default KMS encryption key that DMS provides.
startReplicationTaskAssessmentRun_resultEncryptionMode :: Lens.Lens' StartReplicationTaskAssessmentRun (Prelude.Maybe Prelude.Text)
startReplicationTaskAssessmentRun_resultEncryptionMode = Lens.lens (\StartReplicationTaskAssessmentRun' {resultEncryptionMode} -> resultEncryptionMode) (\s@StartReplicationTaskAssessmentRun' {} a -> s {resultEncryptionMode = a} :: StartReplicationTaskAssessmentRun)

-- | ARN of a custom KMS encryption key that you specify when you set
-- @ResultEncryptionMode@ to @\"SSE_KMS@\".
startReplicationTaskAssessmentRun_resultKmsKeyArn :: Lens.Lens' StartReplicationTaskAssessmentRun (Prelude.Maybe Prelude.Text)
startReplicationTaskAssessmentRun_resultKmsKeyArn = Lens.lens (\StartReplicationTaskAssessmentRun' {resultKmsKeyArn} -> resultKmsKeyArn) (\s@StartReplicationTaskAssessmentRun' {} a -> s {resultKmsKeyArn = a} :: StartReplicationTaskAssessmentRun)

-- | Folder within an Amazon S3 bucket where you want DMS to store the
-- results of this assessment run.
startReplicationTaskAssessmentRun_resultLocationFolder :: Lens.Lens' StartReplicationTaskAssessmentRun (Prelude.Maybe Prelude.Text)
startReplicationTaskAssessmentRun_resultLocationFolder = Lens.lens (\StartReplicationTaskAssessmentRun' {resultLocationFolder} -> resultLocationFolder) (\s@StartReplicationTaskAssessmentRun' {} a -> s {resultLocationFolder = a} :: StartReplicationTaskAssessmentRun)

-- | Amazon Resource Name (ARN) of the migration task associated with the
-- premigration assessment run that you want to start.
startReplicationTaskAssessmentRun_replicationTaskArn :: Lens.Lens' StartReplicationTaskAssessmentRun Prelude.Text
startReplicationTaskAssessmentRun_replicationTaskArn = Lens.lens (\StartReplicationTaskAssessmentRun' {replicationTaskArn} -> replicationTaskArn) (\s@StartReplicationTaskAssessmentRun' {} a -> s {replicationTaskArn = a} :: StartReplicationTaskAssessmentRun)

-- | ARN of the service role needed to start the assessment run. The role
-- must allow the @iam:PassRole@ action.
startReplicationTaskAssessmentRun_serviceAccessRoleArn :: Lens.Lens' StartReplicationTaskAssessmentRun Prelude.Text
startReplicationTaskAssessmentRun_serviceAccessRoleArn = Lens.lens (\StartReplicationTaskAssessmentRun' {serviceAccessRoleArn} -> serviceAccessRoleArn) (\s@StartReplicationTaskAssessmentRun' {} a -> s {serviceAccessRoleArn = a} :: StartReplicationTaskAssessmentRun)

-- | Amazon S3 bucket where you want DMS to store the results of this
-- assessment run.
startReplicationTaskAssessmentRun_resultLocationBucket :: Lens.Lens' StartReplicationTaskAssessmentRun Prelude.Text
startReplicationTaskAssessmentRun_resultLocationBucket = Lens.lens (\StartReplicationTaskAssessmentRun' {resultLocationBucket} -> resultLocationBucket) (\s@StartReplicationTaskAssessmentRun' {} a -> s {resultLocationBucket = a} :: StartReplicationTaskAssessmentRun)

-- | Unique name to identify the assessment run.
startReplicationTaskAssessmentRun_assessmentRunName :: Lens.Lens' StartReplicationTaskAssessmentRun Prelude.Text
startReplicationTaskAssessmentRun_assessmentRunName = Lens.lens (\StartReplicationTaskAssessmentRun' {assessmentRunName} -> assessmentRunName) (\s@StartReplicationTaskAssessmentRun' {} a -> s {assessmentRunName = a} :: StartReplicationTaskAssessmentRun)

instance
  Core.AWSRequest
    StartReplicationTaskAssessmentRun
  where
  type
    AWSResponse StartReplicationTaskAssessmentRun =
      StartReplicationTaskAssessmentRunResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartReplicationTaskAssessmentRunResponse'
            Prelude.<$> (x Data..?> "ReplicationTaskAssessmentRun")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartReplicationTaskAssessmentRun
  where
  hashWithSalt
    _salt
    StartReplicationTaskAssessmentRun' {..} =
      _salt `Prelude.hashWithSalt` exclude
        `Prelude.hashWithSalt` includeOnly
        `Prelude.hashWithSalt` resultEncryptionMode
        `Prelude.hashWithSalt` resultKmsKeyArn
        `Prelude.hashWithSalt` resultLocationFolder
        `Prelude.hashWithSalt` replicationTaskArn
        `Prelude.hashWithSalt` serviceAccessRoleArn
        `Prelude.hashWithSalt` resultLocationBucket
        `Prelude.hashWithSalt` assessmentRunName

instance
  Prelude.NFData
    StartReplicationTaskAssessmentRun
  where
  rnf StartReplicationTaskAssessmentRun' {..} =
    Prelude.rnf exclude
      `Prelude.seq` Prelude.rnf includeOnly
      `Prelude.seq` Prelude.rnf resultEncryptionMode
      `Prelude.seq` Prelude.rnf resultKmsKeyArn
      `Prelude.seq` Prelude.rnf resultLocationFolder
      `Prelude.seq` Prelude.rnf replicationTaskArn
      `Prelude.seq` Prelude.rnf serviceAccessRoleArn
      `Prelude.seq` Prelude.rnf resultLocationBucket
      `Prelude.seq` Prelude.rnf assessmentRunName

instance
  Data.ToHeaders
    StartReplicationTaskAssessmentRun
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDMSv20160101.StartReplicationTaskAssessmentRun" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    StartReplicationTaskAssessmentRun
  where
  toJSON StartReplicationTaskAssessmentRun' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Exclude" Data..=) Prelude.<$> exclude,
            ("IncludeOnly" Data..=) Prelude.<$> includeOnly,
            ("ResultEncryptionMode" Data..=)
              Prelude.<$> resultEncryptionMode,
            ("ResultKmsKeyArn" Data..=)
              Prelude.<$> resultKmsKeyArn,
            ("ResultLocationFolder" Data..=)
              Prelude.<$> resultLocationFolder,
            Prelude.Just
              ("ReplicationTaskArn" Data..= replicationTaskArn),
            Prelude.Just
              ( "ServiceAccessRoleArn"
                  Data..= serviceAccessRoleArn
              ),
            Prelude.Just
              ( "ResultLocationBucket"
                  Data..= resultLocationBucket
              ),
            Prelude.Just
              ("AssessmentRunName" Data..= assessmentRunName)
          ]
      )

instance
  Data.ToPath
    StartReplicationTaskAssessmentRun
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    StartReplicationTaskAssessmentRun
  where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newStartReplicationTaskAssessmentRunResponse' smart constructor.
data StartReplicationTaskAssessmentRunResponse = StartReplicationTaskAssessmentRunResponse'
  { -- | The premigration assessment run that was started.
    replicationTaskAssessmentRun :: Prelude.Maybe ReplicationTaskAssessmentRun,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartReplicationTaskAssessmentRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationTaskAssessmentRun', 'startReplicationTaskAssessmentRunResponse_replicationTaskAssessmentRun' - The premigration assessment run that was started.
--
-- 'httpStatus', 'startReplicationTaskAssessmentRunResponse_httpStatus' - The response's http status code.
newStartReplicationTaskAssessmentRunResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartReplicationTaskAssessmentRunResponse
newStartReplicationTaskAssessmentRunResponse
  pHttpStatus_ =
    StartReplicationTaskAssessmentRunResponse'
      { replicationTaskAssessmentRun =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The premigration assessment run that was started.
startReplicationTaskAssessmentRunResponse_replicationTaskAssessmentRun :: Lens.Lens' StartReplicationTaskAssessmentRunResponse (Prelude.Maybe ReplicationTaskAssessmentRun)
startReplicationTaskAssessmentRunResponse_replicationTaskAssessmentRun = Lens.lens (\StartReplicationTaskAssessmentRunResponse' {replicationTaskAssessmentRun} -> replicationTaskAssessmentRun) (\s@StartReplicationTaskAssessmentRunResponse' {} a -> s {replicationTaskAssessmentRun = a} :: StartReplicationTaskAssessmentRunResponse)

-- | The response's http status code.
startReplicationTaskAssessmentRunResponse_httpStatus :: Lens.Lens' StartReplicationTaskAssessmentRunResponse Prelude.Int
startReplicationTaskAssessmentRunResponse_httpStatus = Lens.lens (\StartReplicationTaskAssessmentRunResponse' {httpStatus} -> httpStatus) (\s@StartReplicationTaskAssessmentRunResponse' {} a -> s {httpStatus = a} :: StartReplicationTaskAssessmentRunResponse)

instance
  Prelude.NFData
    StartReplicationTaskAssessmentRunResponse
  where
  rnf StartReplicationTaskAssessmentRunResponse' {..} =
    Prelude.rnf replicationTaskAssessmentRun
      `Prelude.seq` Prelude.rnf httpStatus
