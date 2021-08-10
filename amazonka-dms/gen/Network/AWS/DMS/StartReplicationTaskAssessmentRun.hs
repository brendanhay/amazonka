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
-- Module      : Network.AWS.DMS.StartReplicationTaskAssessmentRun
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.DMS.StartReplicationTaskAssessmentRun
  ( -- * Creating a Request
    StartReplicationTaskAssessmentRun (..),
    newStartReplicationTaskAssessmentRun,

    -- * Request Lenses
    startReplicationTaskAssessmentRun_resultKmsKeyArn,
    startReplicationTaskAssessmentRun_resultEncryptionMode,
    startReplicationTaskAssessmentRun_exclude,
    startReplicationTaskAssessmentRun_includeOnly,
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

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newStartReplicationTaskAssessmentRun' smart constructor.
data StartReplicationTaskAssessmentRun = StartReplicationTaskAssessmentRun'
  { -- | ARN of a custom KMS encryption key that you specify when you set
    -- @ResultEncryptionMode@ to @\"SSE_KMS@\".
    resultKmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | Encryption mode that you can specify to encrypt the results of this
    -- assessment run. If you don\'t specify this request parameter, AWS DMS
    -- stores the assessment run results without encryption. You can specify
    -- one of the options following:
    --
    -- -   @\"SSE_S3\"@ – The server-side encryption provided as a default by
    --     Amazon S3.
    --
    -- -   @\"SSE_KMS\"@ – AWS Key Management Service (AWS KMS) encryption.
    --     This encryption can use either a custom KMS encryption key that you
    --     specify or the default KMS encryption key that DMS provides.
    resultEncryptionMode :: Prelude.Maybe Prelude.Text,
    -- | Space-separated list of names for specific individual assessments that
    -- you want to exclude. These names come from the default list of
    -- individual assessments that AWS DMS supports for the associated
    -- migration task. This task is specified by @ReplicationTaskArn@.
    --
    -- You can\'t set a value for @Exclude@ if you also set a value for
    -- @IncludeOnly@ in the API operation.
    --
    -- To identify the names of the default individual assessments that AWS DMS
    -- supports for the associated migration task, run the
    -- @DescribeApplicableIndividualAssessments@ operation using its own
    -- @ReplicationTaskArn@ request parameter.
    exclude :: Prelude.Maybe [Prelude.Text],
    -- | Space-separated list of names for specific individual assessments that
    -- you want to include. These names come from the default list of
    -- individual assessments that AWS DMS supports for the associated
    -- migration task. This task is specified by @ReplicationTaskArn@.
    --
    -- You can\'t set a value for @IncludeOnly@ if you also set a value for
    -- @Exclude@ in the API operation.
    --
    -- To identify the names of the default individual assessments that AWS DMS
    -- supports for the associated migration task, run the
    -- @DescribeApplicableIndividualAssessments@ operation using its own
    -- @ReplicationTaskArn@ request parameter.
    includeOnly :: Prelude.Maybe [Prelude.Text],
    -- | Folder within an Amazon S3 bucket where you want AWS DMS to store the
    -- results of this assessment run.
    resultLocationFolder :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) of the migration task associated with the
    -- premigration assessment run that you want to start.
    replicationTaskArn :: Prelude.Text,
    -- | ARN of a service role needed to start the assessment run.
    serviceAccessRoleArn :: Prelude.Text,
    -- | Amazon S3 bucket where you want AWS DMS to store the results of this
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
-- 'resultKmsKeyArn', 'startReplicationTaskAssessmentRun_resultKmsKeyArn' - ARN of a custom KMS encryption key that you specify when you set
-- @ResultEncryptionMode@ to @\"SSE_KMS@\".
--
-- 'resultEncryptionMode', 'startReplicationTaskAssessmentRun_resultEncryptionMode' - Encryption mode that you can specify to encrypt the results of this
-- assessment run. If you don\'t specify this request parameter, AWS DMS
-- stores the assessment run results without encryption. You can specify
-- one of the options following:
--
-- -   @\"SSE_S3\"@ – The server-side encryption provided as a default by
--     Amazon S3.
--
-- -   @\"SSE_KMS\"@ – AWS Key Management Service (AWS KMS) encryption.
--     This encryption can use either a custom KMS encryption key that you
--     specify or the default KMS encryption key that DMS provides.
--
-- 'exclude', 'startReplicationTaskAssessmentRun_exclude' - Space-separated list of names for specific individual assessments that
-- you want to exclude. These names come from the default list of
-- individual assessments that AWS DMS supports for the associated
-- migration task. This task is specified by @ReplicationTaskArn@.
--
-- You can\'t set a value for @Exclude@ if you also set a value for
-- @IncludeOnly@ in the API operation.
--
-- To identify the names of the default individual assessments that AWS DMS
-- supports for the associated migration task, run the
-- @DescribeApplicableIndividualAssessments@ operation using its own
-- @ReplicationTaskArn@ request parameter.
--
-- 'includeOnly', 'startReplicationTaskAssessmentRun_includeOnly' - Space-separated list of names for specific individual assessments that
-- you want to include. These names come from the default list of
-- individual assessments that AWS DMS supports for the associated
-- migration task. This task is specified by @ReplicationTaskArn@.
--
-- You can\'t set a value for @IncludeOnly@ if you also set a value for
-- @Exclude@ in the API operation.
--
-- To identify the names of the default individual assessments that AWS DMS
-- supports for the associated migration task, run the
-- @DescribeApplicableIndividualAssessments@ operation using its own
-- @ReplicationTaskArn@ request parameter.
--
-- 'resultLocationFolder', 'startReplicationTaskAssessmentRun_resultLocationFolder' - Folder within an Amazon S3 bucket where you want AWS DMS to store the
-- results of this assessment run.
--
-- 'replicationTaskArn', 'startReplicationTaskAssessmentRun_replicationTaskArn' - Amazon Resource Name (ARN) of the migration task associated with the
-- premigration assessment run that you want to start.
--
-- 'serviceAccessRoleArn', 'startReplicationTaskAssessmentRun_serviceAccessRoleArn' - ARN of a service role needed to start the assessment run.
--
-- 'resultLocationBucket', 'startReplicationTaskAssessmentRun_resultLocationBucket' - Amazon S3 bucket where you want AWS DMS to store the results of this
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
      { resultKmsKeyArn =
          Prelude.Nothing,
        resultEncryptionMode = Prelude.Nothing,
        exclude = Prelude.Nothing,
        includeOnly = Prelude.Nothing,
        resultLocationFolder = Prelude.Nothing,
        replicationTaskArn =
          pReplicationTaskArn_,
        serviceAccessRoleArn =
          pServiceAccessRoleArn_,
        resultLocationBucket =
          pResultLocationBucket_,
        assessmentRunName = pAssessmentRunName_
      }

-- | ARN of a custom KMS encryption key that you specify when you set
-- @ResultEncryptionMode@ to @\"SSE_KMS@\".
startReplicationTaskAssessmentRun_resultKmsKeyArn :: Lens.Lens' StartReplicationTaskAssessmentRun (Prelude.Maybe Prelude.Text)
startReplicationTaskAssessmentRun_resultKmsKeyArn = Lens.lens (\StartReplicationTaskAssessmentRun' {resultKmsKeyArn} -> resultKmsKeyArn) (\s@StartReplicationTaskAssessmentRun' {} a -> s {resultKmsKeyArn = a} :: StartReplicationTaskAssessmentRun)

-- | Encryption mode that you can specify to encrypt the results of this
-- assessment run. If you don\'t specify this request parameter, AWS DMS
-- stores the assessment run results without encryption. You can specify
-- one of the options following:
--
-- -   @\"SSE_S3\"@ – The server-side encryption provided as a default by
--     Amazon S3.
--
-- -   @\"SSE_KMS\"@ – AWS Key Management Service (AWS KMS) encryption.
--     This encryption can use either a custom KMS encryption key that you
--     specify or the default KMS encryption key that DMS provides.
startReplicationTaskAssessmentRun_resultEncryptionMode :: Lens.Lens' StartReplicationTaskAssessmentRun (Prelude.Maybe Prelude.Text)
startReplicationTaskAssessmentRun_resultEncryptionMode = Lens.lens (\StartReplicationTaskAssessmentRun' {resultEncryptionMode} -> resultEncryptionMode) (\s@StartReplicationTaskAssessmentRun' {} a -> s {resultEncryptionMode = a} :: StartReplicationTaskAssessmentRun)

-- | Space-separated list of names for specific individual assessments that
-- you want to exclude. These names come from the default list of
-- individual assessments that AWS DMS supports for the associated
-- migration task. This task is specified by @ReplicationTaskArn@.
--
-- You can\'t set a value for @Exclude@ if you also set a value for
-- @IncludeOnly@ in the API operation.
--
-- To identify the names of the default individual assessments that AWS DMS
-- supports for the associated migration task, run the
-- @DescribeApplicableIndividualAssessments@ operation using its own
-- @ReplicationTaskArn@ request parameter.
startReplicationTaskAssessmentRun_exclude :: Lens.Lens' StartReplicationTaskAssessmentRun (Prelude.Maybe [Prelude.Text])
startReplicationTaskAssessmentRun_exclude = Lens.lens (\StartReplicationTaskAssessmentRun' {exclude} -> exclude) (\s@StartReplicationTaskAssessmentRun' {} a -> s {exclude = a} :: StartReplicationTaskAssessmentRun) Prelude.. Lens.mapping Lens._Coerce

-- | Space-separated list of names for specific individual assessments that
-- you want to include. These names come from the default list of
-- individual assessments that AWS DMS supports for the associated
-- migration task. This task is specified by @ReplicationTaskArn@.
--
-- You can\'t set a value for @IncludeOnly@ if you also set a value for
-- @Exclude@ in the API operation.
--
-- To identify the names of the default individual assessments that AWS DMS
-- supports for the associated migration task, run the
-- @DescribeApplicableIndividualAssessments@ operation using its own
-- @ReplicationTaskArn@ request parameter.
startReplicationTaskAssessmentRun_includeOnly :: Lens.Lens' StartReplicationTaskAssessmentRun (Prelude.Maybe [Prelude.Text])
startReplicationTaskAssessmentRun_includeOnly = Lens.lens (\StartReplicationTaskAssessmentRun' {includeOnly} -> includeOnly) (\s@StartReplicationTaskAssessmentRun' {} a -> s {includeOnly = a} :: StartReplicationTaskAssessmentRun) Prelude.. Lens.mapping Lens._Coerce

-- | Folder within an Amazon S3 bucket where you want AWS DMS to store the
-- results of this assessment run.
startReplicationTaskAssessmentRun_resultLocationFolder :: Lens.Lens' StartReplicationTaskAssessmentRun (Prelude.Maybe Prelude.Text)
startReplicationTaskAssessmentRun_resultLocationFolder = Lens.lens (\StartReplicationTaskAssessmentRun' {resultLocationFolder} -> resultLocationFolder) (\s@StartReplicationTaskAssessmentRun' {} a -> s {resultLocationFolder = a} :: StartReplicationTaskAssessmentRun)

-- | Amazon Resource Name (ARN) of the migration task associated with the
-- premigration assessment run that you want to start.
startReplicationTaskAssessmentRun_replicationTaskArn :: Lens.Lens' StartReplicationTaskAssessmentRun Prelude.Text
startReplicationTaskAssessmentRun_replicationTaskArn = Lens.lens (\StartReplicationTaskAssessmentRun' {replicationTaskArn} -> replicationTaskArn) (\s@StartReplicationTaskAssessmentRun' {} a -> s {replicationTaskArn = a} :: StartReplicationTaskAssessmentRun)

-- | ARN of a service role needed to start the assessment run.
startReplicationTaskAssessmentRun_serviceAccessRoleArn :: Lens.Lens' StartReplicationTaskAssessmentRun Prelude.Text
startReplicationTaskAssessmentRun_serviceAccessRoleArn = Lens.lens (\StartReplicationTaskAssessmentRun' {serviceAccessRoleArn} -> serviceAccessRoleArn) (\s@StartReplicationTaskAssessmentRun' {} a -> s {serviceAccessRoleArn = a} :: StartReplicationTaskAssessmentRun)

-- | Amazon S3 bucket where you want AWS DMS to store the results of this
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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartReplicationTaskAssessmentRunResponse'
            Prelude.<$> (x Core..?> "ReplicationTaskAssessmentRun")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartReplicationTaskAssessmentRun

instance
  Prelude.NFData
    StartReplicationTaskAssessmentRun

instance
  Core.ToHeaders
    StartReplicationTaskAssessmentRun
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.StartReplicationTaskAssessmentRun" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    StartReplicationTaskAssessmentRun
  where
  toJSON StartReplicationTaskAssessmentRun' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ResultKmsKeyArn" Core..=)
              Prelude.<$> resultKmsKeyArn,
            ("ResultEncryptionMode" Core..=)
              Prelude.<$> resultEncryptionMode,
            ("Exclude" Core..=) Prelude.<$> exclude,
            ("IncludeOnly" Core..=) Prelude.<$> includeOnly,
            ("ResultLocationFolder" Core..=)
              Prelude.<$> resultLocationFolder,
            Prelude.Just
              ("ReplicationTaskArn" Core..= replicationTaskArn),
            Prelude.Just
              ( "ServiceAccessRoleArn"
                  Core..= serviceAccessRoleArn
              ),
            Prelude.Just
              ( "ResultLocationBucket"
                  Core..= resultLocationBucket
              ),
            Prelude.Just
              ("AssessmentRunName" Core..= assessmentRunName)
          ]
      )

instance
  Core.ToPath
    StartReplicationTaskAssessmentRun
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
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
