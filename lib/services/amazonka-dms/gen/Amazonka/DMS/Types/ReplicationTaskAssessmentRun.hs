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
-- Module      : Amazonka.DMS.Types.ReplicationTaskAssessmentRun
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.ReplicationTaskAssessmentRun where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types.ReplicationTaskAssessmentRunProgress
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information that describes a premigration assessment run that
-- you have started using the @StartReplicationTaskAssessmentRun@
-- operation.
--
-- Some of the information appears based on other operations that can
-- return the @ReplicationTaskAssessmentRun@ object.
--
-- /See:/ 'newReplicationTaskAssessmentRun' smart constructor.
data ReplicationTaskAssessmentRun = ReplicationTaskAssessmentRun'
  { -- | Indication of the completion progress for the individual assessments
    -- specified to run.
    assessmentProgress :: Prelude.Maybe ReplicationTaskAssessmentRunProgress,
    -- | Unique name of the assessment run.
    assessmentRunName :: Prelude.Maybe Prelude.Text,
    -- | Last message generated by an individual assessment failure.
    lastFailureMessage :: Prelude.Maybe Prelude.Text,
    -- | ARN of the migration task associated with this premigration assessment
    -- run.
    replicationTaskArn :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) of this assessment run.
    replicationTaskAssessmentRunArn :: Prelude.Maybe Prelude.Text,
    -- | Date on which the assessment run was created using the
    -- @StartReplicationTaskAssessmentRun@ operation.
    replicationTaskAssessmentRunCreationDate :: Prelude.Maybe Data.POSIX,
    -- | Encryption mode used to encrypt the assessment run results.
    resultEncryptionMode :: Prelude.Maybe Prelude.Text,
    -- | ARN of the KMS encryption key used to encrypt the assessment run
    -- results.
    resultKmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | Amazon S3 bucket where DMS stores the results of this assessment run.
    resultLocationBucket :: Prelude.Maybe Prelude.Text,
    -- | Folder in an Amazon S3 bucket where DMS stores the results of this
    -- assessment run.
    resultLocationFolder :: Prelude.Maybe Prelude.Text,
    -- | ARN of the service role used to start the assessment run using the
    -- @StartReplicationTaskAssessmentRun@ operation. The role must allow the
    -- @iam:PassRole@ action.
    serviceAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Assessment run status.
    --
    -- This status can have one of the following values:
    --
    -- -   @\"cancelling\"@ – The assessment run was canceled by the
    --     @CancelReplicationTaskAssessmentRun@ operation.
    --
    -- -   @\"deleting\"@ – The assessment run was deleted by the
    --     @DeleteReplicationTaskAssessmentRun@ operation.
    --
    -- -   @\"failed\"@ – At least one individual assessment completed with a
    --     @failed@ status.
    --
    -- -   @\"error-provisioning\"@ – An internal error occurred while
    --     resources were provisioned (during @provisioning@ status).
    --
    -- -   @\"error-executing\"@ – An internal error occurred while individual
    --     assessments ran (during @running@ status).
    --
    -- -   @\"invalid state\"@ – The assessment run is in an unknown state.
    --
    -- -   @\"passed\"@ – All individual assessments have completed, and none
    --     has a @failed@ status.
    --
    -- -   @\"provisioning\"@ – Resources required to run individual
    --     assessments are being provisioned.
    --
    -- -   @\"running\"@ – Individual assessments are being run.
    --
    -- -   @\"starting\"@ – The assessment run is starting, but resources are
    --     not yet being provisioned for individual assessments.
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicationTaskAssessmentRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentProgress', 'replicationTaskAssessmentRun_assessmentProgress' - Indication of the completion progress for the individual assessments
-- specified to run.
--
-- 'assessmentRunName', 'replicationTaskAssessmentRun_assessmentRunName' - Unique name of the assessment run.
--
-- 'lastFailureMessage', 'replicationTaskAssessmentRun_lastFailureMessage' - Last message generated by an individual assessment failure.
--
-- 'replicationTaskArn', 'replicationTaskAssessmentRun_replicationTaskArn' - ARN of the migration task associated with this premigration assessment
-- run.
--
-- 'replicationTaskAssessmentRunArn', 'replicationTaskAssessmentRun_replicationTaskAssessmentRunArn' - Amazon Resource Name (ARN) of this assessment run.
--
-- 'replicationTaskAssessmentRunCreationDate', 'replicationTaskAssessmentRun_replicationTaskAssessmentRunCreationDate' - Date on which the assessment run was created using the
-- @StartReplicationTaskAssessmentRun@ operation.
--
-- 'resultEncryptionMode', 'replicationTaskAssessmentRun_resultEncryptionMode' - Encryption mode used to encrypt the assessment run results.
--
-- 'resultKmsKeyArn', 'replicationTaskAssessmentRun_resultKmsKeyArn' - ARN of the KMS encryption key used to encrypt the assessment run
-- results.
--
-- 'resultLocationBucket', 'replicationTaskAssessmentRun_resultLocationBucket' - Amazon S3 bucket where DMS stores the results of this assessment run.
--
-- 'resultLocationFolder', 'replicationTaskAssessmentRun_resultLocationFolder' - Folder in an Amazon S3 bucket where DMS stores the results of this
-- assessment run.
--
-- 'serviceAccessRoleArn', 'replicationTaskAssessmentRun_serviceAccessRoleArn' - ARN of the service role used to start the assessment run using the
-- @StartReplicationTaskAssessmentRun@ operation. The role must allow the
-- @iam:PassRole@ action.
--
-- 'status', 'replicationTaskAssessmentRun_status' - Assessment run status.
--
-- This status can have one of the following values:
--
-- -   @\"cancelling\"@ – The assessment run was canceled by the
--     @CancelReplicationTaskAssessmentRun@ operation.
--
-- -   @\"deleting\"@ – The assessment run was deleted by the
--     @DeleteReplicationTaskAssessmentRun@ operation.
--
-- -   @\"failed\"@ – At least one individual assessment completed with a
--     @failed@ status.
--
-- -   @\"error-provisioning\"@ – An internal error occurred while
--     resources were provisioned (during @provisioning@ status).
--
-- -   @\"error-executing\"@ – An internal error occurred while individual
--     assessments ran (during @running@ status).
--
-- -   @\"invalid state\"@ – The assessment run is in an unknown state.
--
-- -   @\"passed\"@ – All individual assessments have completed, and none
--     has a @failed@ status.
--
-- -   @\"provisioning\"@ – Resources required to run individual
--     assessments are being provisioned.
--
-- -   @\"running\"@ – Individual assessments are being run.
--
-- -   @\"starting\"@ – The assessment run is starting, but resources are
--     not yet being provisioned for individual assessments.
newReplicationTaskAssessmentRun ::
  ReplicationTaskAssessmentRun
newReplicationTaskAssessmentRun =
  ReplicationTaskAssessmentRun'
    { assessmentProgress =
        Prelude.Nothing,
      assessmentRunName = Prelude.Nothing,
      lastFailureMessage = Prelude.Nothing,
      replicationTaskArn = Prelude.Nothing,
      replicationTaskAssessmentRunArn =
        Prelude.Nothing,
      replicationTaskAssessmentRunCreationDate =
        Prelude.Nothing,
      resultEncryptionMode = Prelude.Nothing,
      resultKmsKeyArn = Prelude.Nothing,
      resultLocationBucket = Prelude.Nothing,
      resultLocationFolder = Prelude.Nothing,
      serviceAccessRoleArn = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Indication of the completion progress for the individual assessments
-- specified to run.
replicationTaskAssessmentRun_assessmentProgress :: Lens.Lens' ReplicationTaskAssessmentRun (Prelude.Maybe ReplicationTaskAssessmentRunProgress)
replicationTaskAssessmentRun_assessmentProgress = Lens.lens (\ReplicationTaskAssessmentRun' {assessmentProgress} -> assessmentProgress) (\s@ReplicationTaskAssessmentRun' {} a -> s {assessmentProgress = a} :: ReplicationTaskAssessmentRun)

-- | Unique name of the assessment run.
replicationTaskAssessmentRun_assessmentRunName :: Lens.Lens' ReplicationTaskAssessmentRun (Prelude.Maybe Prelude.Text)
replicationTaskAssessmentRun_assessmentRunName = Lens.lens (\ReplicationTaskAssessmentRun' {assessmentRunName} -> assessmentRunName) (\s@ReplicationTaskAssessmentRun' {} a -> s {assessmentRunName = a} :: ReplicationTaskAssessmentRun)

-- | Last message generated by an individual assessment failure.
replicationTaskAssessmentRun_lastFailureMessage :: Lens.Lens' ReplicationTaskAssessmentRun (Prelude.Maybe Prelude.Text)
replicationTaskAssessmentRun_lastFailureMessage = Lens.lens (\ReplicationTaskAssessmentRun' {lastFailureMessage} -> lastFailureMessage) (\s@ReplicationTaskAssessmentRun' {} a -> s {lastFailureMessage = a} :: ReplicationTaskAssessmentRun)

-- | ARN of the migration task associated with this premigration assessment
-- run.
replicationTaskAssessmentRun_replicationTaskArn :: Lens.Lens' ReplicationTaskAssessmentRun (Prelude.Maybe Prelude.Text)
replicationTaskAssessmentRun_replicationTaskArn = Lens.lens (\ReplicationTaskAssessmentRun' {replicationTaskArn} -> replicationTaskArn) (\s@ReplicationTaskAssessmentRun' {} a -> s {replicationTaskArn = a} :: ReplicationTaskAssessmentRun)

-- | Amazon Resource Name (ARN) of this assessment run.
replicationTaskAssessmentRun_replicationTaskAssessmentRunArn :: Lens.Lens' ReplicationTaskAssessmentRun (Prelude.Maybe Prelude.Text)
replicationTaskAssessmentRun_replicationTaskAssessmentRunArn = Lens.lens (\ReplicationTaskAssessmentRun' {replicationTaskAssessmentRunArn} -> replicationTaskAssessmentRunArn) (\s@ReplicationTaskAssessmentRun' {} a -> s {replicationTaskAssessmentRunArn = a} :: ReplicationTaskAssessmentRun)

-- | Date on which the assessment run was created using the
-- @StartReplicationTaskAssessmentRun@ operation.
replicationTaskAssessmentRun_replicationTaskAssessmentRunCreationDate :: Lens.Lens' ReplicationTaskAssessmentRun (Prelude.Maybe Prelude.UTCTime)
replicationTaskAssessmentRun_replicationTaskAssessmentRunCreationDate = Lens.lens (\ReplicationTaskAssessmentRun' {replicationTaskAssessmentRunCreationDate} -> replicationTaskAssessmentRunCreationDate) (\s@ReplicationTaskAssessmentRun' {} a -> s {replicationTaskAssessmentRunCreationDate = a} :: ReplicationTaskAssessmentRun) Prelude.. Lens.mapping Data._Time

-- | Encryption mode used to encrypt the assessment run results.
replicationTaskAssessmentRun_resultEncryptionMode :: Lens.Lens' ReplicationTaskAssessmentRun (Prelude.Maybe Prelude.Text)
replicationTaskAssessmentRun_resultEncryptionMode = Lens.lens (\ReplicationTaskAssessmentRun' {resultEncryptionMode} -> resultEncryptionMode) (\s@ReplicationTaskAssessmentRun' {} a -> s {resultEncryptionMode = a} :: ReplicationTaskAssessmentRun)

-- | ARN of the KMS encryption key used to encrypt the assessment run
-- results.
replicationTaskAssessmentRun_resultKmsKeyArn :: Lens.Lens' ReplicationTaskAssessmentRun (Prelude.Maybe Prelude.Text)
replicationTaskAssessmentRun_resultKmsKeyArn = Lens.lens (\ReplicationTaskAssessmentRun' {resultKmsKeyArn} -> resultKmsKeyArn) (\s@ReplicationTaskAssessmentRun' {} a -> s {resultKmsKeyArn = a} :: ReplicationTaskAssessmentRun)

-- | Amazon S3 bucket where DMS stores the results of this assessment run.
replicationTaskAssessmentRun_resultLocationBucket :: Lens.Lens' ReplicationTaskAssessmentRun (Prelude.Maybe Prelude.Text)
replicationTaskAssessmentRun_resultLocationBucket = Lens.lens (\ReplicationTaskAssessmentRun' {resultLocationBucket} -> resultLocationBucket) (\s@ReplicationTaskAssessmentRun' {} a -> s {resultLocationBucket = a} :: ReplicationTaskAssessmentRun)

-- | Folder in an Amazon S3 bucket where DMS stores the results of this
-- assessment run.
replicationTaskAssessmentRun_resultLocationFolder :: Lens.Lens' ReplicationTaskAssessmentRun (Prelude.Maybe Prelude.Text)
replicationTaskAssessmentRun_resultLocationFolder = Lens.lens (\ReplicationTaskAssessmentRun' {resultLocationFolder} -> resultLocationFolder) (\s@ReplicationTaskAssessmentRun' {} a -> s {resultLocationFolder = a} :: ReplicationTaskAssessmentRun)

-- | ARN of the service role used to start the assessment run using the
-- @StartReplicationTaskAssessmentRun@ operation. The role must allow the
-- @iam:PassRole@ action.
replicationTaskAssessmentRun_serviceAccessRoleArn :: Lens.Lens' ReplicationTaskAssessmentRun (Prelude.Maybe Prelude.Text)
replicationTaskAssessmentRun_serviceAccessRoleArn = Lens.lens (\ReplicationTaskAssessmentRun' {serviceAccessRoleArn} -> serviceAccessRoleArn) (\s@ReplicationTaskAssessmentRun' {} a -> s {serviceAccessRoleArn = a} :: ReplicationTaskAssessmentRun)

-- | Assessment run status.
--
-- This status can have one of the following values:
--
-- -   @\"cancelling\"@ – The assessment run was canceled by the
--     @CancelReplicationTaskAssessmentRun@ operation.
--
-- -   @\"deleting\"@ – The assessment run was deleted by the
--     @DeleteReplicationTaskAssessmentRun@ operation.
--
-- -   @\"failed\"@ – At least one individual assessment completed with a
--     @failed@ status.
--
-- -   @\"error-provisioning\"@ – An internal error occurred while
--     resources were provisioned (during @provisioning@ status).
--
-- -   @\"error-executing\"@ – An internal error occurred while individual
--     assessments ran (during @running@ status).
--
-- -   @\"invalid state\"@ – The assessment run is in an unknown state.
--
-- -   @\"passed\"@ – All individual assessments have completed, and none
--     has a @failed@ status.
--
-- -   @\"provisioning\"@ – Resources required to run individual
--     assessments are being provisioned.
--
-- -   @\"running\"@ – Individual assessments are being run.
--
-- -   @\"starting\"@ – The assessment run is starting, but resources are
--     not yet being provisioned for individual assessments.
replicationTaskAssessmentRun_status :: Lens.Lens' ReplicationTaskAssessmentRun (Prelude.Maybe Prelude.Text)
replicationTaskAssessmentRun_status = Lens.lens (\ReplicationTaskAssessmentRun' {status} -> status) (\s@ReplicationTaskAssessmentRun' {} a -> s {status = a} :: ReplicationTaskAssessmentRun)

instance Data.FromJSON ReplicationTaskAssessmentRun where
  parseJSON =
    Data.withObject
      "ReplicationTaskAssessmentRun"
      ( \x ->
          ReplicationTaskAssessmentRun'
            Prelude.<$> (x Data..:? "AssessmentProgress")
            Prelude.<*> (x Data..:? "AssessmentRunName")
            Prelude.<*> (x Data..:? "LastFailureMessage")
            Prelude.<*> (x Data..:? "ReplicationTaskArn")
            Prelude.<*> (x Data..:? "ReplicationTaskAssessmentRunArn")
            Prelude.<*> ( x
                            Data..:? "ReplicationTaskAssessmentRunCreationDate"
                        )
            Prelude.<*> (x Data..:? "ResultEncryptionMode")
            Prelude.<*> (x Data..:? "ResultKmsKeyArn")
            Prelude.<*> (x Data..:? "ResultLocationBucket")
            Prelude.<*> (x Data..:? "ResultLocationFolder")
            Prelude.<*> (x Data..:? "ServiceAccessRoleArn")
            Prelude.<*> (x Data..:? "Status")
      )

instance
  Prelude.Hashable
    ReplicationTaskAssessmentRun
  where
  hashWithSalt _salt ReplicationTaskAssessmentRun' {..} =
    _salt
      `Prelude.hashWithSalt` assessmentProgress
      `Prelude.hashWithSalt` assessmentRunName
      `Prelude.hashWithSalt` lastFailureMessage
      `Prelude.hashWithSalt` replicationTaskArn
      `Prelude.hashWithSalt` replicationTaskAssessmentRunArn
      `Prelude.hashWithSalt` replicationTaskAssessmentRunCreationDate
      `Prelude.hashWithSalt` resultEncryptionMode
      `Prelude.hashWithSalt` resultKmsKeyArn
      `Prelude.hashWithSalt` resultLocationBucket
      `Prelude.hashWithSalt` resultLocationFolder
      `Prelude.hashWithSalt` serviceAccessRoleArn
      `Prelude.hashWithSalt` status

instance Prelude.NFData ReplicationTaskAssessmentRun where
  rnf ReplicationTaskAssessmentRun' {..} =
    Prelude.rnf assessmentProgress `Prelude.seq`
      Prelude.rnf assessmentRunName `Prelude.seq`
        Prelude.rnf lastFailureMessage `Prelude.seq`
          Prelude.rnf replicationTaskArn `Prelude.seq`
            Prelude.rnf replicationTaskAssessmentRunArn `Prelude.seq`
              Prelude.rnf replicationTaskAssessmentRunCreationDate `Prelude.seq`
                Prelude.rnf resultEncryptionMode `Prelude.seq`
                  Prelude.rnf resultKmsKeyArn `Prelude.seq`
                    Prelude.rnf resultLocationBucket `Prelude.seq`
                      Prelude.rnf resultLocationFolder `Prelude.seq`
                        Prelude.rnf serviceAccessRoleArn `Prelude.seq`
                          Prelude.rnf status
