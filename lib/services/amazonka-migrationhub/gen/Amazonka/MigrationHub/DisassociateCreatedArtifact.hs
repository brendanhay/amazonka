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
-- Module      : Amazonka.MigrationHub.DisassociateCreatedArtifact
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a created artifact of an AWS resource with a migration
-- task performed by a migration tool that was previously associated. This
-- API has the following traits:
--
-- -   A migration user can call the @DisassociateCreatedArtifacts@
--     operation to disassociate a created AWS Artifact from a migration
--     task.
--
-- -   The created artifact name must be provided in ARN (Amazon Resource
--     Name) format which will contain information about type and region;
--     for example:
--     @arn:aws:ec2:us-east-1:488216288981:image\/ami-6d0ba87b@.
--
-- -   Examples of the AWS resource behind the created artifact are,
--     AMI\'s, EC2 instance, or RDS instance, etc.
module Amazonka.MigrationHub.DisassociateCreatedArtifact
  ( -- * Creating a Request
    DisassociateCreatedArtifact (..),
    newDisassociateCreatedArtifact,

    -- * Request Lenses
    disassociateCreatedArtifact_dryRun,
    disassociateCreatedArtifact_progressUpdateStream,
    disassociateCreatedArtifact_migrationTaskName,
    disassociateCreatedArtifact_createdArtifactName,

    -- * Destructuring the Response
    DisassociateCreatedArtifactResponse (..),
    newDisassociateCreatedArtifactResponse,

    -- * Response Lenses
    disassociateCreatedArtifactResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MigrationHub.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateCreatedArtifact' smart constructor.
data DisassociateCreatedArtifact = DisassociateCreatedArtifact'
  { -- | Optional boolean flag to indicate whether any effect should take place.
    -- Used to test if the caller has permission to make the call.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The name of the ProgressUpdateStream.
    progressUpdateStream :: Prelude.Text,
    -- | Unique identifier that references the migration task to be disassociated
    -- with the artifact. /Do not store personal data in this field./
    migrationTaskName :: Prelude.Text,
    -- | An ARN of the AWS resource related to the migration (e.g., AMI, EC2
    -- instance, RDS instance, etc.)
    createdArtifactName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateCreatedArtifact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'disassociateCreatedArtifact_dryRun' - Optional boolean flag to indicate whether any effect should take place.
-- Used to test if the caller has permission to make the call.
--
-- 'progressUpdateStream', 'disassociateCreatedArtifact_progressUpdateStream' - The name of the ProgressUpdateStream.
--
-- 'migrationTaskName', 'disassociateCreatedArtifact_migrationTaskName' - Unique identifier that references the migration task to be disassociated
-- with the artifact. /Do not store personal data in this field./
--
-- 'createdArtifactName', 'disassociateCreatedArtifact_createdArtifactName' - An ARN of the AWS resource related to the migration (e.g., AMI, EC2
-- instance, RDS instance, etc.)
newDisassociateCreatedArtifact ::
  -- | 'progressUpdateStream'
  Prelude.Text ->
  -- | 'migrationTaskName'
  Prelude.Text ->
  -- | 'createdArtifactName'
  Prelude.Text ->
  DisassociateCreatedArtifact
newDisassociateCreatedArtifact
  pProgressUpdateStream_
  pMigrationTaskName_
  pCreatedArtifactName_ =
    DisassociateCreatedArtifact'
      { dryRun =
          Prelude.Nothing,
        progressUpdateStream = pProgressUpdateStream_,
        migrationTaskName = pMigrationTaskName_,
        createdArtifactName = pCreatedArtifactName_
      }

-- | Optional boolean flag to indicate whether any effect should take place.
-- Used to test if the caller has permission to make the call.
disassociateCreatedArtifact_dryRun :: Lens.Lens' DisassociateCreatedArtifact (Prelude.Maybe Prelude.Bool)
disassociateCreatedArtifact_dryRun = Lens.lens (\DisassociateCreatedArtifact' {dryRun} -> dryRun) (\s@DisassociateCreatedArtifact' {} a -> s {dryRun = a} :: DisassociateCreatedArtifact)

-- | The name of the ProgressUpdateStream.
disassociateCreatedArtifact_progressUpdateStream :: Lens.Lens' DisassociateCreatedArtifact Prelude.Text
disassociateCreatedArtifact_progressUpdateStream = Lens.lens (\DisassociateCreatedArtifact' {progressUpdateStream} -> progressUpdateStream) (\s@DisassociateCreatedArtifact' {} a -> s {progressUpdateStream = a} :: DisassociateCreatedArtifact)

-- | Unique identifier that references the migration task to be disassociated
-- with the artifact. /Do not store personal data in this field./
disassociateCreatedArtifact_migrationTaskName :: Lens.Lens' DisassociateCreatedArtifact Prelude.Text
disassociateCreatedArtifact_migrationTaskName = Lens.lens (\DisassociateCreatedArtifact' {migrationTaskName} -> migrationTaskName) (\s@DisassociateCreatedArtifact' {} a -> s {migrationTaskName = a} :: DisassociateCreatedArtifact)

-- | An ARN of the AWS resource related to the migration (e.g., AMI, EC2
-- instance, RDS instance, etc.)
disassociateCreatedArtifact_createdArtifactName :: Lens.Lens' DisassociateCreatedArtifact Prelude.Text
disassociateCreatedArtifact_createdArtifactName = Lens.lens (\DisassociateCreatedArtifact' {createdArtifactName} -> createdArtifactName) (\s@DisassociateCreatedArtifact' {} a -> s {createdArtifactName = a} :: DisassociateCreatedArtifact)

instance Core.AWSRequest DisassociateCreatedArtifact where
  type
    AWSResponse DisassociateCreatedArtifact =
      DisassociateCreatedArtifactResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateCreatedArtifactResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateCreatedArtifact where
  hashWithSalt _salt DisassociateCreatedArtifact' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` progressUpdateStream
      `Prelude.hashWithSalt` migrationTaskName
      `Prelude.hashWithSalt` createdArtifactName

instance Prelude.NFData DisassociateCreatedArtifact where
  rnf DisassociateCreatedArtifact' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf progressUpdateStream
      `Prelude.seq` Prelude.rnf migrationTaskName
      `Prelude.seq` Prelude.rnf createdArtifactName

instance Core.ToHeaders DisassociateCreatedArtifact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSMigrationHub.DisassociateCreatedArtifact" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DisassociateCreatedArtifact where
  toJSON DisassociateCreatedArtifact' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DryRun" Core..=) Prelude.<$> dryRun,
            Prelude.Just
              ( "ProgressUpdateStream"
                  Core..= progressUpdateStream
              ),
            Prelude.Just
              ("MigrationTaskName" Core..= migrationTaskName),
            Prelude.Just
              ("CreatedArtifactName" Core..= createdArtifactName)
          ]
      )

instance Core.ToPath DisassociateCreatedArtifact where
  toPath = Prelude.const "/"

instance Core.ToQuery DisassociateCreatedArtifact where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateCreatedArtifactResponse' smart constructor.
data DisassociateCreatedArtifactResponse = DisassociateCreatedArtifactResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateCreatedArtifactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateCreatedArtifactResponse_httpStatus' - The response's http status code.
newDisassociateCreatedArtifactResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateCreatedArtifactResponse
newDisassociateCreatedArtifactResponse pHttpStatus_ =
  DisassociateCreatedArtifactResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disassociateCreatedArtifactResponse_httpStatus :: Lens.Lens' DisassociateCreatedArtifactResponse Prelude.Int
disassociateCreatedArtifactResponse_httpStatus = Lens.lens (\DisassociateCreatedArtifactResponse' {httpStatus} -> httpStatus) (\s@DisassociateCreatedArtifactResponse' {} a -> s {httpStatus = a} :: DisassociateCreatedArtifactResponse)

instance
  Prelude.NFData
    DisassociateCreatedArtifactResponse
  where
  rnf DisassociateCreatedArtifactResponse' {..} =
    Prelude.rnf httpStatus
