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
-- Module      : Amazonka.MigrationHub.AssociateCreatedArtifact
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a created artifact of an AWS cloud resource, the target
-- receiving the migration, with the migration task performed by a
-- migration tool. This API has the following traits:
--
-- -   Migration tools can call the @AssociateCreatedArtifact@ operation to
--     indicate which AWS artifact is associated with a migration task.
--
-- -   The created artifact name must be provided in ARN (Amazon Resource
--     Name) format which will contain information about type and region;
--     for example:
--     @arn:aws:ec2:us-east-1:488216288981:image\/ami-6d0ba87b@.
--
-- -   Examples of the AWS resource behind the created artifact are,
--     AMI\'s, EC2 instance, or DMS endpoint, etc.
module Amazonka.MigrationHub.AssociateCreatedArtifact
  ( -- * Creating a Request
    AssociateCreatedArtifact (..),
    newAssociateCreatedArtifact,

    -- * Request Lenses
    associateCreatedArtifact_dryRun,
    associateCreatedArtifact_progressUpdateStream,
    associateCreatedArtifact_migrationTaskName,
    associateCreatedArtifact_createdArtifact,

    -- * Destructuring the Response
    AssociateCreatedArtifactResponse (..),
    newAssociateCreatedArtifactResponse,

    -- * Response Lenses
    associateCreatedArtifactResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHub.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateCreatedArtifact' smart constructor.
data AssociateCreatedArtifact = AssociateCreatedArtifact'
  { -- | Optional boolean flag to indicate whether any effect should take place.
    -- Used to test if the caller has permission to make the call.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The name of the ProgressUpdateStream.
    progressUpdateStream :: Prelude.Text,
    -- | Unique identifier that references the migration task. /Do not store
    -- personal data in this field./
    migrationTaskName :: Prelude.Text,
    -- | An ARN of the AWS resource related to the migration (e.g., AMI, EC2
    -- instance, RDS instance, etc.)
    createdArtifact :: CreatedArtifact
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateCreatedArtifact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'associateCreatedArtifact_dryRun' - Optional boolean flag to indicate whether any effect should take place.
-- Used to test if the caller has permission to make the call.
--
-- 'progressUpdateStream', 'associateCreatedArtifact_progressUpdateStream' - The name of the ProgressUpdateStream.
--
-- 'migrationTaskName', 'associateCreatedArtifact_migrationTaskName' - Unique identifier that references the migration task. /Do not store
-- personal data in this field./
--
-- 'createdArtifact', 'associateCreatedArtifact_createdArtifact' - An ARN of the AWS resource related to the migration (e.g., AMI, EC2
-- instance, RDS instance, etc.)
newAssociateCreatedArtifact ::
  -- | 'progressUpdateStream'
  Prelude.Text ->
  -- | 'migrationTaskName'
  Prelude.Text ->
  -- | 'createdArtifact'
  CreatedArtifact ->
  AssociateCreatedArtifact
newAssociateCreatedArtifact
  pProgressUpdateStream_
  pMigrationTaskName_
  pCreatedArtifact_ =
    AssociateCreatedArtifact'
      { dryRun = Prelude.Nothing,
        progressUpdateStream = pProgressUpdateStream_,
        migrationTaskName = pMigrationTaskName_,
        createdArtifact = pCreatedArtifact_
      }

-- | Optional boolean flag to indicate whether any effect should take place.
-- Used to test if the caller has permission to make the call.
associateCreatedArtifact_dryRun :: Lens.Lens' AssociateCreatedArtifact (Prelude.Maybe Prelude.Bool)
associateCreatedArtifact_dryRun = Lens.lens (\AssociateCreatedArtifact' {dryRun} -> dryRun) (\s@AssociateCreatedArtifact' {} a -> s {dryRun = a} :: AssociateCreatedArtifact)

-- | The name of the ProgressUpdateStream.
associateCreatedArtifact_progressUpdateStream :: Lens.Lens' AssociateCreatedArtifact Prelude.Text
associateCreatedArtifact_progressUpdateStream = Lens.lens (\AssociateCreatedArtifact' {progressUpdateStream} -> progressUpdateStream) (\s@AssociateCreatedArtifact' {} a -> s {progressUpdateStream = a} :: AssociateCreatedArtifact)

-- | Unique identifier that references the migration task. /Do not store
-- personal data in this field./
associateCreatedArtifact_migrationTaskName :: Lens.Lens' AssociateCreatedArtifact Prelude.Text
associateCreatedArtifact_migrationTaskName = Lens.lens (\AssociateCreatedArtifact' {migrationTaskName} -> migrationTaskName) (\s@AssociateCreatedArtifact' {} a -> s {migrationTaskName = a} :: AssociateCreatedArtifact)

-- | An ARN of the AWS resource related to the migration (e.g., AMI, EC2
-- instance, RDS instance, etc.)
associateCreatedArtifact_createdArtifact :: Lens.Lens' AssociateCreatedArtifact CreatedArtifact
associateCreatedArtifact_createdArtifact = Lens.lens (\AssociateCreatedArtifact' {createdArtifact} -> createdArtifact) (\s@AssociateCreatedArtifact' {} a -> s {createdArtifact = a} :: AssociateCreatedArtifact)

instance Core.AWSRequest AssociateCreatedArtifact where
  type
    AWSResponse AssociateCreatedArtifact =
      AssociateCreatedArtifactResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateCreatedArtifactResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateCreatedArtifact where
  hashWithSalt _salt AssociateCreatedArtifact' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` progressUpdateStream
      `Prelude.hashWithSalt` migrationTaskName
      `Prelude.hashWithSalt` createdArtifact

instance Prelude.NFData AssociateCreatedArtifact where
  rnf AssociateCreatedArtifact' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf progressUpdateStream
      `Prelude.seq` Prelude.rnf migrationTaskName
      `Prelude.seq` Prelude.rnf createdArtifact

instance Data.ToHeaders AssociateCreatedArtifact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSMigrationHub.AssociateCreatedArtifact" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateCreatedArtifact where
  toJSON AssociateCreatedArtifact' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DryRun" Data..=) Prelude.<$> dryRun,
            Prelude.Just
              ( "ProgressUpdateStream"
                  Data..= progressUpdateStream
              ),
            Prelude.Just
              ("MigrationTaskName" Data..= migrationTaskName),
            Prelude.Just
              ("CreatedArtifact" Data..= createdArtifact)
          ]
      )

instance Data.ToPath AssociateCreatedArtifact where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateCreatedArtifact where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateCreatedArtifactResponse' smart constructor.
data AssociateCreatedArtifactResponse = AssociateCreatedArtifactResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateCreatedArtifactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateCreatedArtifactResponse_httpStatus' - The response's http status code.
newAssociateCreatedArtifactResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateCreatedArtifactResponse
newAssociateCreatedArtifactResponse pHttpStatus_ =
  AssociateCreatedArtifactResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
associateCreatedArtifactResponse_httpStatus :: Lens.Lens' AssociateCreatedArtifactResponse Prelude.Int
associateCreatedArtifactResponse_httpStatus = Lens.lens (\AssociateCreatedArtifactResponse' {httpStatus} -> httpStatus) (\s@AssociateCreatedArtifactResponse' {} a -> s {httpStatus = a} :: AssociateCreatedArtifactResponse)

instance
  Prelude.NFData
    AssociateCreatedArtifactResponse
  where
  rnf AssociateCreatedArtifactResponse' {..} =
    Prelude.rnf httpStatus
