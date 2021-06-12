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
-- Module      : Network.AWS.MigrationHub.AssociateCreatedArtifact
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.MigrationHub.AssociateCreatedArtifact
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateCreatedArtifact' smart constructor.
data AssociateCreatedArtifact = AssociateCreatedArtifact'
  { -- | Optional boolean flag to indicate whether any effect should take place.
    -- Used to test if the caller has permission to make the call.
    dryRun :: Core.Maybe Core.Bool,
    -- | The name of the ProgressUpdateStream.
    progressUpdateStream :: Core.Text,
    -- | Unique identifier that references the migration task. /Do not store
    -- personal data in this field./
    migrationTaskName :: Core.Text,
    -- | An ARN of the AWS resource related to the migration (e.g., AMI, EC2
    -- instance, RDS instance, etc.)
    createdArtifact :: CreatedArtifact
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'migrationTaskName'
  Core.Text ->
  -- | 'createdArtifact'
  CreatedArtifact ->
  AssociateCreatedArtifact
newAssociateCreatedArtifact
  pProgressUpdateStream_
  pMigrationTaskName_
  pCreatedArtifact_ =
    AssociateCreatedArtifact'
      { dryRun = Core.Nothing,
        progressUpdateStream = pProgressUpdateStream_,
        migrationTaskName = pMigrationTaskName_,
        createdArtifact = pCreatedArtifact_
      }

-- | Optional boolean flag to indicate whether any effect should take place.
-- Used to test if the caller has permission to make the call.
associateCreatedArtifact_dryRun :: Lens.Lens' AssociateCreatedArtifact (Core.Maybe Core.Bool)
associateCreatedArtifact_dryRun = Lens.lens (\AssociateCreatedArtifact' {dryRun} -> dryRun) (\s@AssociateCreatedArtifact' {} a -> s {dryRun = a} :: AssociateCreatedArtifact)

-- | The name of the ProgressUpdateStream.
associateCreatedArtifact_progressUpdateStream :: Lens.Lens' AssociateCreatedArtifact Core.Text
associateCreatedArtifact_progressUpdateStream = Lens.lens (\AssociateCreatedArtifact' {progressUpdateStream} -> progressUpdateStream) (\s@AssociateCreatedArtifact' {} a -> s {progressUpdateStream = a} :: AssociateCreatedArtifact)

-- | Unique identifier that references the migration task. /Do not store
-- personal data in this field./
associateCreatedArtifact_migrationTaskName :: Lens.Lens' AssociateCreatedArtifact Core.Text
associateCreatedArtifact_migrationTaskName = Lens.lens (\AssociateCreatedArtifact' {migrationTaskName} -> migrationTaskName) (\s@AssociateCreatedArtifact' {} a -> s {migrationTaskName = a} :: AssociateCreatedArtifact)

-- | An ARN of the AWS resource related to the migration (e.g., AMI, EC2
-- instance, RDS instance, etc.)
associateCreatedArtifact_createdArtifact :: Lens.Lens' AssociateCreatedArtifact CreatedArtifact
associateCreatedArtifact_createdArtifact = Lens.lens (\AssociateCreatedArtifact' {createdArtifact} -> createdArtifact) (\s@AssociateCreatedArtifact' {} a -> s {createdArtifact = a} :: AssociateCreatedArtifact)

instance Core.AWSRequest AssociateCreatedArtifact where
  type
    AWSResponse AssociateCreatedArtifact =
      AssociateCreatedArtifactResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateCreatedArtifactResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AssociateCreatedArtifact

instance Core.NFData AssociateCreatedArtifact

instance Core.ToHeaders AssociateCreatedArtifact where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSMigrationHub.AssociateCreatedArtifact" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AssociateCreatedArtifact where
  toJSON AssociateCreatedArtifact' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DryRun" Core..=) Core.<$> dryRun,
            Core.Just
              ( "ProgressUpdateStream"
                  Core..= progressUpdateStream
              ),
            Core.Just
              ("MigrationTaskName" Core..= migrationTaskName),
            Core.Just
              ("CreatedArtifact" Core..= createdArtifact)
          ]
      )

instance Core.ToPath AssociateCreatedArtifact where
  toPath = Core.const "/"

instance Core.ToQuery AssociateCreatedArtifact where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAssociateCreatedArtifactResponse' smart constructor.
data AssociateCreatedArtifactResponse = AssociateCreatedArtifactResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  AssociateCreatedArtifactResponse
newAssociateCreatedArtifactResponse pHttpStatus_ =
  AssociateCreatedArtifactResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
associateCreatedArtifactResponse_httpStatus :: Lens.Lens' AssociateCreatedArtifactResponse Core.Int
associateCreatedArtifactResponse_httpStatus = Lens.lens (\AssociateCreatedArtifactResponse' {httpStatus} -> httpStatus) (\s@AssociateCreatedArtifactResponse' {} a -> s {httpStatus = a} :: AssociateCreatedArtifactResponse)

instance Core.NFData AssociateCreatedArtifactResponse
