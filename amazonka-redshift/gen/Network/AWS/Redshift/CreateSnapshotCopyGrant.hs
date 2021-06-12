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
-- Module      : Network.AWS.Redshift.CreateSnapshotCopyGrant
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot copy grant that permits Amazon Redshift to use a
-- customer master key (CMK) from AWS Key Management Service (AWS KMS) to
-- encrypt copied snapshots in a destination region.
--
-- For more information about managing snapshot copy grants, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-db-encryption.html Amazon Redshift Database Encryption>
-- in the /Amazon Redshift Cluster Management Guide/.
module Network.AWS.Redshift.CreateSnapshotCopyGrant
  ( -- * Creating a Request
    CreateSnapshotCopyGrant (..),
    newCreateSnapshotCopyGrant,

    -- * Request Lenses
    createSnapshotCopyGrant_kmsKeyId,
    createSnapshotCopyGrant_tags,
    createSnapshotCopyGrant_snapshotCopyGrantName,

    -- * Destructuring the Response
    CreateSnapshotCopyGrantResponse (..),
    newCreateSnapshotCopyGrantResponse,

    -- * Response Lenses
    createSnapshotCopyGrantResponse_snapshotCopyGrant,
    createSnapshotCopyGrantResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The result of the @CreateSnapshotCopyGrant@ action.
--
-- /See:/ 'newCreateSnapshotCopyGrant' smart constructor.
data CreateSnapshotCopyGrant = CreateSnapshotCopyGrant'
  { -- | The unique identifier of the customer master key (CMK) to which to grant
    -- Amazon Redshift permission. If no key is specified, the default key is
    -- used.
    kmsKeyId :: Core.Maybe Core.Text,
    -- | A list of tag instances.
    tags :: Core.Maybe [Tag],
    -- | The name of the snapshot copy grant. This name must be unique in the
    -- region for the AWS account.
    --
    -- Constraints:
    --
    -- -   Must contain from 1 to 63 alphanumeric characters or hyphens.
    --
    -- -   Alphabetic characters must be lowercase.
    --
    -- -   First character must be a letter.
    --
    -- -   Cannot end with a hyphen or contain two consecutive hyphens.
    --
    -- -   Must be unique for all clusters within an AWS account.
    snapshotCopyGrantName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateSnapshotCopyGrant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'createSnapshotCopyGrant_kmsKeyId' - The unique identifier of the customer master key (CMK) to which to grant
-- Amazon Redshift permission. If no key is specified, the default key is
-- used.
--
-- 'tags', 'createSnapshotCopyGrant_tags' - A list of tag instances.
--
-- 'snapshotCopyGrantName', 'createSnapshotCopyGrant_snapshotCopyGrantName' - The name of the snapshot copy grant. This name must be unique in the
-- region for the AWS account.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens.
--
-- -   Alphabetic characters must be lowercase.
--
-- -   First character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- -   Must be unique for all clusters within an AWS account.
newCreateSnapshotCopyGrant ::
  -- | 'snapshotCopyGrantName'
  Core.Text ->
  CreateSnapshotCopyGrant
newCreateSnapshotCopyGrant pSnapshotCopyGrantName_ =
  CreateSnapshotCopyGrant'
    { kmsKeyId = Core.Nothing,
      tags = Core.Nothing,
      snapshotCopyGrantName = pSnapshotCopyGrantName_
    }

-- | The unique identifier of the customer master key (CMK) to which to grant
-- Amazon Redshift permission. If no key is specified, the default key is
-- used.
createSnapshotCopyGrant_kmsKeyId :: Lens.Lens' CreateSnapshotCopyGrant (Core.Maybe Core.Text)
createSnapshotCopyGrant_kmsKeyId = Lens.lens (\CreateSnapshotCopyGrant' {kmsKeyId} -> kmsKeyId) (\s@CreateSnapshotCopyGrant' {} a -> s {kmsKeyId = a} :: CreateSnapshotCopyGrant)

-- | A list of tag instances.
createSnapshotCopyGrant_tags :: Lens.Lens' CreateSnapshotCopyGrant (Core.Maybe [Tag])
createSnapshotCopyGrant_tags = Lens.lens (\CreateSnapshotCopyGrant' {tags} -> tags) (\s@CreateSnapshotCopyGrant' {} a -> s {tags = a} :: CreateSnapshotCopyGrant) Core.. Lens.mapping Lens._Coerce

-- | The name of the snapshot copy grant. This name must be unique in the
-- region for the AWS account.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens.
--
-- -   Alphabetic characters must be lowercase.
--
-- -   First character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- -   Must be unique for all clusters within an AWS account.
createSnapshotCopyGrant_snapshotCopyGrantName :: Lens.Lens' CreateSnapshotCopyGrant Core.Text
createSnapshotCopyGrant_snapshotCopyGrantName = Lens.lens (\CreateSnapshotCopyGrant' {snapshotCopyGrantName} -> snapshotCopyGrantName) (\s@CreateSnapshotCopyGrant' {} a -> s {snapshotCopyGrantName = a} :: CreateSnapshotCopyGrant)

instance Core.AWSRequest CreateSnapshotCopyGrant where
  type
    AWSResponse CreateSnapshotCopyGrant =
      CreateSnapshotCopyGrantResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateSnapshotCopyGrantResult"
      ( \s h x ->
          CreateSnapshotCopyGrantResponse'
            Core.<$> (x Core..@? "SnapshotCopyGrant")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateSnapshotCopyGrant

instance Core.NFData CreateSnapshotCopyGrant

instance Core.ToHeaders CreateSnapshotCopyGrant where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateSnapshotCopyGrant where
  toPath = Core.const "/"

instance Core.ToQuery CreateSnapshotCopyGrant where
  toQuery CreateSnapshotCopyGrant' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CreateSnapshotCopyGrant" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "KmsKeyId" Core.=: kmsKeyId,
        "Tags"
          Core.=: Core.toQuery (Core.toQueryList "Tag" Core.<$> tags),
        "SnapshotCopyGrantName"
          Core.=: snapshotCopyGrantName
      ]

-- | /See:/ 'newCreateSnapshotCopyGrantResponse' smart constructor.
data CreateSnapshotCopyGrantResponse = CreateSnapshotCopyGrantResponse'
  { snapshotCopyGrant :: Core.Maybe SnapshotCopyGrant,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateSnapshotCopyGrantResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshotCopyGrant', 'createSnapshotCopyGrantResponse_snapshotCopyGrant' - Undocumented member.
--
-- 'httpStatus', 'createSnapshotCopyGrantResponse_httpStatus' - The response's http status code.
newCreateSnapshotCopyGrantResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateSnapshotCopyGrantResponse
newCreateSnapshotCopyGrantResponse pHttpStatus_ =
  CreateSnapshotCopyGrantResponse'
    { snapshotCopyGrant =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createSnapshotCopyGrantResponse_snapshotCopyGrant :: Lens.Lens' CreateSnapshotCopyGrantResponse (Core.Maybe SnapshotCopyGrant)
createSnapshotCopyGrantResponse_snapshotCopyGrant = Lens.lens (\CreateSnapshotCopyGrantResponse' {snapshotCopyGrant} -> snapshotCopyGrant) (\s@CreateSnapshotCopyGrantResponse' {} a -> s {snapshotCopyGrant = a} :: CreateSnapshotCopyGrantResponse)

-- | The response's http status code.
createSnapshotCopyGrantResponse_httpStatus :: Lens.Lens' CreateSnapshotCopyGrantResponse Core.Int
createSnapshotCopyGrantResponse_httpStatus = Lens.lens (\CreateSnapshotCopyGrantResponse' {httpStatus} -> httpStatus) (\s@CreateSnapshotCopyGrantResponse' {} a -> s {httpStatus = a} :: CreateSnapshotCopyGrantResponse)

instance Core.NFData CreateSnapshotCopyGrantResponse
