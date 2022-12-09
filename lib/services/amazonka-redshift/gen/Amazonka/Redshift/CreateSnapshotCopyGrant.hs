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
-- Module      : Amazonka.Redshift.CreateSnapshotCopyGrant
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot copy grant that permits Amazon Redshift to use an
-- encrypted symmetric key from Key Management Service (KMS) to encrypt
-- copied snapshots in a destination region.
--
-- For more information about managing snapshot copy grants, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-db-encryption.html Amazon Redshift Database Encryption>
-- in the /Amazon Redshift Cluster Management Guide/.
module Amazonka.Redshift.CreateSnapshotCopyGrant
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The result of the @CreateSnapshotCopyGrant@ action.
--
-- /See:/ 'newCreateSnapshotCopyGrant' smart constructor.
data CreateSnapshotCopyGrant = CreateSnapshotCopyGrant'
  { -- | The unique identifier of the encrypted symmetric key to which to grant
    -- Amazon Redshift permission. If no key is specified, the default key is
    -- used.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | A list of tag instances.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the snapshot copy grant. This name must be unique in the
    -- region for the Amazon Web Services account.
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
    -- -   Must be unique for all clusters within an Amazon Web Services
    --     account.
    snapshotCopyGrantName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSnapshotCopyGrant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'createSnapshotCopyGrant_kmsKeyId' - The unique identifier of the encrypted symmetric key to which to grant
-- Amazon Redshift permission. If no key is specified, the default key is
-- used.
--
-- 'tags', 'createSnapshotCopyGrant_tags' - A list of tag instances.
--
-- 'snapshotCopyGrantName', 'createSnapshotCopyGrant_snapshotCopyGrantName' - The name of the snapshot copy grant. This name must be unique in the
-- region for the Amazon Web Services account.
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
-- -   Must be unique for all clusters within an Amazon Web Services
--     account.
newCreateSnapshotCopyGrant ::
  -- | 'snapshotCopyGrantName'
  Prelude.Text ->
  CreateSnapshotCopyGrant
newCreateSnapshotCopyGrant pSnapshotCopyGrantName_ =
  CreateSnapshotCopyGrant'
    { kmsKeyId =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      snapshotCopyGrantName = pSnapshotCopyGrantName_
    }

-- | The unique identifier of the encrypted symmetric key to which to grant
-- Amazon Redshift permission. If no key is specified, the default key is
-- used.
createSnapshotCopyGrant_kmsKeyId :: Lens.Lens' CreateSnapshotCopyGrant (Prelude.Maybe Prelude.Text)
createSnapshotCopyGrant_kmsKeyId = Lens.lens (\CreateSnapshotCopyGrant' {kmsKeyId} -> kmsKeyId) (\s@CreateSnapshotCopyGrant' {} a -> s {kmsKeyId = a} :: CreateSnapshotCopyGrant)

-- | A list of tag instances.
createSnapshotCopyGrant_tags :: Lens.Lens' CreateSnapshotCopyGrant (Prelude.Maybe [Tag])
createSnapshotCopyGrant_tags = Lens.lens (\CreateSnapshotCopyGrant' {tags} -> tags) (\s@CreateSnapshotCopyGrant' {} a -> s {tags = a} :: CreateSnapshotCopyGrant) Prelude.. Lens.mapping Lens.coerced

-- | The name of the snapshot copy grant. This name must be unique in the
-- region for the Amazon Web Services account.
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
-- -   Must be unique for all clusters within an Amazon Web Services
--     account.
createSnapshotCopyGrant_snapshotCopyGrantName :: Lens.Lens' CreateSnapshotCopyGrant Prelude.Text
createSnapshotCopyGrant_snapshotCopyGrantName = Lens.lens (\CreateSnapshotCopyGrant' {snapshotCopyGrantName} -> snapshotCopyGrantName) (\s@CreateSnapshotCopyGrant' {} a -> s {snapshotCopyGrantName = a} :: CreateSnapshotCopyGrant)

instance Core.AWSRequest CreateSnapshotCopyGrant where
  type
    AWSResponse CreateSnapshotCopyGrant =
      CreateSnapshotCopyGrantResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateSnapshotCopyGrantResult"
      ( \s h x ->
          CreateSnapshotCopyGrantResponse'
            Prelude.<$> (x Data..@? "SnapshotCopyGrant")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSnapshotCopyGrant where
  hashWithSalt _salt CreateSnapshotCopyGrant' {..} =
    _salt `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` snapshotCopyGrantName

instance Prelude.NFData CreateSnapshotCopyGrant where
  rnf CreateSnapshotCopyGrant' {..} =
    Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf snapshotCopyGrantName

instance Data.ToHeaders CreateSnapshotCopyGrant where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateSnapshotCopyGrant where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateSnapshotCopyGrant where
  toQuery CreateSnapshotCopyGrant' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateSnapshotCopyGrant" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "KmsKeyId" Data.=: kmsKeyId,
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "Tag" Prelude.<$> tags),
        "SnapshotCopyGrantName"
          Data.=: snapshotCopyGrantName
      ]

-- | /See:/ 'newCreateSnapshotCopyGrantResponse' smart constructor.
data CreateSnapshotCopyGrantResponse = CreateSnapshotCopyGrantResponse'
  { snapshotCopyGrant :: Prelude.Maybe SnapshotCopyGrant,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateSnapshotCopyGrantResponse
newCreateSnapshotCopyGrantResponse pHttpStatus_ =
  CreateSnapshotCopyGrantResponse'
    { snapshotCopyGrant =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createSnapshotCopyGrantResponse_snapshotCopyGrant :: Lens.Lens' CreateSnapshotCopyGrantResponse (Prelude.Maybe SnapshotCopyGrant)
createSnapshotCopyGrantResponse_snapshotCopyGrant = Lens.lens (\CreateSnapshotCopyGrantResponse' {snapshotCopyGrant} -> snapshotCopyGrant) (\s@CreateSnapshotCopyGrantResponse' {} a -> s {snapshotCopyGrant = a} :: CreateSnapshotCopyGrantResponse)

-- | The response's http status code.
createSnapshotCopyGrantResponse_httpStatus :: Lens.Lens' CreateSnapshotCopyGrantResponse Prelude.Int
createSnapshotCopyGrantResponse_httpStatus = Lens.lens (\CreateSnapshotCopyGrantResponse' {httpStatus} -> httpStatus) (\s@CreateSnapshotCopyGrantResponse' {} a -> s {httpStatus = a} :: CreateSnapshotCopyGrantResponse)

instance
  Prelude.NFData
    CreateSnapshotCopyGrantResponse
  where
  rnf CreateSnapshotCopyGrantResponse' {..} =
    Prelude.rnf snapshotCopyGrant
      `Prelude.seq` Prelude.rnf httpStatus
