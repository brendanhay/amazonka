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
-- Module      : Amazonka.KMS.RevokeGrant
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified grant. You revoke a grant to terminate the
-- permissions that the grant allows. For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/managing-grants.html#grant-delete Retiring and revoking grants>
-- in the //Key Management Service Developer Guide// .
--
-- When you create, retire, or revoke a grant, there might be a brief
-- delay, usually less than five minutes, until the grant is available
-- throughout KMS. This state is known as /eventual consistency/. For
-- details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#terms-eventual-consistency Eventual consistency>
-- in the //Key Management Service Developer Guide// .
--
-- For detailed information about grants, including grant terminology, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html Grants in KMS>
-- in the //Key Management Service Developer Guide// . For examples of
-- working with grants in several programming languages, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/programming-grants.html Programming grants>.
--
-- __Cross-account use__: Yes. To perform this operation on a KMS key in a
-- different Amazon Web Services account, specify the key ARN in the value
-- of the @KeyId@ parameter.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:RevokeGrant>
-- (key policy).
--
-- __Related operations:__
--
-- -   CreateGrant
--
-- -   ListGrants
--
-- -   ListRetirableGrants
--
-- -   RetireGrant
module Amazonka.KMS.RevokeGrant
  ( -- * Creating a Request
    RevokeGrant (..),
    newRevokeGrant,

    -- * Request Lenses
    revokeGrant_keyId,
    revokeGrant_grantId,

    -- * Destructuring the Response
    RevokeGrantResponse (..),
    newRevokeGrantResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRevokeGrant' smart constructor.
data RevokeGrant = RevokeGrant'
  { -- | A unique identifier for the KMS key associated with the grant. To get
    -- the key ID and key ARN for a KMS key, use ListKeys or DescribeKey.
    --
    -- Specify the key ID or key ARN of the KMS key. To specify a KMS key in a
    -- different Amazon Web Services account, you must use the key ARN.
    --
    -- For example:
    --
    -- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- -   Key ARN:
    --     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- To get the key ID and key ARN for a KMS key, use ListKeys or
    -- DescribeKey.
    keyId :: Prelude.Text,
    -- | Identifies the grant to revoke. To get the grant ID, use CreateGrant,
    -- ListGrants, or ListRetirableGrants.
    grantId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RevokeGrant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyId', 'revokeGrant_keyId' - A unique identifier for the KMS key associated with the grant. To get
-- the key ID and key ARN for a KMS key, use ListKeys or DescribeKey.
--
-- Specify the key ID or key ARN of the KMS key. To specify a KMS key in a
-- different Amazon Web Services account, you must use the key ARN.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- To get the key ID and key ARN for a KMS key, use ListKeys or
-- DescribeKey.
--
-- 'grantId', 'revokeGrant_grantId' - Identifies the grant to revoke. To get the grant ID, use CreateGrant,
-- ListGrants, or ListRetirableGrants.
newRevokeGrant ::
  -- | 'keyId'
  Prelude.Text ->
  -- | 'grantId'
  Prelude.Text ->
  RevokeGrant
newRevokeGrant pKeyId_ pGrantId_ =
  RevokeGrant' {keyId = pKeyId_, grantId = pGrantId_}

-- | A unique identifier for the KMS key associated with the grant. To get
-- the key ID and key ARN for a KMS key, use ListKeys or DescribeKey.
--
-- Specify the key ID or key ARN of the KMS key. To specify a KMS key in a
-- different Amazon Web Services account, you must use the key ARN.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- To get the key ID and key ARN for a KMS key, use ListKeys or
-- DescribeKey.
revokeGrant_keyId :: Lens.Lens' RevokeGrant Prelude.Text
revokeGrant_keyId = Lens.lens (\RevokeGrant' {keyId} -> keyId) (\s@RevokeGrant' {} a -> s {keyId = a} :: RevokeGrant)

-- | Identifies the grant to revoke. To get the grant ID, use CreateGrant,
-- ListGrants, or ListRetirableGrants.
revokeGrant_grantId :: Lens.Lens' RevokeGrant Prelude.Text
revokeGrant_grantId = Lens.lens (\RevokeGrant' {grantId} -> grantId) (\s@RevokeGrant' {} a -> s {grantId = a} :: RevokeGrant)

instance Core.AWSRequest RevokeGrant where
  type AWSResponse RevokeGrant = RevokeGrantResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull RevokeGrantResponse'

instance Prelude.Hashable RevokeGrant where
  hashWithSalt _salt RevokeGrant' {..} =
    _salt `Prelude.hashWithSalt` keyId
      `Prelude.hashWithSalt` grantId

instance Prelude.NFData RevokeGrant where
  rnf RevokeGrant' {..} =
    Prelude.rnf keyId `Prelude.seq` Prelude.rnf grantId

instance Core.ToHeaders RevokeGrant where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("TrentService.RevokeGrant" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RevokeGrant where
  toJSON RevokeGrant' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("KeyId" Core..= keyId),
            Prelude.Just ("GrantId" Core..= grantId)
          ]
      )

instance Core.ToPath RevokeGrant where
  toPath = Prelude.const "/"

instance Core.ToQuery RevokeGrant where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRevokeGrantResponse' smart constructor.
data RevokeGrantResponse = RevokeGrantResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RevokeGrantResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRevokeGrantResponse ::
  RevokeGrantResponse
newRevokeGrantResponse = RevokeGrantResponse'

instance Prelude.NFData RevokeGrantResponse where
  rnf _ = ()
