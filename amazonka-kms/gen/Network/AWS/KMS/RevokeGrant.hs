{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.KMS.RevokeGrant
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes the specified grant for the specified customer master key (CMK).
-- You can revoke a grant to actively deny operations that depend on it.
--
-- __Cross-account use__: Yes. To perform this operation on a CMK in a
-- different AWS account, specify the key ARN in the value of the @KeyId@
-- parameter.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:RevokeGrant>
-- (key policy)
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
module Network.AWS.KMS.RevokeGrant
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

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRevokeGrant' smart constructor.
data RevokeGrant = RevokeGrant'
  { -- | A unique identifier for the customer master key associated with the
    -- grant.
    --
    -- Specify the key ID or the Amazon Resource Name (ARN) of the CMK. To
    -- specify a CMK in a different AWS account, you must use the key ARN.
    --
    -- For example:
    --
    -- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- -   Key ARN:
    --     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- To get the key ID and key ARN for a CMK, use ListKeys or DescribeKey.
    keyId :: Prelude.Text,
    -- | Identifier of the grant to be revoked.
    grantId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RevokeGrant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyId', 'revokeGrant_keyId' - A unique identifier for the customer master key associated with the
-- grant.
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK. To
-- specify a CMK in a different AWS account, you must use the key ARN.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- To get the key ID and key ARN for a CMK, use ListKeys or DescribeKey.
--
-- 'grantId', 'revokeGrant_grantId' - Identifier of the grant to be revoked.
newRevokeGrant ::
  -- | 'keyId'
  Prelude.Text ->
  -- | 'grantId'
  Prelude.Text ->
  RevokeGrant
newRevokeGrant pKeyId_ pGrantId_ =
  RevokeGrant' {keyId = pKeyId_, grantId = pGrantId_}

-- | A unique identifier for the customer master key associated with the
-- grant.
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK. To
-- specify a CMK in a different AWS account, you must use the key ARN.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- To get the key ID and key ARN for a CMK, use ListKeys or DescribeKey.
revokeGrant_keyId :: Lens.Lens' RevokeGrant Prelude.Text
revokeGrant_keyId = Lens.lens (\RevokeGrant' {keyId} -> keyId) (\s@RevokeGrant' {} a -> s {keyId = a} :: RevokeGrant)

-- | Identifier of the grant to be revoked.
revokeGrant_grantId :: Lens.Lens' RevokeGrant Prelude.Text
revokeGrant_grantId = Lens.lens (\RevokeGrant' {grantId} -> grantId) (\s@RevokeGrant' {} a -> s {grantId = a} :: RevokeGrant)

instance Prelude.AWSRequest RevokeGrant where
  type Rs RevokeGrant = RevokeGrantResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull RevokeGrantResponse'

instance Prelude.Hashable RevokeGrant

instance Prelude.NFData RevokeGrant

instance Prelude.ToHeaders RevokeGrant where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("TrentService.RevokeGrant" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON RevokeGrant where
  toJSON RevokeGrant' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("KeyId" Prelude..= keyId),
            Prelude.Just ("GrantId" Prelude..= grantId)
          ]
      )

instance Prelude.ToPath RevokeGrant where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RevokeGrant where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRevokeGrantResponse' smart constructor.
data RevokeGrantResponse = RevokeGrantResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RevokeGrantResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRevokeGrantResponse ::
  RevokeGrantResponse
newRevokeGrantResponse = RevokeGrantResponse'

instance Prelude.NFData RevokeGrantResponse
