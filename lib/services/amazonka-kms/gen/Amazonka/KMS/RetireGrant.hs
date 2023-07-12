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
-- Module      : Amazonka.KMS.RetireGrant
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a grant. Typically, you retire a grant when you no longer need
-- its permissions. To identify the grant to retire, use a
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#grant_token grant token>,
-- or both the grant ID and a key identifier (key ID or key ARN) of the KMS
-- key. The CreateGrant operation returns both values.
--
-- This operation can be called by the /retiring principal/ for a grant, by
-- the /grantee principal/ if the grant allows the @RetireGrant@ operation,
-- and by the Amazon Web Services account in which the grant is created. It
-- can also be called by principals to whom permission for retiring a grant
-- is delegated. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grant-manage.html#grant-delete Retiring and revoking grants>
-- in the /Key Management Service Developer Guide/.
--
-- For detailed information about grants, including grant terminology, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html Grants in KMS>
-- in the //Key Management Service Developer Guide// . For examples of
-- working with grants in several programming languages, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/programming-grants.html Programming grants>.
--
-- __Cross-account use__: Yes. You can retire a grant on a KMS key in a
-- different Amazon Web Services account.
--
-- __Required permissions:__:Permission to retire a grant is determined
-- primarily by the grant. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grant-manage.html#grant-delete Retiring and revoking grants>
-- in the /Key Management Service Developer Guide/.
--
-- __Related operations:__
--
-- -   CreateGrant
--
-- -   ListGrants
--
-- -   ListRetirableGrants
--
-- -   RevokeGrant
module Amazonka.KMS.RetireGrant
  ( -- * Creating a Request
    RetireGrant (..),
    newRetireGrant,

    -- * Request Lenses
    retireGrant_grantId,
    retireGrant_grantToken,
    retireGrant_keyId,

    -- * Destructuring the Response
    RetireGrantResponse (..),
    newRetireGrantResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRetireGrant' smart constructor.
data RetireGrant = RetireGrant'
  { -- | Identifies the grant to retire. To get the grant ID, use CreateGrant,
    -- ListGrants, or ListRetirableGrants.
    --
    -- -   Grant ID Example -
    --     0123456789012345678901234567890123456789012345678901234567890123
    grantId :: Prelude.Maybe Prelude.Text,
    -- | Identifies the grant to be retired. You can use a grant token to
    -- identify a new grant even before it has achieved eventual consistency.
    --
    -- Only the CreateGrant operation returns a grant token. For details, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#grant_token Grant token>
    -- and
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#terms-eventual-consistency Eventual consistency>
    -- in the /Key Management Service Developer Guide/.
    grantToken :: Prelude.Maybe Prelude.Text,
    -- | The key ARN KMS key associated with the grant. To find the key ARN, use
    -- the ListKeys operation.
    --
    -- For example:
    -- @arn:aws:kms:us-east-2:444455556666:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
    keyId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RetireGrant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'grantId', 'retireGrant_grantId' - Identifies the grant to retire. To get the grant ID, use CreateGrant,
-- ListGrants, or ListRetirableGrants.
--
-- -   Grant ID Example -
--     0123456789012345678901234567890123456789012345678901234567890123
--
-- 'grantToken', 'retireGrant_grantToken' - Identifies the grant to be retired. You can use a grant token to
-- identify a new grant even before it has achieved eventual consistency.
--
-- Only the CreateGrant operation returns a grant token. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#grant_token Grant token>
-- and
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#terms-eventual-consistency Eventual consistency>
-- in the /Key Management Service Developer Guide/.
--
-- 'keyId', 'retireGrant_keyId' - The key ARN KMS key associated with the grant. To find the key ARN, use
-- the ListKeys operation.
--
-- For example:
-- @arn:aws:kms:us-east-2:444455556666:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
newRetireGrant ::
  RetireGrant
newRetireGrant =
  RetireGrant'
    { grantId = Prelude.Nothing,
      grantToken = Prelude.Nothing,
      keyId = Prelude.Nothing
    }

-- | Identifies the grant to retire. To get the grant ID, use CreateGrant,
-- ListGrants, or ListRetirableGrants.
--
-- -   Grant ID Example -
--     0123456789012345678901234567890123456789012345678901234567890123
retireGrant_grantId :: Lens.Lens' RetireGrant (Prelude.Maybe Prelude.Text)
retireGrant_grantId = Lens.lens (\RetireGrant' {grantId} -> grantId) (\s@RetireGrant' {} a -> s {grantId = a} :: RetireGrant)

-- | Identifies the grant to be retired. You can use a grant token to
-- identify a new grant even before it has achieved eventual consistency.
--
-- Only the CreateGrant operation returns a grant token. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#grant_token Grant token>
-- and
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#terms-eventual-consistency Eventual consistency>
-- in the /Key Management Service Developer Guide/.
retireGrant_grantToken :: Lens.Lens' RetireGrant (Prelude.Maybe Prelude.Text)
retireGrant_grantToken = Lens.lens (\RetireGrant' {grantToken} -> grantToken) (\s@RetireGrant' {} a -> s {grantToken = a} :: RetireGrant)

-- | The key ARN KMS key associated with the grant. To find the key ARN, use
-- the ListKeys operation.
--
-- For example:
-- @arn:aws:kms:us-east-2:444455556666:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
retireGrant_keyId :: Lens.Lens' RetireGrant (Prelude.Maybe Prelude.Text)
retireGrant_keyId = Lens.lens (\RetireGrant' {keyId} -> keyId) (\s@RetireGrant' {} a -> s {keyId = a} :: RetireGrant)

instance Core.AWSRequest RetireGrant where
  type AWSResponse RetireGrant = RetireGrantResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull RetireGrantResponse'

instance Prelude.Hashable RetireGrant where
  hashWithSalt _salt RetireGrant' {..} =
    _salt
      `Prelude.hashWithSalt` grantId
      `Prelude.hashWithSalt` grantToken
      `Prelude.hashWithSalt` keyId

instance Prelude.NFData RetireGrant where
  rnf RetireGrant' {..} =
    Prelude.rnf grantId
      `Prelude.seq` Prelude.rnf grantToken
      `Prelude.seq` Prelude.rnf keyId

instance Data.ToHeaders RetireGrant where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("TrentService.RetireGrant" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RetireGrant where
  toJSON RetireGrant' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GrantId" Data..=) Prelude.<$> grantId,
            ("GrantToken" Data..=) Prelude.<$> grantToken,
            ("KeyId" Data..=) Prelude.<$> keyId
          ]
      )

instance Data.ToPath RetireGrant where
  toPath = Prelude.const "/"

instance Data.ToQuery RetireGrant where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRetireGrantResponse' smart constructor.
data RetireGrantResponse = RetireGrantResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RetireGrantResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRetireGrantResponse ::
  RetireGrantResponse
newRetireGrantResponse = RetireGrantResponse'

instance Prelude.NFData RetireGrantResponse where
  rnf _ = ()
