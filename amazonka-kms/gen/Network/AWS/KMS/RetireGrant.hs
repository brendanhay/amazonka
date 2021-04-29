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
-- Module      : Network.AWS.KMS.RetireGrant
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retires a grant. To clean up, you can retire a grant when you\'re done
-- using it. You should revoke a grant when you intend to actively deny
-- operations that depend on it. The following are permitted to call this
-- API:
--
-- -   The AWS account (root user) under which the grant was created
--
-- -   The @RetiringPrincipal@, if present in the grant
--
-- -   The @GranteePrincipal@, if @RetireGrant@ is an operation specified
--     in the grant
--
-- You must identify the grant to retire by its grant token or by a
-- combination of the grant ID and the Amazon Resource Name (ARN) of the
-- customer master key (CMK). A grant token is a unique variable-length
-- base64-encoded string. A grant ID is a 64 character unique identifier of
-- a grant. The CreateGrant operation returns both.
--
-- __Cross-account use__: Yes. You can retire a grant on a CMK in a
-- different AWS account.
--
-- __Required permissions:__: Permission to retire a grant is specified in
-- the grant. You cannot control access to this operation in a policy. For
-- more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html Using grants>
-- in the /AWS Key Management Service Developer Guide/.
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
module Network.AWS.KMS.RetireGrant
  ( -- * Creating a Request
    RetireGrant (..),
    newRetireGrant,

    -- * Request Lenses
    retireGrant_grantToken,
    retireGrant_grantId,
    retireGrant_keyId,

    -- * Destructuring the Response
    RetireGrantResponse (..),
    newRetireGrantResponse,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRetireGrant' smart constructor.
data RetireGrant = RetireGrant'
  { -- | Token that identifies the grant to be retired.
    grantToken :: Prelude.Maybe Prelude.Text,
    -- | Unique identifier of the grant to retire. The grant ID is returned in
    -- the response to a @CreateGrant@ operation.
    --
    -- -   Grant ID Example -
    --     0123456789012345678901234567890123456789012345678901234567890123
    grantId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the CMK associated with the grant.
    --
    -- For example:
    -- @arn:aws:kms:us-east-2:444455556666:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
    keyId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RetireGrant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'grantToken', 'retireGrant_grantToken' - Token that identifies the grant to be retired.
--
-- 'grantId', 'retireGrant_grantId' - Unique identifier of the grant to retire. The grant ID is returned in
-- the response to a @CreateGrant@ operation.
--
-- -   Grant ID Example -
--     0123456789012345678901234567890123456789012345678901234567890123
--
-- 'keyId', 'retireGrant_keyId' - The Amazon Resource Name (ARN) of the CMK associated with the grant.
--
-- For example:
-- @arn:aws:kms:us-east-2:444455556666:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
newRetireGrant ::
  RetireGrant
newRetireGrant =
  RetireGrant'
    { grantToken = Prelude.Nothing,
      grantId = Prelude.Nothing,
      keyId = Prelude.Nothing
    }

-- | Token that identifies the grant to be retired.
retireGrant_grantToken :: Lens.Lens' RetireGrant (Prelude.Maybe Prelude.Text)
retireGrant_grantToken = Lens.lens (\RetireGrant' {grantToken} -> grantToken) (\s@RetireGrant' {} a -> s {grantToken = a} :: RetireGrant)

-- | Unique identifier of the grant to retire. The grant ID is returned in
-- the response to a @CreateGrant@ operation.
--
-- -   Grant ID Example -
--     0123456789012345678901234567890123456789012345678901234567890123
retireGrant_grantId :: Lens.Lens' RetireGrant (Prelude.Maybe Prelude.Text)
retireGrant_grantId = Lens.lens (\RetireGrant' {grantId} -> grantId) (\s@RetireGrant' {} a -> s {grantId = a} :: RetireGrant)

-- | The Amazon Resource Name (ARN) of the CMK associated with the grant.
--
-- For example:
-- @arn:aws:kms:us-east-2:444455556666:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
retireGrant_keyId :: Lens.Lens' RetireGrant (Prelude.Maybe Prelude.Text)
retireGrant_keyId = Lens.lens (\RetireGrant' {keyId} -> keyId) (\s@RetireGrant' {} a -> s {keyId = a} :: RetireGrant)

instance Prelude.AWSRequest RetireGrant where
  type Rs RetireGrant = RetireGrantResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull RetireGrantResponse'

instance Prelude.Hashable RetireGrant

instance Prelude.NFData RetireGrant

instance Prelude.ToHeaders RetireGrant where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("TrentService.RetireGrant" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON RetireGrant where
  toJSON RetireGrant' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("GrantToken" Prelude..=) Prelude.<$> grantToken,
            ("GrantId" Prelude..=) Prelude.<$> grantId,
            ("KeyId" Prelude..=) Prelude.<$> keyId
          ]
      )

instance Prelude.ToPath RetireGrant where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RetireGrant where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRetireGrantResponse' smart constructor.
data RetireGrantResponse = RetireGrantResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RetireGrantResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRetireGrantResponse ::
  RetireGrantResponse
newRetireGrantResponse = RetireGrantResponse'

instance Prelude.NFData RetireGrantResponse
