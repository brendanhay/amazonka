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
-- Module      : Amazonka.ManagedBlockChain.CreateMember
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a member within a Managed Blockchain network.
--
-- Applies only to Hyperledger Fabric.
module Amazonka.ManagedBlockChain.CreateMember
  ( -- * Creating a Request
    CreateMember (..),
    newCreateMember,

    -- * Request Lenses
    createMember_clientRequestToken,
    createMember_invitationId,
    createMember_networkId,
    createMember_memberConfiguration,

    -- * Destructuring the Response
    CreateMemberResponse (..),
    newCreateMemberResponse,

    -- * Response Lenses
    createMemberResponse_memberId,
    createMemberResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ManagedBlockChain.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateMember' smart constructor.
data CreateMember = CreateMember'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the operation. An idempotent operation completes no more
    -- than one time. This identifier is required only if you make a service
    -- request directly using an HTTP client. It is generated automatically if
    -- you use an Amazon Web Services SDK or the CLI.
    clientRequestToken :: Prelude.Text,
    -- | The unique identifier of the invitation that is sent to the member to
    -- join the network.
    invitationId :: Prelude.Text,
    -- | The unique identifier of the network in which the member is created.
    networkId :: Prelude.Text,
    -- | Member configuration parameters.
    memberConfiguration :: MemberConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMember' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'createMember_clientRequestToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the operation. An idempotent operation completes no more
-- than one time. This identifier is required only if you make a service
-- request directly using an HTTP client. It is generated automatically if
-- you use an Amazon Web Services SDK or the CLI.
--
-- 'invitationId', 'createMember_invitationId' - The unique identifier of the invitation that is sent to the member to
-- join the network.
--
-- 'networkId', 'createMember_networkId' - The unique identifier of the network in which the member is created.
--
-- 'memberConfiguration', 'createMember_memberConfiguration' - Member configuration parameters.
newCreateMember ::
  -- | 'clientRequestToken'
  Prelude.Text ->
  -- | 'invitationId'
  Prelude.Text ->
  -- | 'networkId'
  Prelude.Text ->
  -- | 'memberConfiguration'
  MemberConfiguration ->
  CreateMember
newCreateMember
  pClientRequestToken_
  pInvitationId_
  pNetworkId_
  pMemberConfiguration_ =
    CreateMember'
      { clientRequestToken =
          pClientRequestToken_,
        invitationId = pInvitationId_,
        networkId = pNetworkId_,
        memberConfiguration = pMemberConfiguration_
      }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the operation. An idempotent operation completes no more
-- than one time. This identifier is required only if you make a service
-- request directly using an HTTP client. It is generated automatically if
-- you use an Amazon Web Services SDK or the CLI.
createMember_clientRequestToken :: Lens.Lens' CreateMember Prelude.Text
createMember_clientRequestToken = Lens.lens (\CreateMember' {clientRequestToken} -> clientRequestToken) (\s@CreateMember' {} a -> s {clientRequestToken = a} :: CreateMember)

-- | The unique identifier of the invitation that is sent to the member to
-- join the network.
createMember_invitationId :: Lens.Lens' CreateMember Prelude.Text
createMember_invitationId = Lens.lens (\CreateMember' {invitationId} -> invitationId) (\s@CreateMember' {} a -> s {invitationId = a} :: CreateMember)

-- | The unique identifier of the network in which the member is created.
createMember_networkId :: Lens.Lens' CreateMember Prelude.Text
createMember_networkId = Lens.lens (\CreateMember' {networkId} -> networkId) (\s@CreateMember' {} a -> s {networkId = a} :: CreateMember)

-- | Member configuration parameters.
createMember_memberConfiguration :: Lens.Lens' CreateMember MemberConfiguration
createMember_memberConfiguration = Lens.lens (\CreateMember' {memberConfiguration} -> memberConfiguration) (\s@CreateMember' {} a -> s {memberConfiguration = a} :: CreateMember)

instance Core.AWSRequest CreateMember where
  type AWSResponse CreateMember = CreateMemberResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMemberResponse'
            Prelude.<$> (x Core..?> "MemberId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateMember where
  hashWithSalt _salt CreateMember' {..} =
    _salt `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` invitationId
      `Prelude.hashWithSalt` networkId
      `Prelude.hashWithSalt` memberConfiguration

instance Prelude.NFData CreateMember where
  rnf CreateMember' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf invitationId
      `Prelude.seq` Prelude.rnf networkId
      `Prelude.seq` Prelude.rnf memberConfiguration

instance Core.ToHeaders CreateMember where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateMember where
  toJSON CreateMember' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ClientRequestToken" Core..= clientRequestToken),
            Prelude.Just ("InvitationId" Core..= invitationId),
            Prelude.Just
              ("MemberConfiguration" Core..= memberConfiguration)
          ]
      )

instance Core.ToPath CreateMember where
  toPath CreateMember' {..} =
    Prelude.mconcat
      ["/networks/", Core.toBS networkId, "/members"]

instance Core.ToQuery CreateMember where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateMemberResponse' smart constructor.
data CreateMemberResponse = CreateMemberResponse'
  { -- | The unique identifier of the member.
    memberId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMemberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'memberId', 'createMemberResponse_memberId' - The unique identifier of the member.
--
-- 'httpStatus', 'createMemberResponse_httpStatus' - The response's http status code.
newCreateMemberResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateMemberResponse
newCreateMemberResponse pHttpStatus_ =
  CreateMemberResponse'
    { memberId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier of the member.
createMemberResponse_memberId :: Lens.Lens' CreateMemberResponse (Prelude.Maybe Prelude.Text)
createMemberResponse_memberId = Lens.lens (\CreateMemberResponse' {memberId} -> memberId) (\s@CreateMemberResponse' {} a -> s {memberId = a} :: CreateMemberResponse)

-- | The response's http status code.
createMemberResponse_httpStatus :: Lens.Lens' CreateMemberResponse Prelude.Int
createMemberResponse_httpStatus = Lens.lens (\CreateMemberResponse' {httpStatus} -> httpStatus) (\s@CreateMemberResponse' {} a -> s {httpStatus = a} :: CreateMemberResponse)

instance Prelude.NFData CreateMemberResponse where
  rnf CreateMemberResponse' {..} =
    Prelude.rnf memberId
      `Prelude.seq` Prelude.rnf httpStatus
