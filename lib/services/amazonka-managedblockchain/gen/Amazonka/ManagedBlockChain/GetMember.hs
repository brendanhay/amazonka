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
-- Module      : Amazonka.ManagedBlockChain.GetMember
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed information about a member.
--
-- Applies only to Hyperledger Fabric.
module Amazonka.ManagedBlockChain.GetMember
  ( -- * Creating a Request
    GetMember (..),
    newGetMember,

    -- * Request Lenses
    getMember_networkId,
    getMember_memberId,

    -- * Destructuring the Response
    GetMemberResponse (..),
    newGetMemberResponse,

    -- * Response Lenses
    getMemberResponse_member,
    getMemberResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ManagedBlockChain.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetMember' smart constructor.
data GetMember = GetMember'
  { -- | The unique identifier of the network to which the member belongs.
    networkId :: Prelude.Text,
    -- | The unique identifier of the member.
    memberId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMember' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkId', 'getMember_networkId' - The unique identifier of the network to which the member belongs.
--
-- 'memberId', 'getMember_memberId' - The unique identifier of the member.
newGetMember ::
  -- | 'networkId'
  Prelude.Text ->
  -- | 'memberId'
  Prelude.Text ->
  GetMember
newGetMember pNetworkId_ pMemberId_ =
  GetMember'
    { networkId = pNetworkId_,
      memberId = pMemberId_
    }

-- | The unique identifier of the network to which the member belongs.
getMember_networkId :: Lens.Lens' GetMember Prelude.Text
getMember_networkId = Lens.lens (\GetMember' {networkId} -> networkId) (\s@GetMember' {} a -> s {networkId = a} :: GetMember)

-- | The unique identifier of the member.
getMember_memberId :: Lens.Lens' GetMember Prelude.Text
getMember_memberId = Lens.lens (\GetMember' {memberId} -> memberId) (\s@GetMember' {} a -> s {memberId = a} :: GetMember)

instance Core.AWSRequest GetMember where
  type AWSResponse GetMember = GetMemberResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMemberResponse'
            Prelude.<$> (x Data..?> "Member")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetMember where
  hashWithSalt _salt GetMember' {..} =
    _salt
      `Prelude.hashWithSalt` networkId
      `Prelude.hashWithSalt` memberId

instance Prelude.NFData GetMember where
  rnf GetMember' {..} =
    Prelude.rnf networkId
      `Prelude.seq` Prelude.rnf memberId

instance Data.ToHeaders GetMember where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetMember where
  toPath GetMember' {..} =
    Prelude.mconcat
      [ "/networks/",
        Data.toBS networkId,
        "/members/",
        Data.toBS memberId
      ]

instance Data.ToQuery GetMember where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMemberResponse' smart constructor.
data GetMemberResponse = GetMemberResponse'
  { -- | The properties of a member.
    member :: Prelude.Maybe Member,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMemberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'member', 'getMemberResponse_member' - The properties of a member.
--
-- 'httpStatus', 'getMemberResponse_httpStatus' - The response's http status code.
newGetMemberResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMemberResponse
newGetMemberResponse pHttpStatus_ =
  GetMemberResponse'
    { member = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The properties of a member.
getMemberResponse_member :: Lens.Lens' GetMemberResponse (Prelude.Maybe Member)
getMemberResponse_member = Lens.lens (\GetMemberResponse' {member} -> member) (\s@GetMemberResponse' {} a -> s {member = a} :: GetMemberResponse)

-- | The response's http status code.
getMemberResponse_httpStatus :: Lens.Lens' GetMemberResponse Prelude.Int
getMemberResponse_httpStatus = Lens.lens (\GetMemberResponse' {httpStatus} -> httpStatus) (\s@GetMemberResponse' {} a -> s {httpStatus = a} :: GetMemberResponse)

instance Prelude.NFData GetMemberResponse where
  rnf GetMemberResponse' {..} =
    Prelude.rnf member
      `Prelude.seq` Prelude.rnf httpStatus
