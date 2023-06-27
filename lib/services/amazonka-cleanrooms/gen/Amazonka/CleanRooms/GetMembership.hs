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
-- Module      : Amazonka.CleanRooms.GetMembership
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a specified membership for an identifier.
module Amazonka.CleanRooms.GetMembership
  ( -- * Creating a Request
    GetMembership (..),
    newGetMembership,

    -- * Request Lenses
    getMembership_membershipIdentifier,

    -- * Destructuring the Response
    GetMembershipResponse (..),
    newGetMembershipResponse,

    -- * Response Lenses
    getMembershipResponse_httpStatus,
    getMembershipResponse_membership,
  )
where

import Amazonka.CleanRooms.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetMembership' smart constructor.
data GetMembership = GetMembership'
  { -- | The identifier for a membership resource.
    membershipIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'membershipIdentifier', 'getMembership_membershipIdentifier' - The identifier for a membership resource.
newGetMembership ::
  -- | 'membershipIdentifier'
  Prelude.Text ->
  GetMembership
newGetMembership pMembershipIdentifier_ =
  GetMembership'
    { membershipIdentifier =
        pMembershipIdentifier_
    }

-- | The identifier for a membership resource.
getMembership_membershipIdentifier :: Lens.Lens' GetMembership Prelude.Text
getMembership_membershipIdentifier = Lens.lens (\GetMembership' {membershipIdentifier} -> membershipIdentifier) (\s@GetMembership' {} a -> s {membershipIdentifier = a} :: GetMembership)

instance Core.AWSRequest GetMembership where
  type
    AWSResponse GetMembership =
      GetMembershipResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMembershipResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "membership")
      )

instance Prelude.Hashable GetMembership where
  hashWithSalt _salt GetMembership' {..} =
    _salt `Prelude.hashWithSalt` membershipIdentifier

instance Prelude.NFData GetMembership where
  rnf GetMembership' {..} =
    Prelude.rnf membershipIdentifier

instance Data.ToHeaders GetMembership where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetMembership where
  toPath GetMembership' {..} =
    Prelude.mconcat
      ["/memberships/", Data.toBS membershipIdentifier]

instance Data.ToQuery GetMembership where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMembershipResponse' smart constructor.
data GetMembershipResponse = GetMembershipResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The membership retrieved for the provided identifier.
    membership :: Membership
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMembershipResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getMembershipResponse_httpStatus' - The response's http status code.
--
-- 'membership', 'getMembershipResponse_membership' - The membership retrieved for the provided identifier.
newGetMembershipResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'membership'
  Membership ->
  GetMembershipResponse
newGetMembershipResponse pHttpStatus_ pMembership_ =
  GetMembershipResponse'
    { httpStatus = pHttpStatus_,
      membership = pMembership_
    }

-- | The response's http status code.
getMembershipResponse_httpStatus :: Lens.Lens' GetMembershipResponse Prelude.Int
getMembershipResponse_httpStatus = Lens.lens (\GetMembershipResponse' {httpStatus} -> httpStatus) (\s@GetMembershipResponse' {} a -> s {httpStatus = a} :: GetMembershipResponse)

-- | The membership retrieved for the provided identifier.
getMembershipResponse_membership :: Lens.Lens' GetMembershipResponse Membership
getMembershipResponse_membership = Lens.lens (\GetMembershipResponse' {membership} -> membership) (\s@GetMembershipResponse' {} a -> s {membership = a} :: GetMembershipResponse)

instance Prelude.NFData GetMembershipResponse where
  rnf GetMembershipResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf membership
