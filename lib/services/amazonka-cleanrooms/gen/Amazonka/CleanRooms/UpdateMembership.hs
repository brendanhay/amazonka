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
-- Module      : Amazonka.CleanRooms.UpdateMembership
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a membership.
module Amazonka.CleanRooms.UpdateMembership
  ( -- * Creating a Request
    UpdateMembership (..),
    newUpdateMembership,

    -- * Request Lenses
    updateMembership_queryLogStatus,
    updateMembership_membershipIdentifier,

    -- * Destructuring the Response
    UpdateMembershipResponse (..),
    newUpdateMembershipResponse,

    -- * Response Lenses
    updateMembershipResponse_httpStatus,
    updateMembershipResponse_membership,
  )
where

import Amazonka.CleanRooms.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateMembership' smart constructor.
data UpdateMembership = UpdateMembership'
  { -- | An indicator as to whether query logging has been enabled or disabled
    -- for the collaboration.
    queryLogStatus :: Prelude.Maybe MembershipQueryLogStatus,
    -- | The unique identifier of the membership.
    membershipIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryLogStatus', 'updateMembership_queryLogStatus' - An indicator as to whether query logging has been enabled or disabled
-- for the collaboration.
--
-- 'membershipIdentifier', 'updateMembership_membershipIdentifier' - The unique identifier of the membership.
newUpdateMembership ::
  -- | 'membershipIdentifier'
  Prelude.Text ->
  UpdateMembership
newUpdateMembership pMembershipIdentifier_ =
  UpdateMembership'
    { queryLogStatus = Prelude.Nothing,
      membershipIdentifier = pMembershipIdentifier_
    }

-- | An indicator as to whether query logging has been enabled or disabled
-- for the collaboration.
updateMembership_queryLogStatus :: Lens.Lens' UpdateMembership (Prelude.Maybe MembershipQueryLogStatus)
updateMembership_queryLogStatus = Lens.lens (\UpdateMembership' {queryLogStatus} -> queryLogStatus) (\s@UpdateMembership' {} a -> s {queryLogStatus = a} :: UpdateMembership)

-- | The unique identifier of the membership.
updateMembership_membershipIdentifier :: Lens.Lens' UpdateMembership Prelude.Text
updateMembership_membershipIdentifier = Lens.lens (\UpdateMembership' {membershipIdentifier} -> membershipIdentifier) (\s@UpdateMembership' {} a -> s {membershipIdentifier = a} :: UpdateMembership)

instance Core.AWSRequest UpdateMembership where
  type
    AWSResponse UpdateMembership =
      UpdateMembershipResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMembershipResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "membership")
      )

instance Prelude.Hashable UpdateMembership where
  hashWithSalt _salt UpdateMembership' {..} =
    _salt
      `Prelude.hashWithSalt` queryLogStatus
      `Prelude.hashWithSalt` membershipIdentifier

instance Prelude.NFData UpdateMembership where
  rnf UpdateMembership' {..} =
    Prelude.rnf queryLogStatus
      `Prelude.seq` Prelude.rnf membershipIdentifier

instance Data.ToHeaders UpdateMembership where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateMembership where
  toJSON UpdateMembership' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("queryLogStatus" Data..=)
              Prelude.<$> queryLogStatus
          ]
      )

instance Data.ToPath UpdateMembership where
  toPath UpdateMembership' {..} =
    Prelude.mconcat
      ["/memberships/", Data.toBS membershipIdentifier]

instance Data.ToQuery UpdateMembership where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateMembershipResponse' smart constructor.
data UpdateMembershipResponse = UpdateMembershipResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    membership :: Membership
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMembershipResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateMembershipResponse_httpStatus' - The response's http status code.
--
-- 'membership', 'updateMembershipResponse_membership' - Undocumented member.
newUpdateMembershipResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'membership'
  Membership ->
  UpdateMembershipResponse
newUpdateMembershipResponse pHttpStatus_ pMembership_ =
  UpdateMembershipResponse'
    { httpStatus =
        pHttpStatus_,
      membership = pMembership_
    }

-- | The response's http status code.
updateMembershipResponse_httpStatus :: Lens.Lens' UpdateMembershipResponse Prelude.Int
updateMembershipResponse_httpStatus = Lens.lens (\UpdateMembershipResponse' {httpStatus} -> httpStatus) (\s@UpdateMembershipResponse' {} a -> s {httpStatus = a} :: UpdateMembershipResponse)

-- | Undocumented member.
updateMembershipResponse_membership :: Lens.Lens' UpdateMembershipResponse Membership
updateMembershipResponse_membership = Lens.lens (\UpdateMembershipResponse' {membership} -> membership) (\s@UpdateMembershipResponse' {} a -> s {membership = a} :: UpdateMembershipResponse)

instance Prelude.NFData UpdateMembershipResponse where
  rnf UpdateMembershipResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf membership
