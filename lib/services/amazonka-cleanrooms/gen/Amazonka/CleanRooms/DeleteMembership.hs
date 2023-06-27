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
-- Module      : Amazonka.CleanRooms.DeleteMembership
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified membership. All resources under a membership must be
-- deleted.
module Amazonka.CleanRooms.DeleteMembership
  ( -- * Creating a Request
    DeleteMembership (..),
    newDeleteMembership,

    -- * Request Lenses
    deleteMembership_membershipIdentifier,

    -- * Destructuring the Response
    DeleteMembershipResponse (..),
    newDeleteMembershipResponse,

    -- * Response Lenses
    deleteMembershipResponse_httpStatus,
  )
where

import Amazonka.CleanRooms.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteMembership' smart constructor.
data DeleteMembership = DeleteMembership'
  { -- | The identifier for a membership resource.
    membershipIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'membershipIdentifier', 'deleteMembership_membershipIdentifier' - The identifier for a membership resource.
newDeleteMembership ::
  -- | 'membershipIdentifier'
  Prelude.Text ->
  DeleteMembership
newDeleteMembership pMembershipIdentifier_ =
  DeleteMembership'
    { membershipIdentifier =
        pMembershipIdentifier_
    }

-- | The identifier for a membership resource.
deleteMembership_membershipIdentifier :: Lens.Lens' DeleteMembership Prelude.Text
deleteMembership_membershipIdentifier = Lens.lens (\DeleteMembership' {membershipIdentifier} -> membershipIdentifier) (\s@DeleteMembership' {} a -> s {membershipIdentifier = a} :: DeleteMembership)

instance Core.AWSRequest DeleteMembership where
  type
    AWSResponse DeleteMembership =
      DeleteMembershipResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteMembershipResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteMembership where
  hashWithSalt _salt DeleteMembership' {..} =
    _salt `Prelude.hashWithSalt` membershipIdentifier

instance Prelude.NFData DeleteMembership where
  rnf DeleteMembership' {..} =
    Prelude.rnf membershipIdentifier

instance Data.ToHeaders DeleteMembership where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteMembership where
  toPath DeleteMembership' {..} =
    Prelude.mconcat
      ["/memberships/", Data.toBS membershipIdentifier]

instance Data.ToQuery DeleteMembership where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteMembershipResponse' smart constructor.
data DeleteMembershipResponse = DeleteMembershipResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMembershipResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteMembershipResponse_httpStatus' - The response's http status code.
newDeleteMembershipResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteMembershipResponse
newDeleteMembershipResponse pHttpStatus_ =
  DeleteMembershipResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteMembershipResponse_httpStatus :: Lens.Lens' DeleteMembershipResponse Prelude.Int
deleteMembershipResponse_httpStatus = Lens.lens (\DeleteMembershipResponse' {httpStatus} -> httpStatus) (\s@DeleteMembershipResponse' {} a -> s {httpStatus = a} :: DeleteMembershipResponse)

instance Prelude.NFData DeleteMembershipResponse where
  rnf DeleteMembershipResponse' {..} =
    Prelude.rnf httpStatus
