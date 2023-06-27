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
-- Module      : Amazonka.CleanRooms.CreateMembership
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a membership for a specific collaboration identifier and joins
-- the collaboration.
module Amazonka.CleanRooms.CreateMembership
  ( -- * Creating a Request
    CreateMembership (..),
    newCreateMembership,

    -- * Request Lenses
    createMembership_tags,
    createMembership_collaborationIdentifier,
    createMembership_queryLogStatus,

    -- * Destructuring the Response
    CreateMembershipResponse (..),
    newCreateMembershipResponse,

    -- * Response Lenses
    createMembershipResponse_httpStatus,
    createMembershipResponse_membership,
  )
where

import Amazonka.CleanRooms.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateMembership' smart constructor.
data CreateMembership = CreateMembership'
  { -- | An optional label that you can assign to a resource when you create it.
    -- Each tag consists of a key and an optional value, both of which you
    -- define. When you use tagging, you can also use tag-based access control
    -- in IAM policies to control access to this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The unique ID for the associated collaboration.
    collaborationIdentifier :: Prelude.Text,
    -- | An indicator as to whether query logging has been enabled or disabled
    -- for the collaboration.
    queryLogStatus :: MembershipQueryLogStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createMembership_tags' - An optional label that you can assign to a resource when you create it.
-- Each tag consists of a key and an optional value, both of which you
-- define. When you use tagging, you can also use tag-based access control
-- in IAM policies to control access to this resource.
--
-- 'collaborationIdentifier', 'createMembership_collaborationIdentifier' - The unique ID for the associated collaboration.
--
-- 'queryLogStatus', 'createMembership_queryLogStatus' - An indicator as to whether query logging has been enabled or disabled
-- for the collaboration.
newCreateMembership ::
  -- | 'collaborationIdentifier'
  Prelude.Text ->
  -- | 'queryLogStatus'
  MembershipQueryLogStatus ->
  CreateMembership
newCreateMembership
  pCollaborationIdentifier_
  pQueryLogStatus_ =
    CreateMembership'
      { tags = Prelude.Nothing,
        collaborationIdentifier = pCollaborationIdentifier_,
        queryLogStatus = pQueryLogStatus_
      }

-- | An optional label that you can assign to a resource when you create it.
-- Each tag consists of a key and an optional value, both of which you
-- define. When you use tagging, you can also use tag-based access control
-- in IAM policies to control access to this resource.
createMembership_tags :: Lens.Lens' CreateMembership (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createMembership_tags = Lens.lens (\CreateMembership' {tags} -> tags) (\s@CreateMembership' {} a -> s {tags = a} :: CreateMembership) Prelude.. Lens.mapping Lens.coerced

-- | The unique ID for the associated collaboration.
createMembership_collaborationIdentifier :: Lens.Lens' CreateMembership Prelude.Text
createMembership_collaborationIdentifier = Lens.lens (\CreateMembership' {collaborationIdentifier} -> collaborationIdentifier) (\s@CreateMembership' {} a -> s {collaborationIdentifier = a} :: CreateMembership)

-- | An indicator as to whether query logging has been enabled or disabled
-- for the collaboration.
createMembership_queryLogStatus :: Lens.Lens' CreateMembership MembershipQueryLogStatus
createMembership_queryLogStatus = Lens.lens (\CreateMembership' {queryLogStatus} -> queryLogStatus) (\s@CreateMembership' {} a -> s {queryLogStatus = a} :: CreateMembership)

instance Core.AWSRequest CreateMembership where
  type
    AWSResponse CreateMembership =
      CreateMembershipResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMembershipResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "membership")
      )

instance Prelude.Hashable CreateMembership where
  hashWithSalt _salt CreateMembership' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` collaborationIdentifier
      `Prelude.hashWithSalt` queryLogStatus

instance Prelude.NFData CreateMembership where
  rnf CreateMembership' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf collaborationIdentifier
      `Prelude.seq` Prelude.rnf queryLogStatus

instance Data.ToHeaders CreateMembership where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateMembership where
  toJSON CreateMembership' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ( "collaborationIdentifier"
                  Data..= collaborationIdentifier
              ),
            Prelude.Just
              ("queryLogStatus" Data..= queryLogStatus)
          ]
      )

instance Data.ToPath CreateMembership where
  toPath = Prelude.const "/memberships"

instance Data.ToQuery CreateMembership where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateMembershipResponse' smart constructor.
data CreateMembershipResponse = CreateMembershipResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The membership that was created.
    membership :: Membership
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMembershipResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createMembershipResponse_httpStatus' - The response's http status code.
--
-- 'membership', 'createMembershipResponse_membership' - The membership that was created.
newCreateMembershipResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'membership'
  Membership ->
  CreateMembershipResponse
newCreateMembershipResponse pHttpStatus_ pMembership_ =
  CreateMembershipResponse'
    { httpStatus =
        pHttpStatus_,
      membership = pMembership_
    }

-- | The response's http status code.
createMembershipResponse_httpStatus :: Lens.Lens' CreateMembershipResponse Prelude.Int
createMembershipResponse_httpStatus = Lens.lens (\CreateMembershipResponse' {httpStatus} -> httpStatus) (\s@CreateMembershipResponse' {} a -> s {httpStatus = a} :: CreateMembershipResponse)

-- | The membership that was created.
createMembershipResponse_membership :: Lens.Lens' CreateMembershipResponse Membership
createMembershipResponse_membership = Lens.lens (\CreateMembershipResponse' {membership} -> membership) (\s@CreateMembershipResponse' {} a -> s {membership = a} :: CreateMembershipResponse)

instance Prelude.NFData CreateMembershipResponse where
  rnf CreateMembershipResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf membership
