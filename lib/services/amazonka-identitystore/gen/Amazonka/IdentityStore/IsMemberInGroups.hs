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
-- Module      : Amazonka.IdentityStore.IsMemberInGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Checks the user\'s membership in all requested groups and returns if the
-- member exists in all queried groups.
module Amazonka.IdentityStore.IsMemberInGroups
  ( -- * Creating a Request
    IsMemberInGroups (..),
    newIsMemberInGroups,

    -- * Request Lenses
    isMemberInGroups_identityStoreId,
    isMemberInGroups_memberId,
    isMemberInGroups_groupIds,

    -- * Destructuring the Response
    IsMemberInGroupsResponse (..),
    newIsMemberInGroupsResponse,

    -- * Response Lenses
    isMemberInGroupsResponse_httpStatus,
    isMemberInGroupsResponse_results,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IdentityStore.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newIsMemberInGroups' smart constructor.
data IsMemberInGroups = IsMemberInGroups'
  { -- | The globally unique identifier for the identity store.
    identityStoreId :: Prelude.Text,
    -- | An object containing the identifier of a group member.
    memberId :: MemberId,
    -- | A list of identifiers for groups in the identity store.
    groupIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IsMemberInGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityStoreId', 'isMemberInGroups_identityStoreId' - The globally unique identifier for the identity store.
--
-- 'memberId', 'isMemberInGroups_memberId' - An object containing the identifier of a group member.
--
-- 'groupIds', 'isMemberInGroups_groupIds' - A list of identifiers for groups in the identity store.
newIsMemberInGroups ::
  -- | 'identityStoreId'
  Prelude.Text ->
  -- | 'memberId'
  MemberId ->
  -- | 'groupIds'
  Prelude.NonEmpty Prelude.Text ->
  IsMemberInGroups
newIsMemberInGroups
  pIdentityStoreId_
  pMemberId_
  pGroupIds_ =
    IsMemberInGroups'
      { identityStoreId =
          pIdentityStoreId_,
        memberId = pMemberId_,
        groupIds = Lens.coerced Lens.# pGroupIds_
      }

-- | The globally unique identifier for the identity store.
isMemberInGroups_identityStoreId :: Lens.Lens' IsMemberInGroups Prelude.Text
isMemberInGroups_identityStoreId = Lens.lens (\IsMemberInGroups' {identityStoreId} -> identityStoreId) (\s@IsMemberInGroups' {} a -> s {identityStoreId = a} :: IsMemberInGroups)

-- | An object containing the identifier of a group member.
isMemberInGroups_memberId :: Lens.Lens' IsMemberInGroups MemberId
isMemberInGroups_memberId = Lens.lens (\IsMemberInGroups' {memberId} -> memberId) (\s@IsMemberInGroups' {} a -> s {memberId = a} :: IsMemberInGroups)

-- | A list of identifiers for groups in the identity store.
isMemberInGroups_groupIds :: Lens.Lens' IsMemberInGroups (Prelude.NonEmpty Prelude.Text)
isMemberInGroups_groupIds = Lens.lens (\IsMemberInGroups' {groupIds} -> groupIds) (\s@IsMemberInGroups' {} a -> s {groupIds = a} :: IsMemberInGroups) Prelude.. Lens.coerced

instance Core.AWSRequest IsMemberInGroups where
  type
    AWSResponse IsMemberInGroups =
      IsMemberInGroupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          IsMemberInGroupsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "Results" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable IsMemberInGroups where
  hashWithSalt _salt IsMemberInGroups' {..} =
    _salt `Prelude.hashWithSalt` identityStoreId
      `Prelude.hashWithSalt` memberId
      `Prelude.hashWithSalt` groupIds

instance Prelude.NFData IsMemberInGroups where
  rnf IsMemberInGroups' {..} =
    Prelude.rnf identityStoreId
      `Prelude.seq` Prelude.rnf memberId
      `Prelude.seq` Prelude.rnf groupIds

instance Core.ToHeaders IsMemberInGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSIdentityStore.IsMemberInGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON IsMemberInGroups where
  toJSON IsMemberInGroups' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("IdentityStoreId" Core..= identityStoreId),
            Prelude.Just ("MemberId" Core..= memberId),
            Prelude.Just ("GroupIds" Core..= groupIds)
          ]
      )

instance Core.ToPath IsMemberInGroups where
  toPath = Prelude.const "/"

instance Core.ToQuery IsMemberInGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newIsMemberInGroupsResponse' smart constructor.
data IsMemberInGroupsResponse = IsMemberInGroupsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list containing the results of membership existence checks.
    results :: [GroupMembershipExistenceResult]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IsMemberInGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'isMemberInGroupsResponse_httpStatus' - The response's http status code.
--
-- 'results', 'isMemberInGroupsResponse_results' - A list containing the results of membership existence checks.
newIsMemberInGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  IsMemberInGroupsResponse
newIsMemberInGroupsResponse pHttpStatus_ =
  IsMemberInGroupsResponse'
    { httpStatus =
        pHttpStatus_,
      results = Prelude.mempty
    }

-- | The response's http status code.
isMemberInGroupsResponse_httpStatus :: Lens.Lens' IsMemberInGroupsResponse Prelude.Int
isMemberInGroupsResponse_httpStatus = Lens.lens (\IsMemberInGroupsResponse' {httpStatus} -> httpStatus) (\s@IsMemberInGroupsResponse' {} a -> s {httpStatus = a} :: IsMemberInGroupsResponse)

-- | A list containing the results of membership existence checks.
isMemberInGroupsResponse_results :: Lens.Lens' IsMemberInGroupsResponse [GroupMembershipExistenceResult]
isMemberInGroupsResponse_results = Lens.lens (\IsMemberInGroupsResponse' {results} -> results) (\s@IsMemberInGroupsResponse' {} a -> s {results = a} :: IsMemberInGroupsResponse) Prelude.. Lens.coerced

instance Prelude.NFData IsMemberInGroupsResponse where
  rnf IsMemberInGroupsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf results
