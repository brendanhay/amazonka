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
-- Module      : Amazonka.Route53Resolver.DeleteFirewallDomainList
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified domain list.
module Amazonka.Route53Resolver.DeleteFirewallDomainList
  ( -- * Creating a Request
    DeleteFirewallDomainList (..),
    newDeleteFirewallDomainList,

    -- * Request Lenses
    deleteFirewallDomainList_firewallDomainListId,

    -- * Destructuring the Response
    DeleteFirewallDomainListResponse (..),
    newDeleteFirewallDomainListResponse,

    -- * Response Lenses
    deleteFirewallDomainListResponse_firewallDomainList,
    deleteFirewallDomainListResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newDeleteFirewallDomainList' smart constructor.
data DeleteFirewallDomainList = DeleteFirewallDomainList'
  { -- | The ID of the domain list that you want to delete.
    firewallDomainListId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFirewallDomainList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallDomainListId', 'deleteFirewallDomainList_firewallDomainListId' - The ID of the domain list that you want to delete.
newDeleteFirewallDomainList ::
  -- | 'firewallDomainListId'
  Prelude.Text ->
  DeleteFirewallDomainList
newDeleteFirewallDomainList pFirewallDomainListId_ =
  DeleteFirewallDomainList'
    { firewallDomainListId =
        pFirewallDomainListId_
    }

-- | The ID of the domain list that you want to delete.
deleteFirewallDomainList_firewallDomainListId :: Lens.Lens' DeleteFirewallDomainList Prelude.Text
deleteFirewallDomainList_firewallDomainListId = Lens.lens (\DeleteFirewallDomainList' {firewallDomainListId} -> firewallDomainListId) (\s@DeleteFirewallDomainList' {} a -> s {firewallDomainListId = a} :: DeleteFirewallDomainList)

instance Core.AWSRequest DeleteFirewallDomainList where
  type
    AWSResponse DeleteFirewallDomainList =
      DeleteFirewallDomainListResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteFirewallDomainListResponse'
            Prelude.<$> (x Data..?> "FirewallDomainList")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteFirewallDomainList where
  hashWithSalt _salt DeleteFirewallDomainList' {..} =
    _salt `Prelude.hashWithSalt` firewallDomainListId

instance Prelude.NFData DeleteFirewallDomainList where
  rnf DeleteFirewallDomainList' {..} =
    Prelude.rnf firewallDomainListId

instance Data.ToHeaders DeleteFirewallDomainList where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.DeleteFirewallDomainList" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteFirewallDomainList where
  toJSON DeleteFirewallDomainList' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "FirewallDomainListId"
                  Data..= firewallDomainListId
              )
          ]
      )

instance Data.ToPath DeleteFirewallDomainList where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteFirewallDomainList where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFirewallDomainListResponse' smart constructor.
data DeleteFirewallDomainListResponse = DeleteFirewallDomainListResponse'
  { -- | The domain list that you just deleted.
    firewallDomainList :: Prelude.Maybe FirewallDomainList,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFirewallDomainListResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallDomainList', 'deleteFirewallDomainListResponse_firewallDomainList' - The domain list that you just deleted.
--
-- 'httpStatus', 'deleteFirewallDomainListResponse_httpStatus' - The response's http status code.
newDeleteFirewallDomainListResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteFirewallDomainListResponse
newDeleteFirewallDomainListResponse pHttpStatus_ =
  DeleteFirewallDomainListResponse'
    { firewallDomainList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The domain list that you just deleted.
deleteFirewallDomainListResponse_firewallDomainList :: Lens.Lens' DeleteFirewallDomainListResponse (Prelude.Maybe FirewallDomainList)
deleteFirewallDomainListResponse_firewallDomainList = Lens.lens (\DeleteFirewallDomainListResponse' {firewallDomainList} -> firewallDomainList) (\s@DeleteFirewallDomainListResponse' {} a -> s {firewallDomainList = a} :: DeleteFirewallDomainListResponse)

-- | The response's http status code.
deleteFirewallDomainListResponse_httpStatus :: Lens.Lens' DeleteFirewallDomainListResponse Prelude.Int
deleteFirewallDomainListResponse_httpStatus = Lens.lens (\DeleteFirewallDomainListResponse' {httpStatus} -> httpStatus) (\s@DeleteFirewallDomainListResponse' {} a -> s {httpStatus = a} :: DeleteFirewallDomainListResponse)

instance
  Prelude.NFData
    DeleteFirewallDomainListResponse
  where
  rnf DeleteFirewallDomainListResponse' {..} =
    Prelude.rnf firewallDomainList
      `Prelude.seq` Prelude.rnf httpStatus
