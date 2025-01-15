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
-- Module      : Amazonka.Route53Resolver.GetFirewallDomainList
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified firewall domain list.
module Amazonka.Route53Resolver.GetFirewallDomainList
  ( -- * Creating a Request
    GetFirewallDomainList (..),
    newGetFirewallDomainList,

    -- * Request Lenses
    getFirewallDomainList_firewallDomainListId,

    -- * Destructuring the Response
    GetFirewallDomainListResponse (..),
    newGetFirewallDomainListResponse,

    -- * Response Lenses
    getFirewallDomainListResponse_firewallDomainList,
    getFirewallDomainListResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newGetFirewallDomainList' smart constructor.
data GetFirewallDomainList = GetFirewallDomainList'
  { -- | The ID of the domain list.
    firewallDomainListId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFirewallDomainList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallDomainListId', 'getFirewallDomainList_firewallDomainListId' - The ID of the domain list.
newGetFirewallDomainList ::
  -- | 'firewallDomainListId'
  Prelude.Text ->
  GetFirewallDomainList
newGetFirewallDomainList pFirewallDomainListId_ =
  GetFirewallDomainList'
    { firewallDomainListId =
        pFirewallDomainListId_
    }

-- | The ID of the domain list.
getFirewallDomainList_firewallDomainListId :: Lens.Lens' GetFirewallDomainList Prelude.Text
getFirewallDomainList_firewallDomainListId = Lens.lens (\GetFirewallDomainList' {firewallDomainListId} -> firewallDomainListId) (\s@GetFirewallDomainList' {} a -> s {firewallDomainListId = a} :: GetFirewallDomainList)

instance Core.AWSRequest GetFirewallDomainList where
  type
    AWSResponse GetFirewallDomainList =
      GetFirewallDomainListResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFirewallDomainListResponse'
            Prelude.<$> (x Data..?> "FirewallDomainList")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetFirewallDomainList where
  hashWithSalt _salt GetFirewallDomainList' {..} =
    _salt `Prelude.hashWithSalt` firewallDomainListId

instance Prelude.NFData GetFirewallDomainList where
  rnf GetFirewallDomainList' {..} =
    Prelude.rnf firewallDomainListId

instance Data.ToHeaders GetFirewallDomainList where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.GetFirewallDomainList" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetFirewallDomainList where
  toJSON GetFirewallDomainList' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "FirewallDomainListId"
                  Data..= firewallDomainListId
              )
          ]
      )

instance Data.ToPath GetFirewallDomainList where
  toPath = Prelude.const "/"

instance Data.ToQuery GetFirewallDomainList where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetFirewallDomainListResponse' smart constructor.
data GetFirewallDomainListResponse = GetFirewallDomainListResponse'
  { -- | The domain list that you requested.
    firewallDomainList :: Prelude.Maybe FirewallDomainList,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFirewallDomainListResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallDomainList', 'getFirewallDomainListResponse_firewallDomainList' - The domain list that you requested.
--
-- 'httpStatus', 'getFirewallDomainListResponse_httpStatus' - The response's http status code.
newGetFirewallDomainListResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetFirewallDomainListResponse
newGetFirewallDomainListResponse pHttpStatus_ =
  GetFirewallDomainListResponse'
    { firewallDomainList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The domain list that you requested.
getFirewallDomainListResponse_firewallDomainList :: Lens.Lens' GetFirewallDomainListResponse (Prelude.Maybe FirewallDomainList)
getFirewallDomainListResponse_firewallDomainList = Lens.lens (\GetFirewallDomainListResponse' {firewallDomainList} -> firewallDomainList) (\s@GetFirewallDomainListResponse' {} a -> s {firewallDomainList = a} :: GetFirewallDomainListResponse)

-- | The response's http status code.
getFirewallDomainListResponse_httpStatus :: Lens.Lens' GetFirewallDomainListResponse Prelude.Int
getFirewallDomainListResponse_httpStatus = Lens.lens (\GetFirewallDomainListResponse' {httpStatus} -> httpStatus) (\s@GetFirewallDomainListResponse' {} a -> s {httpStatus = a} :: GetFirewallDomainListResponse)

instance Prelude.NFData GetFirewallDomainListResponse where
  rnf GetFirewallDomainListResponse' {..} =
    Prelude.rnf firewallDomainList `Prelude.seq`
      Prelude.rnf httpStatus
