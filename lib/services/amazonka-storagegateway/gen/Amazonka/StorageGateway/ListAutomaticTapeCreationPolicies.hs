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
-- Module      : Amazonka.StorageGateway.ListAutomaticTapeCreationPolicies
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the automatic tape creation policies for a gateway. If there are
-- no automatic tape creation policies for the gateway, it returns an empty
-- list.
--
-- This operation is only supported for tape gateways.
module Amazonka.StorageGateway.ListAutomaticTapeCreationPolicies
  ( -- * Creating a Request
    ListAutomaticTapeCreationPolicies (..),
    newListAutomaticTapeCreationPolicies,

    -- * Request Lenses
    listAutomaticTapeCreationPolicies_gatewayARN,

    -- * Destructuring the Response
    ListAutomaticTapeCreationPoliciesResponse (..),
    newListAutomaticTapeCreationPoliciesResponse,

    -- * Response Lenses
    listAutomaticTapeCreationPoliciesResponse_automaticTapeCreationPolicyInfos,
    listAutomaticTapeCreationPoliciesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | /See:/ 'newListAutomaticTapeCreationPolicies' smart constructor.
data ListAutomaticTapeCreationPolicies = ListAutomaticTapeCreationPolicies'
  { gatewayARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAutomaticTapeCreationPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'listAutomaticTapeCreationPolicies_gatewayARN' - Undocumented member.
newListAutomaticTapeCreationPolicies ::
  ListAutomaticTapeCreationPolicies
newListAutomaticTapeCreationPolicies =
  ListAutomaticTapeCreationPolicies'
    { gatewayARN =
        Prelude.Nothing
    }

-- | Undocumented member.
listAutomaticTapeCreationPolicies_gatewayARN :: Lens.Lens' ListAutomaticTapeCreationPolicies (Prelude.Maybe Prelude.Text)
listAutomaticTapeCreationPolicies_gatewayARN = Lens.lens (\ListAutomaticTapeCreationPolicies' {gatewayARN} -> gatewayARN) (\s@ListAutomaticTapeCreationPolicies' {} a -> s {gatewayARN = a} :: ListAutomaticTapeCreationPolicies)

instance
  Core.AWSRequest
    ListAutomaticTapeCreationPolicies
  where
  type
    AWSResponse ListAutomaticTapeCreationPolicies =
      ListAutomaticTapeCreationPoliciesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAutomaticTapeCreationPoliciesResponse'
            Prelude.<$> ( x Data..?> "AutomaticTapeCreationPolicyInfos"
                            Core..!@ Prelude.mempty
                        )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListAutomaticTapeCreationPolicies
  where
  hashWithSalt
    _salt
    ListAutomaticTapeCreationPolicies' {..} =
      _salt `Prelude.hashWithSalt` gatewayARN

instance
  Prelude.NFData
    ListAutomaticTapeCreationPolicies
  where
  rnf ListAutomaticTapeCreationPolicies' {..} =
    Prelude.rnf gatewayARN

instance
  Data.ToHeaders
    ListAutomaticTapeCreationPolicies
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.ListAutomaticTapeCreationPolicies" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    ListAutomaticTapeCreationPolicies
  where
  toJSON ListAutomaticTapeCreationPolicies' {..} =
    Data.object
      ( Prelude.catMaybes
          [("GatewayARN" Data..=) Prelude.<$> gatewayARN]
      )

instance
  Data.ToPath
    ListAutomaticTapeCreationPolicies
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ListAutomaticTapeCreationPolicies
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAutomaticTapeCreationPoliciesResponse' smart constructor.
data ListAutomaticTapeCreationPoliciesResponse = ListAutomaticTapeCreationPoliciesResponse'
  { -- | Gets a listing of information about the gateway\'s automatic tape
    -- creation policies, including the automatic tape creation rules and the
    -- gateway that is using the policies.
    automaticTapeCreationPolicyInfos :: Prelude.Maybe [AutomaticTapeCreationPolicyInfo],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAutomaticTapeCreationPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'automaticTapeCreationPolicyInfos', 'listAutomaticTapeCreationPoliciesResponse_automaticTapeCreationPolicyInfos' - Gets a listing of information about the gateway\'s automatic tape
-- creation policies, including the automatic tape creation rules and the
-- gateway that is using the policies.
--
-- 'httpStatus', 'listAutomaticTapeCreationPoliciesResponse_httpStatus' - The response's http status code.
newListAutomaticTapeCreationPoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAutomaticTapeCreationPoliciesResponse
newListAutomaticTapeCreationPoliciesResponse
  pHttpStatus_ =
    ListAutomaticTapeCreationPoliciesResponse'
      { automaticTapeCreationPolicyInfos =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Gets a listing of information about the gateway\'s automatic tape
-- creation policies, including the automatic tape creation rules and the
-- gateway that is using the policies.
listAutomaticTapeCreationPoliciesResponse_automaticTapeCreationPolicyInfos :: Lens.Lens' ListAutomaticTapeCreationPoliciesResponse (Prelude.Maybe [AutomaticTapeCreationPolicyInfo])
listAutomaticTapeCreationPoliciesResponse_automaticTapeCreationPolicyInfos = Lens.lens (\ListAutomaticTapeCreationPoliciesResponse' {automaticTapeCreationPolicyInfos} -> automaticTapeCreationPolicyInfos) (\s@ListAutomaticTapeCreationPoliciesResponse' {} a -> s {automaticTapeCreationPolicyInfos = a} :: ListAutomaticTapeCreationPoliciesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listAutomaticTapeCreationPoliciesResponse_httpStatus :: Lens.Lens' ListAutomaticTapeCreationPoliciesResponse Prelude.Int
listAutomaticTapeCreationPoliciesResponse_httpStatus = Lens.lens (\ListAutomaticTapeCreationPoliciesResponse' {httpStatus} -> httpStatus) (\s@ListAutomaticTapeCreationPoliciesResponse' {} a -> s {httpStatus = a} :: ListAutomaticTapeCreationPoliciesResponse)

instance
  Prelude.NFData
    ListAutomaticTapeCreationPoliciesResponse
  where
  rnf ListAutomaticTapeCreationPoliciesResponse' {..} =
    Prelude.rnf automaticTapeCreationPolicyInfos
      `Prelude.seq` Prelude.rnf httpStatus
