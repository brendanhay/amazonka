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
-- Module      : Network.AWS.StorageGateway.ListAutomaticTapeCreationPolicies
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.StorageGateway.ListAutomaticTapeCreationPolicies
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'newListAutomaticTapeCreationPolicies' smart constructor.
data ListAutomaticTapeCreationPolicies = ListAutomaticTapeCreationPolicies'
  { gatewayARN :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing
    }

-- | Undocumented member.
listAutomaticTapeCreationPolicies_gatewayARN :: Lens.Lens' ListAutomaticTapeCreationPolicies (Core.Maybe Core.Text)
listAutomaticTapeCreationPolicies_gatewayARN = Lens.lens (\ListAutomaticTapeCreationPolicies' {gatewayARN} -> gatewayARN) (\s@ListAutomaticTapeCreationPolicies' {} a -> s {gatewayARN = a} :: ListAutomaticTapeCreationPolicies)

instance
  Core.AWSRequest
    ListAutomaticTapeCreationPolicies
  where
  type
    AWSResponse ListAutomaticTapeCreationPolicies =
      ListAutomaticTapeCreationPoliciesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAutomaticTapeCreationPoliciesResponse'
            Core.<$> ( x Core..?> "AutomaticTapeCreationPolicyInfos"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    ListAutomaticTapeCreationPolicies

instance
  Core.NFData
    ListAutomaticTapeCreationPolicies

instance
  Core.ToHeaders
    ListAutomaticTapeCreationPolicies
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.ListAutomaticTapeCreationPolicies" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    ListAutomaticTapeCreationPolicies
  where
  toJSON ListAutomaticTapeCreationPolicies' {..} =
    Core.object
      ( Core.catMaybes
          [("GatewayARN" Core..=) Core.<$> gatewayARN]
      )

instance
  Core.ToPath
    ListAutomaticTapeCreationPolicies
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    ListAutomaticTapeCreationPolicies
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListAutomaticTapeCreationPoliciesResponse' smart constructor.
data ListAutomaticTapeCreationPoliciesResponse = ListAutomaticTapeCreationPoliciesResponse'
  { -- | Gets a listing of information about the gateway\'s automatic tape
    -- creation policies, including the automatic tape creation rules and the
    -- gateway that is using the policies.
    automaticTapeCreationPolicyInfos :: Core.Maybe [AutomaticTapeCreationPolicyInfo],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListAutomaticTapeCreationPoliciesResponse
newListAutomaticTapeCreationPoliciesResponse
  pHttpStatus_ =
    ListAutomaticTapeCreationPoliciesResponse'
      { automaticTapeCreationPolicyInfos =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Gets a listing of information about the gateway\'s automatic tape
-- creation policies, including the automatic tape creation rules and the
-- gateway that is using the policies.
listAutomaticTapeCreationPoliciesResponse_automaticTapeCreationPolicyInfos :: Lens.Lens' ListAutomaticTapeCreationPoliciesResponse (Core.Maybe [AutomaticTapeCreationPolicyInfo])
listAutomaticTapeCreationPoliciesResponse_automaticTapeCreationPolicyInfos = Lens.lens (\ListAutomaticTapeCreationPoliciesResponse' {automaticTapeCreationPolicyInfos} -> automaticTapeCreationPolicyInfos) (\s@ListAutomaticTapeCreationPoliciesResponse' {} a -> s {automaticTapeCreationPolicyInfos = a} :: ListAutomaticTapeCreationPoliciesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listAutomaticTapeCreationPoliciesResponse_httpStatus :: Lens.Lens' ListAutomaticTapeCreationPoliciesResponse Core.Int
listAutomaticTapeCreationPoliciesResponse_httpStatus = Lens.lens (\ListAutomaticTapeCreationPoliciesResponse' {httpStatus} -> httpStatus) (\s@ListAutomaticTapeCreationPoliciesResponse' {} a -> s {httpStatus = a} :: ListAutomaticTapeCreationPoliciesResponse)

instance
  Core.NFData
    ListAutomaticTapeCreationPoliciesResponse
