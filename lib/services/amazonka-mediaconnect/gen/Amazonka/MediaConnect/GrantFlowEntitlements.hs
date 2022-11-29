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
-- Module      : Amazonka.MediaConnect.GrantFlowEntitlements
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants entitlements to an existing flow.
module Amazonka.MediaConnect.GrantFlowEntitlements
  ( -- * Creating a Request
    GrantFlowEntitlements (..),
    newGrantFlowEntitlements,

    -- * Request Lenses
    grantFlowEntitlements_flowArn,
    grantFlowEntitlements_entitlements,

    -- * Destructuring the Response
    GrantFlowEntitlementsResponse (..),
    newGrantFlowEntitlementsResponse,

    -- * Response Lenses
    grantFlowEntitlementsResponse_entitlements,
    grantFlowEntitlementsResponse_flowArn,
    grantFlowEntitlementsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to grant entitlements on a flow.
--
-- /See:/ 'newGrantFlowEntitlements' smart constructor.
data GrantFlowEntitlements = GrantFlowEntitlements'
  { -- | The flow that you want to grant entitlements on.
    flowArn :: Prelude.Text,
    -- | The list of entitlements that you want to grant.
    entitlements :: [GrantEntitlementRequest]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GrantFlowEntitlements' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowArn', 'grantFlowEntitlements_flowArn' - The flow that you want to grant entitlements on.
--
-- 'entitlements', 'grantFlowEntitlements_entitlements' - The list of entitlements that you want to grant.
newGrantFlowEntitlements ::
  -- | 'flowArn'
  Prelude.Text ->
  GrantFlowEntitlements
newGrantFlowEntitlements pFlowArn_ =
  GrantFlowEntitlements'
    { flowArn = pFlowArn_,
      entitlements = Prelude.mempty
    }

-- | The flow that you want to grant entitlements on.
grantFlowEntitlements_flowArn :: Lens.Lens' GrantFlowEntitlements Prelude.Text
grantFlowEntitlements_flowArn = Lens.lens (\GrantFlowEntitlements' {flowArn} -> flowArn) (\s@GrantFlowEntitlements' {} a -> s {flowArn = a} :: GrantFlowEntitlements)

-- | The list of entitlements that you want to grant.
grantFlowEntitlements_entitlements :: Lens.Lens' GrantFlowEntitlements [GrantEntitlementRequest]
grantFlowEntitlements_entitlements = Lens.lens (\GrantFlowEntitlements' {entitlements} -> entitlements) (\s@GrantFlowEntitlements' {} a -> s {entitlements = a} :: GrantFlowEntitlements) Prelude.. Lens.coerced

instance Core.AWSRequest GrantFlowEntitlements where
  type
    AWSResponse GrantFlowEntitlements =
      GrantFlowEntitlementsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GrantFlowEntitlementsResponse'
            Prelude.<$> (x Core..?> "entitlements" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "flowArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GrantFlowEntitlements where
  hashWithSalt _salt GrantFlowEntitlements' {..} =
    _salt `Prelude.hashWithSalt` flowArn
      `Prelude.hashWithSalt` entitlements

instance Prelude.NFData GrantFlowEntitlements where
  rnf GrantFlowEntitlements' {..} =
    Prelude.rnf flowArn
      `Prelude.seq` Prelude.rnf entitlements

instance Core.ToHeaders GrantFlowEntitlements where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GrantFlowEntitlements where
  toJSON GrantFlowEntitlements' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("entitlements" Core..= entitlements)]
      )

instance Core.ToPath GrantFlowEntitlements where
  toPath GrantFlowEntitlements' {..} =
    Prelude.mconcat
      ["/v1/flows/", Core.toBS flowArn, "/entitlements"]

instance Core.ToQuery GrantFlowEntitlements where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGrantFlowEntitlementsResponse' smart constructor.
data GrantFlowEntitlementsResponse = GrantFlowEntitlementsResponse'
  { -- | The entitlements that were just granted.
    entitlements :: Prelude.Maybe [Entitlement],
    -- | The ARN of the flow that these entitlements were granted to.
    flowArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GrantFlowEntitlementsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entitlements', 'grantFlowEntitlementsResponse_entitlements' - The entitlements that were just granted.
--
-- 'flowArn', 'grantFlowEntitlementsResponse_flowArn' - The ARN of the flow that these entitlements were granted to.
--
-- 'httpStatus', 'grantFlowEntitlementsResponse_httpStatus' - The response's http status code.
newGrantFlowEntitlementsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GrantFlowEntitlementsResponse
newGrantFlowEntitlementsResponse pHttpStatus_ =
  GrantFlowEntitlementsResponse'
    { entitlements =
        Prelude.Nothing,
      flowArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The entitlements that were just granted.
grantFlowEntitlementsResponse_entitlements :: Lens.Lens' GrantFlowEntitlementsResponse (Prelude.Maybe [Entitlement])
grantFlowEntitlementsResponse_entitlements = Lens.lens (\GrantFlowEntitlementsResponse' {entitlements} -> entitlements) (\s@GrantFlowEntitlementsResponse' {} a -> s {entitlements = a} :: GrantFlowEntitlementsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the flow that these entitlements were granted to.
grantFlowEntitlementsResponse_flowArn :: Lens.Lens' GrantFlowEntitlementsResponse (Prelude.Maybe Prelude.Text)
grantFlowEntitlementsResponse_flowArn = Lens.lens (\GrantFlowEntitlementsResponse' {flowArn} -> flowArn) (\s@GrantFlowEntitlementsResponse' {} a -> s {flowArn = a} :: GrantFlowEntitlementsResponse)

-- | The response's http status code.
grantFlowEntitlementsResponse_httpStatus :: Lens.Lens' GrantFlowEntitlementsResponse Prelude.Int
grantFlowEntitlementsResponse_httpStatus = Lens.lens (\GrantFlowEntitlementsResponse' {httpStatus} -> httpStatus) (\s@GrantFlowEntitlementsResponse' {} a -> s {httpStatus = a} :: GrantFlowEntitlementsResponse)

instance Prelude.NFData GrantFlowEntitlementsResponse where
  rnf GrantFlowEntitlementsResponse' {..} =
    Prelude.rnf entitlements
      `Prelude.seq` Prelude.rnf flowArn
      `Prelude.seq` Prelude.rnf httpStatus
