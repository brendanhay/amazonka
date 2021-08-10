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
-- Module      : Network.AWS.StorageGateway.UpdateAutomaticTapeCreationPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the automatic tape creation policy of a gateway. Use this to
-- update the policy with a new set of automatic tape creation rules. This
-- is only supported for tape gateways.
--
-- By default, there is no automatic tape creation policy.
--
-- A gateway can have only one automatic tape creation policy.
module Network.AWS.StorageGateway.UpdateAutomaticTapeCreationPolicy
  ( -- * Creating a Request
    UpdateAutomaticTapeCreationPolicy (..),
    newUpdateAutomaticTapeCreationPolicy,

    -- * Request Lenses
    updateAutomaticTapeCreationPolicy_automaticTapeCreationRules,
    updateAutomaticTapeCreationPolicy_gatewayARN,

    -- * Destructuring the Response
    UpdateAutomaticTapeCreationPolicyResponse (..),
    newUpdateAutomaticTapeCreationPolicyResponse,

    -- * Response Lenses
    updateAutomaticTapeCreationPolicyResponse_gatewayARN,
    updateAutomaticTapeCreationPolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'newUpdateAutomaticTapeCreationPolicy' smart constructor.
data UpdateAutomaticTapeCreationPolicy = UpdateAutomaticTapeCreationPolicy'
  { -- | An automatic tape creation policy consists of a list of automatic tape
    -- creation rules. The rules determine when and how to automatically create
    -- new tapes.
    automaticTapeCreationRules :: Prelude.NonEmpty AutomaticTapeCreationRule,
    gatewayARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAutomaticTapeCreationPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'automaticTapeCreationRules', 'updateAutomaticTapeCreationPolicy_automaticTapeCreationRules' - An automatic tape creation policy consists of a list of automatic tape
-- creation rules. The rules determine when and how to automatically create
-- new tapes.
--
-- 'gatewayARN', 'updateAutomaticTapeCreationPolicy_gatewayARN' - Undocumented member.
newUpdateAutomaticTapeCreationPolicy ::
  -- | 'automaticTapeCreationRules'
  Prelude.NonEmpty AutomaticTapeCreationRule ->
  -- | 'gatewayARN'
  Prelude.Text ->
  UpdateAutomaticTapeCreationPolicy
newUpdateAutomaticTapeCreationPolicy
  pAutomaticTapeCreationRules_
  pGatewayARN_ =
    UpdateAutomaticTapeCreationPolicy'
      { automaticTapeCreationRules =
          Lens._Coerce
            Lens.# pAutomaticTapeCreationRules_,
        gatewayARN = pGatewayARN_
      }

-- | An automatic tape creation policy consists of a list of automatic tape
-- creation rules. The rules determine when and how to automatically create
-- new tapes.
updateAutomaticTapeCreationPolicy_automaticTapeCreationRules :: Lens.Lens' UpdateAutomaticTapeCreationPolicy (Prelude.NonEmpty AutomaticTapeCreationRule)
updateAutomaticTapeCreationPolicy_automaticTapeCreationRules = Lens.lens (\UpdateAutomaticTapeCreationPolicy' {automaticTapeCreationRules} -> automaticTapeCreationRules) (\s@UpdateAutomaticTapeCreationPolicy' {} a -> s {automaticTapeCreationRules = a} :: UpdateAutomaticTapeCreationPolicy) Prelude.. Lens._Coerce

-- | Undocumented member.
updateAutomaticTapeCreationPolicy_gatewayARN :: Lens.Lens' UpdateAutomaticTapeCreationPolicy Prelude.Text
updateAutomaticTapeCreationPolicy_gatewayARN = Lens.lens (\UpdateAutomaticTapeCreationPolicy' {gatewayARN} -> gatewayARN) (\s@UpdateAutomaticTapeCreationPolicy' {} a -> s {gatewayARN = a} :: UpdateAutomaticTapeCreationPolicy)

instance
  Core.AWSRequest
    UpdateAutomaticTapeCreationPolicy
  where
  type
    AWSResponse UpdateAutomaticTapeCreationPolicy =
      UpdateAutomaticTapeCreationPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAutomaticTapeCreationPolicyResponse'
            Prelude.<$> (x Core..?> "GatewayARN")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateAutomaticTapeCreationPolicy

instance
  Prelude.NFData
    UpdateAutomaticTapeCreationPolicy

instance
  Core.ToHeaders
    UpdateAutomaticTapeCreationPolicy
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.UpdateAutomaticTapeCreationPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    UpdateAutomaticTapeCreationPolicy
  where
  toJSON UpdateAutomaticTapeCreationPolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "AutomaticTapeCreationRules"
                  Core..= automaticTapeCreationRules
              ),
            Prelude.Just ("GatewayARN" Core..= gatewayARN)
          ]
      )

instance
  Core.ToPath
    UpdateAutomaticTapeCreationPolicy
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    UpdateAutomaticTapeCreationPolicy
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAutomaticTapeCreationPolicyResponse' smart constructor.
data UpdateAutomaticTapeCreationPolicyResponse = UpdateAutomaticTapeCreationPolicyResponse'
  { gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAutomaticTapeCreationPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'updateAutomaticTapeCreationPolicyResponse_gatewayARN' - Undocumented member.
--
-- 'httpStatus', 'updateAutomaticTapeCreationPolicyResponse_httpStatus' - The response's http status code.
newUpdateAutomaticTapeCreationPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateAutomaticTapeCreationPolicyResponse
newUpdateAutomaticTapeCreationPolicyResponse
  pHttpStatus_ =
    UpdateAutomaticTapeCreationPolicyResponse'
      { gatewayARN =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
updateAutomaticTapeCreationPolicyResponse_gatewayARN :: Lens.Lens' UpdateAutomaticTapeCreationPolicyResponse (Prelude.Maybe Prelude.Text)
updateAutomaticTapeCreationPolicyResponse_gatewayARN = Lens.lens (\UpdateAutomaticTapeCreationPolicyResponse' {gatewayARN} -> gatewayARN) (\s@UpdateAutomaticTapeCreationPolicyResponse' {} a -> s {gatewayARN = a} :: UpdateAutomaticTapeCreationPolicyResponse)

-- | The response's http status code.
updateAutomaticTapeCreationPolicyResponse_httpStatus :: Lens.Lens' UpdateAutomaticTapeCreationPolicyResponse Prelude.Int
updateAutomaticTapeCreationPolicyResponse_httpStatus = Lens.lens (\UpdateAutomaticTapeCreationPolicyResponse' {httpStatus} -> httpStatus) (\s@UpdateAutomaticTapeCreationPolicyResponse' {} a -> s {httpStatus = a} :: UpdateAutomaticTapeCreationPolicyResponse)

instance
  Prelude.NFData
    UpdateAutomaticTapeCreationPolicyResponse
