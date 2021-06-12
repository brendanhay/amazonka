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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'newUpdateAutomaticTapeCreationPolicy' smart constructor.
data UpdateAutomaticTapeCreationPolicy = UpdateAutomaticTapeCreationPolicy'
  { -- | An automatic tape creation policy consists of a list of automatic tape
    -- creation rules. The rules determine when and how to automatically create
    -- new tapes.
    automaticTapeCreationRules :: Core.NonEmpty AutomaticTapeCreationRule,
    gatewayARN :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.NonEmpty AutomaticTapeCreationRule ->
  -- | 'gatewayARN'
  Core.Text ->
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
updateAutomaticTapeCreationPolicy_automaticTapeCreationRules :: Lens.Lens' UpdateAutomaticTapeCreationPolicy (Core.NonEmpty AutomaticTapeCreationRule)
updateAutomaticTapeCreationPolicy_automaticTapeCreationRules = Lens.lens (\UpdateAutomaticTapeCreationPolicy' {automaticTapeCreationRules} -> automaticTapeCreationRules) (\s@UpdateAutomaticTapeCreationPolicy' {} a -> s {automaticTapeCreationRules = a} :: UpdateAutomaticTapeCreationPolicy) Core.. Lens._Coerce

-- | Undocumented member.
updateAutomaticTapeCreationPolicy_gatewayARN :: Lens.Lens' UpdateAutomaticTapeCreationPolicy Core.Text
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
            Core.<$> (x Core..?> "GatewayARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    UpdateAutomaticTapeCreationPolicy

instance
  Core.NFData
    UpdateAutomaticTapeCreationPolicy

instance
  Core.ToHeaders
    UpdateAutomaticTapeCreationPolicy
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.UpdateAutomaticTapeCreationPolicy" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    UpdateAutomaticTapeCreationPolicy
  where
  toJSON UpdateAutomaticTapeCreationPolicy' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "AutomaticTapeCreationRules"
                  Core..= automaticTapeCreationRules
              ),
            Core.Just ("GatewayARN" Core..= gatewayARN)
          ]
      )

instance
  Core.ToPath
    UpdateAutomaticTapeCreationPolicy
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    UpdateAutomaticTapeCreationPolicy
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateAutomaticTapeCreationPolicyResponse' smart constructor.
data UpdateAutomaticTapeCreationPolicyResponse = UpdateAutomaticTapeCreationPolicyResponse'
  { gatewayARN :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateAutomaticTapeCreationPolicyResponse
newUpdateAutomaticTapeCreationPolicyResponse
  pHttpStatus_ =
    UpdateAutomaticTapeCreationPolicyResponse'
      { gatewayARN =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
updateAutomaticTapeCreationPolicyResponse_gatewayARN :: Lens.Lens' UpdateAutomaticTapeCreationPolicyResponse (Core.Maybe Core.Text)
updateAutomaticTapeCreationPolicyResponse_gatewayARN = Lens.lens (\UpdateAutomaticTapeCreationPolicyResponse' {gatewayARN} -> gatewayARN) (\s@UpdateAutomaticTapeCreationPolicyResponse' {} a -> s {gatewayARN = a} :: UpdateAutomaticTapeCreationPolicyResponse)

-- | The response's http status code.
updateAutomaticTapeCreationPolicyResponse_httpStatus :: Lens.Lens' UpdateAutomaticTapeCreationPolicyResponse Core.Int
updateAutomaticTapeCreationPolicyResponse_httpStatus = Lens.lens (\UpdateAutomaticTapeCreationPolicyResponse' {httpStatus} -> httpStatus) (\s@UpdateAutomaticTapeCreationPolicyResponse' {} a -> s {httpStatus = a} :: UpdateAutomaticTapeCreationPolicyResponse)

instance
  Core.NFData
    UpdateAutomaticTapeCreationPolicyResponse
