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
-- Module      : Amazonka.PrivateNetworks.UpdateNetworkSitePlan
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified network site plan.
module Amazonka.PrivateNetworks.UpdateNetworkSitePlan
  ( -- * Creating a Request
    UpdateNetworkSitePlan (..),
    newUpdateNetworkSitePlan,

    -- * Request Lenses
    updateNetworkSitePlan_clientToken,
    updateNetworkSitePlan_networkSiteArn,
    updateNetworkSitePlan_pendingPlan,

    -- * Destructuring the Response
    UpdateNetworkSiteResponse (..),
    newUpdateNetworkSiteResponse,

    -- * Response Lenses
    updateNetworkSiteResponse_tags,
    updateNetworkSiteResponse_networkSite,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.PrivateNetworks.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateNetworkSitePlan' smart constructor.
data UpdateNetworkSitePlan = UpdateNetworkSitePlan'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to ensure idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the network site.
    networkSiteArn :: Prelude.Text,
    -- | The pending plan.
    pendingPlan :: SitePlan
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateNetworkSitePlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateNetworkSitePlan_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to ensure idempotency>.
--
-- 'networkSiteArn', 'updateNetworkSitePlan_networkSiteArn' - The Amazon Resource Name (ARN) of the network site.
--
-- 'pendingPlan', 'updateNetworkSitePlan_pendingPlan' - The pending plan.
newUpdateNetworkSitePlan ::
  -- | 'networkSiteArn'
  Prelude.Text ->
  -- | 'pendingPlan'
  SitePlan ->
  UpdateNetworkSitePlan
newUpdateNetworkSitePlan
  pNetworkSiteArn_
  pPendingPlan_ =
    UpdateNetworkSitePlan'
      { clientToken =
          Prelude.Nothing,
        networkSiteArn = pNetworkSiteArn_,
        pendingPlan = pPendingPlan_
      }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to ensure idempotency>.
updateNetworkSitePlan_clientToken :: Lens.Lens' UpdateNetworkSitePlan (Prelude.Maybe Prelude.Text)
updateNetworkSitePlan_clientToken = Lens.lens (\UpdateNetworkSitePlan' {clientToken} -> clientToken) (\s@UpdateNetworkSitePlan' {} a -> s {clientToken = a} :: UpdateNetworkSitePlan)

-- | The Amazon Resource Name (ARN) of the network site.
updateNetworkSitePlan_networkSiteArn :: Lens.Lens' UpdateNetworkSitePlan Prelude.Text
updateNetworkSitePlan_networkSiteArn = Lens.lens (\UpdateNetworkSitePlan' {networkSiteArn} -> networkSiteArn) (\s@UpdateNetworkSitePlan' {} a -> s {networkSiteArn = a} :: UpdateNetworkSitePlan)

-- | The pending plan.
updateNetworkSitePlan_pendingPlan :: Lens.Lens' UpdateNetworkSitePlan SitePlan
updateNetworkSitePlan_pendingPlan = Lens.lens (\UpdateNetworkSitePlan' {pendingPlan} -> pendingPlan) (\s@UpdateNetworkSitePlan' {} a -> s {pendingPlan = a} :: UpdateNetworkSitePlan)

instance Core.AWSRequest UpdateNetworkSitePlan where
  type
    AWSResponse UpdateNetworkSitePlan =
      UpdateNetworkSiteResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable UpdateNetworkSitePlan where
  hashWithSalt _salt UpdateNetworkSitePlan' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` networkSiteArn
      `Prelude.hashWithSalt` pendingPlan

instance Prelude.NFData UpdateNetworkSitePlan where
  rnf UpdateNetworkSitePlan' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf networkSiteArn
      `Prelude.seq` Prelude.rnf pendingPlan

instance Core.ToHeaders UpdateNetworkSitePlan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateNetworkSitePlan where
  toJSON UpdateNetworkSitePlan' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("clientToken" Core..=) Prelude.<$> clientToken,
            Prelude.Just
              ("networkSiteArn" Core..= networkSiteArn),
            Prelude.Just ("pendingPlan" Core..= pendingPlan)
          ]
      )

instance Core.ToPath UpdateNetworkSitePlan where
  toPath = Prelude.const "/v1/network-sites/plan"

instance Core.ToQuery UpdateNetworkSitePlan where
  toQuery = Prelude.const Prelude.mempty
