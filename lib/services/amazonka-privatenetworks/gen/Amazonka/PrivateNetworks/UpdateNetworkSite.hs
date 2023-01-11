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
-- Module      : Amazonka.PrivateNetworks.UpdateNetworkSite
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified network site.
module Amazonka.PrivateNetworks.UpdateNetworkSite
  ( -- * Creating a Request
    UpdateNetworkSite (..),
    newUpdateNetworkSite,

    -- * Request Lenses
    updateNetworkSite_clientToken,
    updateNetworkSite_description,
    updateNetworkSite_networkSiteArn,

    -- * Destructuring the Response
    UpdateNetworkSiteResponse (..),
    newUpdateNetworkSiteResponse,

    -- * Response Lenses
    updateNetworkSiteResponse_networkSite,
    updateNetworkSiteResponse_tags,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.PrivateNetworks.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateNetworkSite' smart constructor.
data UpdateNetworkSite = UpdateNetworkSite'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to ensure idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the network site.
    networkSiteArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateNetworkSite' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateNetworkSite_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to ensure idempotency>.
--
-- 'description', 'updateNetworkSite_description' - The description.
--
-- 'networkSiteArn', 'updateNetworkSite_networkSiteArn' - The Amazon Resource Name (ARN) of the network site.
newUpdateNetworkSite ::
  -- | 'networkSiteArn'
  Prelude.Text ->
  UpdateNetworkSite
newUpdateNetworkSite pNetworkSiteArn_ =
  UpdateNetworkSite'
    { clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      networkSiteArn = pNetworkSiteArn_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to ensure idempotency>.
updateNetworkSite_clientToken :: Lens.Lens' UpdateNetworkSite (Prelude.Maybe Prelude.Text)
updateNetworkSite_clientToken = Lens.lens (\UpdateNetworkSite' {clientToken} -> clientToken) (\s@UpdateNetworkSite' {} a -> s {clientToken = a} :: UpdateNetworkSite)

-- | The description.
updateNetworkSite_description :: Lens.Lens' UpdateNetworkSite (Prelude.Maybe Prelude.Text)
updateNetworkSite_description = Lens.lens (\UpdateNetworkSite' {description} -> description) (\s@UpdateNetworkSite' {} a -> s {description = a} :: UpdateNetworkSite)

-- | The Amazon Resource Name (ARN) of the network site.
updateNetworkSite_networkSiteArn :: Lens.Lens' UpdateNetworkSite Prelude.Text
updateNetworkSite_networkSiteArn = Lens.lens (\UpdateNetworkSite' {networkSiteArn} -> networkSiteArn) (\s@UpdateNetworkSite' {} a -> s {networkSiteArn = a} :: UpdateNetworkSite)

instance Core.AWSRequest UpdateNetworkSite where
  type
    AWSResponse UpdateNetworkSite =
      UpdateNetworkSiteResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable UpdateNetworkSite where
  hashWithSalt _salt UpdateNetworkSite' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` networkSiteArn

instance Prelude.NFData UpdateNetworkSite where
  rnf UpdateNetworkSite' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf networkSiteArn

instance Data.ToHeaders UpdateNetworkSite where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateNetworkSite where
  toJSON UpdateNetworkSite' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("description" Data..=) Prelude.<$> description,
            Prelude.Just
              ("networkSiteArn" Data..= networkSiteArn)
          ]
      )

instance Data.ToPath UpdateNetworkSite where
  toPath = Prelude.const "/v1/network-sites/site"

instance Data.ToQuery UpdateNetworkSite where
  toQuery = Prelude.const Prelude.mempty
