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
-- Module      : Amazonka.Connect.UpdateRoutingProfileConcurrency
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the channels that agents can handle in the Contact Control Panel
-- (CCP) for a routing profile.
module Amazonka.Connect.UpdateRoutingProfileConcurrency
  ( -- * Creating a Request
    UpdateRoutingProfileConcurrency (..),
    newUpdateRoutingProfileConcurrency,

    -- * Request Lenses
    updateRoutingProfileConcurrency_instanceId,
    updateRoutingProfileConcurrency_routingProfileId,
    updateRoutingProfileConcurrency_mediaConcurrencies,

    -- * Destructuring the Response
    UpdateRoutingProfileConcurrencyResponse (..),
    newUpdateRoutingProfileConcurrencyResponse,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateRoutingProfileConcurrency' smart constructor.
data UpdateRoutingProfileConcurrency = UpdateRoutingProfileConcurrency'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the routing profile.
    routingProfileId :: Prelude.Text,
    -- | The channels that agents can handle in the Contact Control Panel (CCP).
    mediaConcurrencies :: [MediaConcurrency]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRoutingProfileConcurrency' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'updateRoutingProfileConcurrency_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'routingProfileId', 'updateRoutingProfileConcurrency_routingProfileId' - The identifier of the routing profile.
--
-- 'mediaConcurrencies', 'updateRoutingProfileConcurrency_mediaConcurrencies' - The channels that agents can handle in the Contact Control Panel (CCP).
newUpdateRoutingProfileConcurrency ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'routingProfileId'
  Prelude.Text ->
  UpdateRoutingProfileConcurrency
newUpdateRoutingProfileConcurrency
  pInstanceId_
  pRoutingProfileId_ =
    UpdateRoutingProfileConcurrency'
      { instanceId =
          pInstanceId_,
        routingProfileId = pRoutingProfileId_,
        mediaConcurrencies = Prelude.mempty
      }

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
updateRoutingProfileConcurrency_instanceId :: Lens.Lens' UpdateRoutingProfileConcurrency Prelude.Text
updateRoutingProfileConcurrency_instanceId = Lens.lens (\UpdateRoutingProfileConcurrency' {instanceId} -> instanceId) (\s@UpdateRoutingProfileConcurrency' {} a -> s {instanceId = a} :: UpdateRoutingProfileConcurrency)

-- | The identifier of the routing profile.
updateRoutingProfileConcurrency_routingProfileId :: Lens.Lens' UpdateRoutingProfileConcurrency Prelude.Text
updateRoutingProfileConcurrency_routingProfileId = Lens.lens (\UpdateRoutingProfileConcurrency' {routingProfileId} -> routingProfileId) (\s@UpdateRoutingProfileConcurrency' {} a -> s {routingProfileId = a} :: UpdateRoutingProfileConcurrency)

-- | The channels that agents can handle in the Contact Control Panel (CCP).
updateRoutingProfileConcurrency_mediaConcurrencies :: Lens.Lens' UpdateRoutingProfileConcurrency [MediaConcurrency]
updateRoutingProfileConcurrency_mediaConcurrencies = Lens.lens (\UpdateRoutingProfileConcurrency' {mediaConcurrencies} -> mediaConcurrencies) (\s@UpdateRoutingProfileConcurrency' {} a -> s {mediaConcurrencies = a} :: UpdateRoutingProfileConcurrency) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    UpdateRoutingProfileConcurrency
  where
  type
    AWSResponse UpdateRoutingProfileConcurrency =
      UpdateRoutingProfileConcurrencyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      UpdateRoutingProfileConcurrencyResponse'

instance
  Prelude.Hashable
    UpdateRoutingProfileConcurrency
  where
  hashWithSalt
    _salt
    UpdateRoutingProfileConcurrency' {..} =
      _salt `Prelude.hashWithSalt` instanceId
        `Prelude.hashWithSalt` routingProfileId
        `Prelude.hashWithSalt` mediaConcurrencies

instance
  Prelude.NFData
    UpdateRoutingProfileConcurrency
  where
  rnf UpdateRoutingProfileConcurrency' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf routingProfileId
      `Prelude.seq` Prelude.rnf mediaConcurrencies

instance
  Data.ToHeaders
    UpdateRoutingProfileConcurrency
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRoutingProfileConcurrency where
  toJSON UpdateRoutingProfileConcurrency' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("MediaConcurrencies" Data..= mediaConcurrencies)
          ]
      )

instance Data.ToPath UpdateRoutingProfileConcurrency where
  toPath UpdateRoutingProfileConcurrency' {..} =
    Prelude.mconcat
      [ "/routing-profiles/",
        Data.toBS instanceId,
        "/",
        Data.toBS routingProfileId,
        "/concurrency"
      ]

instance Data.ToQuery UpdateRoutingProfileConcurrency where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRoutingProfileConcurrencyResponse' smart constructor.
data UpdateRoutingProfileConcurrencyResponse = UpdateRoutingProfileConcurrencyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRoutingProfileConcurrencyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateRoutingProfileConcurrencyResponse ::
  UpdateRoutingProfileConcurrencyResponse
newUpdateRoutingProfileConcurrencyResponse =
  UpdateRoutingProfileConcurrencyResponse'

instance
  Prelude.NFData
    UpdateRoutingProfileConcurrencyResponse
  where
  rnf _ = ()
