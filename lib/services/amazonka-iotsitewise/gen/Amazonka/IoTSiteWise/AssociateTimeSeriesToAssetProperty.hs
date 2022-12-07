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
-- Module      : Amazonka.IoTSiteWise.AssociateTimeSeriesToAssetProperty
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a time series (data stream) with an asset property.
module Amazonka.IoTSiteWise.AssociateTimeSeriesToAssetProperty
  ( -- * Creating a Request
    AssociateTimeSeriesToAssetProperty (..),
    newAssociateTimeSeriesToAssetProperty,

    -- * Request Lenses
    associateTimeSeriesToAssetProperty_clientToken,
    associateTimeSeriesToAssetProperty_alias,
    associateTimeSeriesToAssetProperty_assetId,
    associateTimeSeriesToAssetProperty_propertyId,

    -- * Destructuring the Response
    AssociateTimeSeriesToAssetPropertyResponse (..),
    newAssociateTimeSeriesToAssetPropertyResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateTimeSeriesToAssetProperty' smart constructor.
data AssociateTimeSeriesToAssetProperty = AssociateTimeSeriesToAssetProperty'
  { -- | A unique case-sensitive identifier that you can provide to ensure the
    -- idempotency of the request. Don\'t reuse this client token if a new
    -- idempotent request is required.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The alias that identifies the time series.
    alias :: Prelude.Text,
    -- | The ID of the asset in which the asset property was created.
    assetId :: Prelude.Text,
    -- | The ID of the asset property.
    propertyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateTimeSeriesToAssetProperty' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'associateTimeSeriesToAssetProperty_clientToken' - A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
--
-- 'alias', 'associateTimeSeriesToAssetProperty_alias' - The alias that identifies the time series.
--
-- 'assetId', 'associateTimeSeriesToAssetProperty_assetId' - The ID of the asset in which the asset property was created.
--
-- 'propertyId', 'associateTimeSeriesToAssetProperty_propertyId' - The ID of the asset property.
newAssociateTimeSeriesToAssetProperty ::
  -- | 'alias'
  Prelude.Text ->
  -- | 'assetId'
  Prelude.Text ->
  -- | 'propertyId'
  Prelude.Text ->
  AssociateTimeSeriesToAssetProperty
newAssociateTimeSeriesToAssetProperty
  pAlias_
  pAssetId_
  pPropertyId_ =
    AssociateTimeSeriesToAssetProperty'
      { clientToken =
          Prelude.Nothing,
        alias = pAlias_,
        assetId = pAssetId_,
        propertyId = pPropertyId_
      }

-- | A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
associateTimeSeriesToAssetProperty_clientToken :: Lens.Lens' AssociateTimeSeriesToAssetProperty (Prelude.Maybe Prelude.Text)
associateTimeSeriesToAssetProperty_clientToken = Lens.lens (\AssociateTimeSeriesToAssetProperty' {clientToken} -> clientToken) (\s@AssociateTimeSeriesToAssetProperty' {} a -> s {clientToken = a} :: AssociateTimeSeriesToAssetProperty)

-- | The alias that identifies the time series.
associateTimeSeriesToAssetProperty_alias :: Lens.Lens' AssociateTimeSeriesToAssetProperty Prelude.Text
associateTimeSeriesToAssetProperty_alias = Lens.lens (\AssociateTimeSeriesToAssetProperty' {alias} -> alias) (\s@AssociateTimeSeriesToAssetProperty' {} a -> s {alias = a} :: AssociateTimeSeriesToAssetProperty)

-- | The ID of the asset in which the asset property was created.
associateTimeSeriesToAssetProperty_assetId :: Lens.Lens' AssociateTimeSeriesToAssetProperty Prelude.Text
associateTimeSeriesToAssetProperty_assetId = Lens.lens (\AssociateTimeSeriesToAssetProperty' {assetId} -> assetId) (\s@AssociateTimeSeriesToAssetProperty' {} a -> s {assetId = a} :: AssociateTimeSeriesToAssetProperty)

-- | The ID of the asset property.
associateTimeSeriesToAssetProperty_propertyId :: Lens.Lens' AssociateTimeSeriesToAssetProperty Prelude.Text
associateTimeSeriesToAssetProperty_propertyId = Lens.lens (\AssociateTimeSeriesToAssetProperty' {propertyId} -> propertyId) (\s@AssociateTimeSeriesToAssetProperty' {} a -> s {propertyId = a} :: AssociateTimeSeriesToAssetProperty)

instance
  Core.AWSRequest
    AssociateTimeSeriesToAssetProperty
  where
  type
    AWSResponse AssociateTimeSeriesToAssetProperty =
      AssociateTimeSeriesToAssetPropertyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      AssociateTimeSeriesToAssetPropertyResponse'

instance
  Prelude.Hashable
    AssociateTimeSeriesToAssetProperty
  where
  hashWithSalt
    _salt
    AssociateTimeSeriesToAssetProperty' {..} =
      _salt `Prelude.hashWithSalt` clientToken
        `Prelude.hashWithSalt` alias
        `Prelude.hashWithSalt` assetId
        `Prelude.hashWithSalt` propertyId

instance
  Prelude.NFData
    AssociateTimeSeriesToAssetProperty
  where
  rnf AssociateTimeSeriesToAssetProperty' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf alias
      `Prelude.seq` Prelude.rnf assetId
      `Prelude.seq` Prelude.rnf propertyId

instance
  Data.ToHeaders
    AssociateTimeSeriesToAssetProperty
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

instance
  Data.ToJSON
    AssociateTimeSeriesToAssetProperty
  where
  toJSON AssociateTimeSeriesToAssetProperty' {..} =
    Data.object
      ( Prelude.catMaybes
          [("clientToken" Data..=) Prelude.<$> clientToken]
      )

instance
  Data.ToPath
    AssociateTimeSeriesToAssetProperty
  where
  toPath = Prelude.const "/timeseries/associate/"

instance
  Data.ToQuery
    AssociateTimeSeriesToAssetProperty
  where
  toQuery AssociateTimeSeriesToAssetProperty' {..} =
    Prelude.mconcat
      [ "alias" Data.=: alias,
        "assetId" Data.=: assetId,
        "propertyId" Data.=: propertyId
      ]

-- | /See:/ 'newAssociateTimeSeriesToAssetPropertyResponse' smart constructor.
data AssociateTimeSeriesToAssetPropertyResponse = AssociateTimeSeriesToAssetPropertyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateTimeSeriesToAssetPropertyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAssociateTimeSeriesToAssetPropertyResponse ::
  AssociateTimeSeriesToAssetPropertyResponse
newAssociateTimeSeriesToAssetPropertyResponse =
  AssociateTimeSeriesToAssetPropertyResponse'

instance
  Prelude.NFData
    AssociateTimeSeriesToAssetPropertyResponse
  where
  rnf _ = ()
