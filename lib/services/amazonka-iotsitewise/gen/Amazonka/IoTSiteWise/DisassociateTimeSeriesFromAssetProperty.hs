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
-- Module      : Amazonka.IoTSiteWise.DisassociateTimeSeriesFromAssetProperty
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a time series (data stream) from an asset property.
module Amazonka.IoTSiteWise.DisassociateTimeSeriesFromAssetProperty
  ( -- * Creating a Request
    DisassociateTimeSeriesFromAssetProperty (..),
    newDisassociateTimeSeriesFromAssetProperty,

    -- * Request Lenses
    disassociateTimeSeriesFromAssetProperty_clientToken,
    disassociateTimeSeriesFromAssetProperty_alias,
    disassociateTimeSeriesFromAssetProperty_assetId,
    disassociateTimeSeriesFromAssetProperty_propertyId,

    -- * Destructuring the Response
    DisassociateTimeSeriesFromAssetPropertyResponse (..),
    newDisassociateTimeSeriesFromAssetPropertyResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateTimeSeriesFromAssetProperty' smart constructor.
data DisassociateTimeSeriesFromAssetProperty = DisassociateTimeSeriesFromAssetProperty'
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
-- Create a value of 'DisassociateTimeSeriesFromAssetProperty' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'disassociateTimeSeriesFromAssetProperty_clientToken' - A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
--
-- 'alias', 'disassociateTimeSeriesFromAssetProperty_alias' - The alias that identifies the time series.
--
-- 'assetId', 'disassociateTimeSeriesFromAssetProperty_assetId' - The ID of the asset in which the asset property was created.
--
-- 'propertyId', 'disassociateTimeSeriesFromAssetProperty_propertyId' - The ID of the asset property.
newDisassociateTimeSeriesFromAssetProperty ::
  -- | 'alias'
  Prelude.Text ->
  -- | 'assetId'
  Prelude.Text ->
  -- | 'propertyId'
  Prelude.Text ->
  DisassociateTimeSeriesFromAssetProperty
newDisassociateTimeSeriesFromAssetProperty
  pAlias_
  pAssetId_
  pPropertyId_ =
    DisassociateTimeSeriesFromAssetProperty'
      { clientToken =
          Prelude.Nothing,
        alias = pAlias_,
        assetId = pAssetId_,
        propertyId = pPropertyId_
      }

-- | A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
disassociateTimeSeriesFromAssetProperty_clientToken :: Lens.Lens' DisassociateTimeSeriesFromAssetProperty (Prelude.Maybe Prelude.Text)
disassociateTimeSeriesFromAssetProperty_clientToken = Lens.lens (\DisassociateTimeSeriesFromAssetProperty' {clientToken} -> clientToken) (\s@DisassociateTimeSeriesFromAssetProperty' {} a -> s {clientToken = a} :: DisassociateTimeSeriesFromAssetProperty)

-- | The alias that identifies the time series.
disassociateTimeSeriesFromAssetProperty_alias :: Lens.Lens' DisassociateTimeSeriesFromAssetProperty Prelude.Text
disassociateTimeSeriesFromAssetProperty_alias = Lens.lens (\DisassociateTimeSeriesFromAssetProperty' {alias} -> alias) (\s@DisassociateTimeSeriesFromAssetProperty' {} a -> s {alias = a} :: DisassociateTimeSeriesFromAssetProperty)

-- | The ID of the asset in which the asset property was created.
disassociateTimeSeriesFromAssetProperty_assetId :: Lens.Lens' DisassociateTimeSeriesFromAssetProperty Prelude.Text
disassociateTimeSeriesFromAssetProperty_assetId = Lens.lens (\DisassociateTimeSeriesFromAssetProperty' {assetId} -> assetId) (\s@DisassociateTimeSeriesFromAssetProperty' {} a -> s {assetId = a} :: DisassociateTimeSeriesFromAssetProperty)

-- | The ID of the asset property.
disassociateTimeSeriesFromAssetProperty_propertyId :: Lens.Lens' DisassociateTimeSeriesFromAssetProperty Prelude.Text
disassociateTimeSeriesFromAssetProperty_propertyId = Lens.lens (\DisassociateTimeSeriesFromAssetProperty' {propertyId} -> propertyId) (\s@DisassociateTimeSeriesFromAssetProperty' {} a -> s {propertyId = a} :: DisassociateTimeSeriesFromAssetProperty)

instance
  Core.AWSRequest
    DisassociateTimeSeriesFromAssetProperty
  where
  type
    AWSResponse
      DisassociateTimeSeriesFromAssetProperty =
      DisassociateTimeSeriesFromAssetPropertyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DisassociateTimeSeriesFromAssetPropertyResponse'

instance
  Prelude.Hashable
    DisassociateTimeSeriesFromAssetProperty
  where
  hashWithSalt
    _salt
    DisassociateTimeSeriesFromAssetProperty' {..} =
      _salt
        `Prelude.hashWithSalt` clientToken
        `Prelude.hashWithSalt` alias
        `Prelude.hashWithSalt` assetId
        `Prelude.hashWithSalt` propertyId

instance
  Prelude.NFData
    DisassociateTimeSeriesFromAssetProperty
  where
  rnf DisassociateTimeSeriesFromAssetProperty' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf alias `Prelude.seq`
        Prelude.rnf assetId `Prelude.seq`
          Prelude.rnf propertyId

instance
  Data.ToHeaders
    DisassociateTimeSeriesFromAssetProperty
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
    DisassociateTimeSeriesFromAssetProperty
  where
  toJSON DisassociateTimeSeriesFromAssetProperty' {..} =
    Data.object
      ( Prelude.catMaybes
          [("clientToken" Data..=) Prelude.<$> clientToken]
      )

instance
  Data.ToPath
    DisassociateTimeSeriesFromAssetProperty
  where
  toPath = Prelude.const "/timeseries/disassociate/"

instance
  Data.ToQuery
    DisassociateTimeSeriesFromAssetProperty
  where
  toQuery DisassociateTimeSeriesFromAssetProperty' {..} =
    Prelude.mconcat
      [ "alias" Data.=: alias,
        "assetId" Data.=: assetId,
        "propertyId" Data.=: propertyId
      ]

-- | /See:/ 'newDisassociateTimeSeriesFromAssetPropertyResponse' smart constructor.
data DisassociateTimeSeriesFromAssetPropertyResponse = DisassociateTimeSeriesFromAssetPropertyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateTimeSeriesFromAssetPropertyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisassociateTimeSeriesFromAssetPropertyResponse ::
  DisassociateTimeSeriesFromAssetPropertyResponse
newDisassociateTimeSeriesFromAssetPropertyResponse =
  DisassociateTimeSeriesFromAssetPropertyResponse'

instance
  Prelude.NFData
    DisassociateTimeSeriesFromAssetPropertyResponse
  where
  rnf _ = ()
