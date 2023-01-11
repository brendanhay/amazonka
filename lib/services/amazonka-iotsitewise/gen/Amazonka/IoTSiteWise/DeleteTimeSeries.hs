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
-- Module      : Amazonka.IoTSiteWise.DeleteTimeSeries
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a time series (data stream). If you delete a time series that\'s
-- associated with an asset property, the asset property still exists, but
-- the time series will no longer be associated with this asset property.
--
-- To identify a time series, do one of the following:
--
-- -   If the time series isn\'t associated with an asset property, specify
--     the @alias@ of the time series.
--
-- -   If the time series is associated with an asset property, specify one
--     of the following:
--
--     -   The @alias@ of the time series.
--
--     -   The @assetId@ and @propertyId@ that identifies the asset
--         property.
module Amazonka.IoTSiteWise.DeleteTimeSeries
  ( -- * Creating a Request
    DeleteTimeSeries (..),
    newDeleteTimeSeries,

    -- * Request Lenses
    deleteTimeSeries_alias,
    deleteTimeSeries_assetId,
    deleteTimeSeries_clientToken,
    deleteTimeSeries_propertyId,

    -- * Destructuring the Response
    DeleteTimeSeriesResponse (..),
    newDeleteTimeSeriesResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteTimeSeries' smart constructor.
data DeleteTimeSeries = DeleteTimeSeries'
  { -- | The alias that identifies the time series.
    alias :: Prelude.Maybe Prelude.Text,
    -- | The ID of the asset in which the asset property was created.
    assetId :: Prelude.Maybe Prelude.Text,
    -- | A unique case-sensitive identifier that you can provide to ensure the
    -- idempotency of the request. Don\'t reuse this client token if a new
    -- idempotent request is required.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the asset property.
    propertyId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTimeSeries' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alias', 'deleteTimeSeries_alias' - The alias that identifies the time series.
--
-- 'assetId', 'deleteTimeSeries_assetId' - The ID of the asset in which the asset property was created.
--
-- 'clientToken', 'deleteTimeSeries_clientToken' - A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
--
-- 'propertyId', 'deleteTimeSeries_propertyId' - The ID of the asset property.
newDeleteTimeSeries ::
  DeleteTimeSeries
newDeleteTimeSeries =
  DeleteTimeSeries'
    { alias = Prelude.Nothing,
      assetId = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      propertyId = Prelude.Nothing
    }

-- | The alias that identifies the time series.
deleteTimeSeries_alias :: Lens.Lens' DeleteTimeSeries (Prelude.Maybe Prelude.Text)
deleteTimeSeries_alias = Lens.lens (\DeleteTimeSeries' {alias} -> alias) (\s@DeleteTimeSeries' {} a -> s {alias = a} :: DeleteTimeSeries)

-- | The ID of the asset in which the asset property was created.
deleteTimeSeries_assetId :: Lens.Lens' DeleteTimeSeries (Prelude.Maybe Prelude.Text)
deleteTimeSeries_assetId = Lens.lens (\DeleteTimeSeries' {assetId} -> assetId) (\s@DeleteTimeSeries' {} a -> s {assetId = a} :: DeleteTimeSeries)

-- | A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
deleteTimeSeries_clientToken :: Lens.Lens' DeleteTimeSeries (Prelude.Maybe Prelude.Text)
deleteTimeSeries_clientToken = Lens.lens (\DeleteTimeSeries' {clientToken} -> clientToken) (\s@DeleteTimeSeries' {} a -> s {clientToken = a} :: DeleteTimeSeries)

-- | The ID of the asset property.
deleteTimeSeries_propertyId :: Lens.Lens' DeleteTimeSeries (Prelude.Maybe Prelude.Text)
deleteTimeSeries_propertyId = Lens.lens (\DeleteTimeSeries' {propertyId} -> propertyId) (\s@DeleteTimeSeries' {} a -> s {propertyId = a} :: DeleteTimeSeries)

instance Core.AWSRequest DeleteTimeSeries where
  type
    AWSResponse DeleteTimeSeries =
      DeleteTimeSeriesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteTimeSeriesResponse'

instance Prelude.Hashable DeleteTimeSeries where
  hashWithSalt _salt DeleteTimeSeries' {..} =
    _salt `Prelude.hashWithSalt` alias
      `Prelude.hashWithSalt` assetId
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` propertyId

instance Prelude.NFData DeleteTimeSeries where
  rnf DeleteTimeSeries' {..} =
    Prelude.rnf alias
      `Prelude.seq` Prelude.rnf assetId
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf propertyId

instance Data.ToHeaders DeleteTimeSeries where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteTimeSeries where
  toJSON DeleteTimeSeries' {..} =
    Data.object
      ( Prelude.catMaybes
          [("clientToken" Data..=) Prelude.<$> clientToken]
      )

instance Data.ToPath DeleteTimeSeries where
  toPath = Prelude.const "/timeseries/delete/"

instance Data.ToQuery DeleteTimeSeries where
  toQuery DeleteTimeSeries' {..} =
    Prelude.mconcat
      [ "alias" Data.=: alias,
        "assetId" Data.=: assetId,
        "propertyId" Data.=: propertyId
      ]

-- | /See:/ 'newDeleteTimeSeriesResponse' smart constructor.
data DeleteTimeSeriesResponse = DeleteTimeSeriesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTimeSeriesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteTimeSeriesResponse ::
  DeleteTimeSeriesResponse
newDeleteTimeSeriesResponse =
  DeleteTimeSeriesResponse'

instance Prelude.NFData DeleteTimeSeriesResponse where
  rnf _ = ()
