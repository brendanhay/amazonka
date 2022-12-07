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
-- Module      : Amazonka.Pi.GetResourceMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve the metadata for different features. For example, the metadata
-- might indicate that a feature is turned on or off on a specific DB
-- instance.
module Amazonka.Pi.GetResourceMetadata
  ( -- * Creating a Request
    GetResourceMetadata (..),
    newGetResourceMetadata,

    -- * Request Lenses
    getResourceMetadata_serviceType,
    getResourceMetadata_identifier,

    -- * Destructuring the Response
    GetResourceMetadataResponse (..),
    newGetResourceMetadataResponse,

    -- * Response Lenses
    getResourceMetadataResponse_features,
    getResourceMetadataResponse_identifier,
    getResourceMetadataResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pi.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetResourceMetadata' smart constructor.
data GetResourceMetadata = GetResourceMetadata'
  { -- | The Amazon Web Services service for which Performance Insights returns
    -- metrics.
    serviceType :: ServiceType,
    -- | An immutable identifier for a data source that is unique for an Amazon
    -- Web Services Region. Performance Insights gathers metrics from this data
    -- source. To use a DB instance as a data source, specify its
    -- @DbiResourceId@ value. For example, specify
    -- @db-ABCDEFGHIJKLMNOPQRSTU1VW2X@.
    identifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourceMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceType', 'getResourceMetadata_serviceType' - The Amazon Web Services service for which Performance Insights returns
-- metrics.
--
-- 'identifier', 'getResourceMetadata_identifier' - An immutable identifier for a data source that is unique for an Amazon
-- Web Services Region. Performance Insights gathers metrics from this data
-- source. To use a DB instance as a data source, specify its
-- @DbiResourceId@ value. For example, specify
-- @db-ABCDEFGHIJKLMNOPQRSTU1VW2X@.
newGetResourceMetadata ::
  -- | 'serviceType'
  ServiceType ->
  -- | 'identifier'
  Prelude.Text ->
  GetResourceMetadata
newGetResourceMetadata pServiceType_ pIdentifier_ =
  GetResourceMetadata'
    { serviceType = pServiceType_,
      identifier = pIdentifier_
    }

-- | The Amazon Web Services service for which Performance Insights returns
-- metrics.
getResourceMetadata_serviceType :: Lens.Lens' GetResourceMetadata ServiceType
getResourceMetadata_serviceType = Lens.lens (\GetResourceMetadata' {serviceType} -> serviceType) (\s@GetResourceMetadata' {} a -> s {serviceType = a} :: GetResourceMetadata)

-- | An immutable identifier for a data source that is unique for an Amazon
-- Web Services Region. Performance Insights gathers metrics from this data
-- source. To use a DB instance as a data source, specify its
-- @DbiResourceId@ value. For example, specify
-- @db-ABCDEFGHIJKLMNOPQRSTU1VW2X@.
getResourceMetadata_identifier :: Lens.Lens' GetResourceMetadata Prelude.Text
getResourceMetadata_identifier = Lens.lens (\GetResourceMetadata' {identifier} -> identifier) (\s@GetResourceMetadata' {} a -> s {identifier = a} :: GetResourceMetadata)

instance Core.AWSRequest GetResourceMetadata where
  type
    AWSResponse GetResourceMetadata =
      GetResourceMetadataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourceMetadataResponse'
            Prelude.<$> (x Data..?> "Features" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Identifier")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetResourceMetadata where
  hashWithSalt _salt GetResourceMetadata' {..} =
    _salt `Prelude.hashWithSalt` serviceType
      `Prelude.hashWithSalt` identifier

instance Prelude.NFData GetResourceMetadata where
  rnf GetResourceMetadata' {..} =
    Prelude.rnf serviceType
      `Prelude.seq` Prelude.rnf identifier

instance Data.ToHeaders GetResourceMetadata where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PerformanceInsightsv20180227.GetResourceMetadata" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetResourceMetadata where
  toJSON GetResourceMetadata' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ServiceType" Data..= serviceType),
            Prelude.Just ("Identifier" Data..= identifier)
          ]
      )

instance Data.ToPath GetResourceMetadata where
  toPath = Prelude.const "/"

instance Data.ToQuery GetResourceMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResourceMetadataResponse' smart constructor.
data GetResourceMetadataResponse = GetResourceMetadataResponse'
  { -- | The metadata for different features. For example, the metadata might
    -- indicate that a feature is turned on or off on a specific DB instance.
    features :: Prelude.Maybe (Prelude.HashMap Prelude.Text FeatureMetadata),
    -- | An immutable identifier for a data source that is unique for an Amazon
    -- Web Services Region. Performance Insights gathers metrics from this data
    -- source. To use a DB instance as a data source, specify its
    -- @DbiResourceId@ value. For example, specify
    -- @db-ABCDEFGHIJKLMNOPQRSTU1VW2X@.
    identifier :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourceMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'features', 'getResourceMetadataResponse_features' - The metadata for different features. For example, the metadata might
-- indicate that a feature is turned on or off on a specific DB instance.
--
-- 'identifier', 'getResourceMetadataResponse_identifier' - An immutable identifier for a data source that is unique for an Amazon
-- Web Services Region. Performance Insights gathers metrics from this data
-- source. To use a DB instance as a data source, specify its
-- @DbiResourceId@ value. For example, specify
-- @db-ABCDEFGHIJKLMNOPQRSTU1VW2X@.
--
-- 'httpStatus', 'getResourceMetadataResponse_httpStatus' - The response's http status code.
newGetResourceMetadataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResourceMetadataResponse
newGetResourceMetadataResponse pHttpStatus_ =
  GetResourceMetadataResponse'
    { features =
        Prelude.Nothing,
      identifier = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The metadata for different features. For example, the metadata might
-- indicate that a feature is turned on or off on a specific DB instance.
getResourceMetadataResponse_features :: Lens.Lens' GetResourceMetadataResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text FeatureMetadata))
getResourceMetadataResponse_features = Lens.lens (\GetResourceMetadataResponse' {features} -> features) (\s@GetResourceMetadataResponse' {} a -> s {features = a} :: GetResourceMetadataResponse) Prelude.. Lens.mapping Lens.coerced

-- | An immutable identifier for a data source that is unique for an Amazon
-- Web Services Region. Performance Insights gathers metrics from this data
-- source. To use a DB instance as a data source, specify its
-- @DbiResourceId@ value. For example, specify
-- @db-ABCDEFGHIJKLMNOPQRSTU1VW2X@.
getResourceMetadataResponse_identifier :: Lens.Lens' GetResourceMetadataResponse (Prelude.Maybe Prelude.Text)
getResourceMetadataResponse_identifier = Lens.lens (\GetResourceMetadataResponse' {identifier} -> identifier) (\s@GetResourceMetadataResponse' {} a -> s {identifier = a} :: GetResourceMetadataResponse)

-- | The response's http status code.
getResourceMetadataResponse_httpStatus :: Lens.Lens' GetResourceMetadataResponse Prelude.Int
getResourceMetadataResponse_httpStatus = Lens.lens (\GetResourceMetadataResponse' {httpStatus} -> httpStatus) (\s@GetResourceMetadataResponse' {} a -> s {httpStatus = a} :: GetResourceMetadataResponse)

instance Prelude.NFData GetResourceMetadataResponse where
  rnf GetResourceMetadataResponse' {..} =
    Prelude.rnf features
      `Prelude.seq` Prelude.rnf identifier
      `Prelude.seq` Prelude.rnf httpStatus
