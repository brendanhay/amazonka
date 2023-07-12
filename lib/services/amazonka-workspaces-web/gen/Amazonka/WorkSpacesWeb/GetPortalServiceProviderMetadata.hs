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
-- Module      : Amazonka.WorkSpacesWeb.GetPortalServiceProviderMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the service provider metadata.
module Amazonka.WorkSpacesWeb.GetPortalServiceProviderMetadata
  ( -- * Creating a Request
    GetPortalServiceProviderMetadata (..),
    newGetPortalServiceProviderMetadata,

    -- * Request Lenses
    getPortalServiceProviderMetadata_portalArn,

    -- * Destructuring the Response
    GetPortalServiceProviderMetadataResponse (..),
    newGetPortalServiceProviderMetadataResponse,

    -- * Response Lenses
    getPortalServiceProviderMetadataResponse_serviceProviderSamlMetadata,
    getPortalServiceProviderMetadataResponse_httpStatus,
    getPortalServiceProviderMetadataResponse_portalArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newGetPortalServiceProviderMetadata' smart constructor.
data GetPortalServiceProviderMetadata = GetPortalServiceProviderMetadata'
  { -- | The ARN of the web portal.
    portalArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPortalServiceProviderMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portalArn', 'getPortalServiceProviderMetadata_portalArn' - The ARN of the web portal.
newGetPortalServiceProviderMetadata ::
  -- | 'portalArn'
  Prelude.Text ->
  GetPortalServiceProviderMetadata
newGetPortalServiceProviderMetadata pPortalArn_ =
  GetPortalServiceProviderMetadata'
    { portalArn =
        pPortalArn_
    }

-- | The ARN of the web portal.
getPortalServiceProviderMetadata_portalArn :: Lens.Lens' GetPortalServiceProviderMetadata Prelude.Text
getPortalServiceProviderMetadata_portalArn = Lens.lens (\GetPortalServiceProviderMetadata' {portalArn} -> portalArn) (\s@GetPortalServiceProviderMetadata' {} a -> s {portalArn = a} :: GetPortalServiceProviderMetadata)

instance
  Core.AWSRequest
    GetPortalServiceProviderMetadata
  where
  type
    AWSResponse GetPortalServiceProviderMetadata =
      GetPortalServiceProviderMetadataResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPortalServiceProviderMetadataResponse'
            Prelude.<$> (x Data..?> "serviceProviderSamlMetadata")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "portalArn")
      )

instance
  Prelude.Hashable
    GetPortalServiceProviderMetadata
  where
  hashWithSalt
    _salt
    GetPortalServiceProviderMetadata' {..} =
      _salt `Prelude.hashWithSalt` portalArn

instance
  Prelude.NFData
    GetPortalServiceProviderMetadata
  where
  rnf GetPortalServiceProviderMetadata' {..} =
    Prelude.rnf portalArn

instance
  Data.ToHeaders
    GetPortalServiceProviderMetadata
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

instance Data.ToPath GetPortalServiceProviderMetadata where
  toPath GetPortalServiceProviderMetadata' {..} =
    Prelude.mconcat
      ["/portalIdp/", Data.toBS portalArn]

instance
  Data.ToQuery
    GetPortalServiceProviderMetadata
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPortalServiceProviderMetadataResponse' smart constructor.
data GetPortalServiceProviderMetadataResponse = GetPortalServiceProviderMetadataResponse'
  { -- | The service provider SAML metadata.
    serviceProviderSamlMetadata :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN of the web portal.
    portalArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPortalServiceProviderMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceProviderSamlMetadata', 'getPortalServiceProviderMetadataResponse_serviceProviderSamlMetadata' - The service provider SAML metadata.
--
-- 'httpStatus', 'getPortalServiceProviderMetadataResponse_httpStatus' - The response's http status code.
--
-- 'portalArn', 'getPortalServiceProviderMetadataResponse_portalArn' - The ARN of the web portal.
newGetPortalServiceProviderMetadataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'portalArn'
  Prelude.Text ->
  GetPortalServiceProviderMetadataResponse
newGetPortalServiceProviderMetadataResponse
  pHttpStatus_
  pPortalArn_ =
    GetPortalServiceProviderMetadataResponse'
      { serviceProviderSamlMetadata =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        portalArn = pPortalArn_
      }

-- | The service provider SAML metadata.
getPortalServiceProviderMetadataResponse_serviceProviderSamlMetadata :: Lens.Lens' GetPortalServiceProviderMetadataResponse (Prelude.Maybe Prelude.Text)
getPortalServiceProviderMetadataResponse_serviceProviderSamlMetadata = Lens.lens (\GetPortalServiceProviderMetadataResponse' {serviceProviderSamlMetadata} -> serviceProviderSamlMetadata) (\s@GetPortalServiceProviderMetadataResponse' {} a -> s {serviceProviderSamlMetadata = a} :: GetPortalServiceProviderMetadataResponse)

-- | The response's http status code.
getPortalServiceProviderMetadataResponse_httpStatus :: Lens.Lens' GetPortalServiceProviderMetadataResponse Prelude.Int
getPortalServiceProviderMetadataResponse_httpStatus = Lens.lens (\GetPortalServiceProviderMetadataResponse' {httpStatus} -> httpStatus) (\s@GetPortalServiceProviderMetadataResponse' {} a -> s {httpStatus = a} :: GetPortalServiceProviderMetadataResponse)

-- | The ARN of the web portal.
getPortalServiceProviderMetadataResponse_portalArn :: Lens.Lens' GetPortalServiceProviderMetadataResponse Prelude.Text
getPortalServiceProviderMetadataResponse_portalArn = Lens.lens (\GetPortalServiceProviderMetadataResponse' {portalArn} -> portalArn) (\s@GetPortalServiceProviderMetadataResponse' {} a -> s {portalArn = a} :: GetPortalServiceProviderMetadataResponse)

instance
  Prelude.NFData
    GetPortalServiceProviderMetadataResponse
  where
  rnf GetPortalServiceProviderMetadataResponse' {..} =
    Prelude.rnf serviceProviderSamlMetadata
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf portalArn
