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
-- Module      : Amazonka.CodeStarConnections.GetHost
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the host ARN and details such as status, provider type,
-- endpoint, and, if applicable, the VPC configuration.
module Amazonka.CodeStarConnections.GetHost
  ( -- * Creating a Request
    GetHost (..),
    newGetHost,

    -- * Request Lenses
    getHost_hostArn,

    -- * Destructuring the Response
    GetHostResponse (..),
    newGetHostResponse,

    -- * Response Lenses
    getHostResponse_name,
    getHostResponse_providerEndpoint,
    getHostResponse_providerType,
    getHostResponse_status,
    getHostResponse_vpcConfiguration,
    getHostResponse_httpStatus,
  )
where

import Amazonka.CodeStarConnections.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetHost' smart constructor.
data GetHost = GetHost'
  { -- | The Amazon Resource Name (ARN) of the requested host.
    hostArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetHost' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostArn', 'getHost_hostArn' - The Amazon Resource Name (ARN) of the requested host.
newGetHost ::
  -- | 'hostArn'
  Prelude.Text ->
  GetHost
newGetHost pHostArn_ = GetHost' {hostArn = pHostArn_}

-- | The Amazon Resource Name (ARN) of the requested host.
getHost_hostArn :: Lens.Lens' GetHost Prelude.Text
getHost_hostArn = Lens.lens (\GetHost' {hostArn} -> hostArn) (\s@GetHost' {} a -> s {hostArn = a} :: GetHost)

instance Core.AWSRequest GetHost where
  type AWSResponse GetHost = GetHostResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetHostResponse'
            Prelude.<$> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "ProviderEndpoint")
            Prelude.<*> (x Data..?> "ProviderType")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "VpcConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetHost where
  hashWithSalt _salt GetHost' {..} =
    _salt `Prelude.hashWithSalt` hostArn

instance Prelude.NFData GetHost where
  rnf GetHost' {..} = Prelude.rnf hostArn

instance Data.ToHeaders GetHost where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "com.amazonaws.codestar.connections.CodeStar_connections_20191201.GetHost" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetHost where
  toJSON GetHost' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("HostArn" Data..= hostArn)]
      )

instance Data.ToPath GetHost where
  toPath = Prelude.const "/"

instance Data.ToQuery GetHost where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetHostResponse' smart constructor.
data GetHostResponse = GetHostResponse'
  { -- | The name of the requested host.
    name :: Prelude.Maybe Prelude.Text,
    -- | The endpoint of the infrastructure represented by the requested host.
    providerEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The provider type of the requested host, such as GitHub Enterprise
    -- Server.
    providerType :: Prelude.Maybe ProviderType,
    -- | The status of the requested host.
    status :: Prelude.Maybe Prelude.Text,
    -- | The VPC configuration of the requested host.
    vpcConfiguration :: Prelude.Maybe VpcConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetHostResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getHostResponse_name' - The name of the requested host.
--
-- 'providerEndpoint', 'getHostResponse_providerEndpoint' - The endpoint of the infrastructure represented by the requested host.
--
-- 'providerType', 'getHostResponse_providerType' - The provider type of the requested host, such as GitHub Enterprise
-- Server.
--
-- 'status', 'getHostResponse_status' - The status of the requested host.
--
-- 'vpcConfiguration', 'getHostResponse_vpcConfiguration' - The VPC configuration of the requested host.
--
-- 'httpStatus', 'getHostResponse_httpStatus' - The response's http status code.
newGetHostResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetHostResponse
newGetHostResponse pHttpStatus_ =
  GetHostResponse'
    { name = Prelude.Nothing,
      providerEndpoint = Prelude.Nothing,
      providerType = Prelude.Nothing,
      status = Prelude.Nothing,
      vpcConfiguration = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the requested host.
getHostResponse_name :: Lens.Lens' GetHostResponse (Prelude.Maybe Prelude.Text)
getHostResponse_name = Lens.lens (\GetHostResponse' {name} -> name) (\s@GetHostResponse' {} a -> s {name = a} :: GetHostResponse)

-- | The endpoint of the infrastructure represented by the requested host.
getHostResponse_providerEndpoint :: Lens.Lens' GetHostResponse (Prelude.Maybe Prelude.Text)
getHostResponse_providerEndpoint = Lens.lens (\GetHostResponse' {providerEndpoint} -> providerEndpoint) (\s@GetHostResponse' {} a -> s {providerEndpoint = a} :: GetHostResponse)

-- | The provider type of the requested host, such as GitHub Enterprise
-- Server.
getHostResponse_providerType :: Lens.Lens' GetHostResponse (Prelude.Maybe ProviderType)
getHostResponse_providerType = Lens.lens (\GetHostResponse' {providerType} -> providerType) (\s@GetHostResponse' {} a -> s {providerType = a} :: GetHostResponse)

-- | The status of the requested host.
getHostResponse_status :: Lens.Lens' GetHostResponse (Prelude.Maybe Prelude.Text)
getHostResponse_status = Lens.lens (\GetHostResponse' {status} -> status) (\s@GetHostResponse' {} a -> s {status = a} :: GetHostResponse)

-- | The VPC configuration of the requested host.
getHostResponse_vpcConfiguration :: Lens.Lens' GetHostResponse (Prelude.Maybe VpcConfiguration)
getHostResponse_vpcConfiguration = Lens.lens (\GetHostResponse' {vpcConfiguration} -> vpcConfiguration) (\s@GetHostResponse' {} a -> s {vpcConfiguration = a} :: GetHostResponse)

-- | The response's http status code.
getHostResponse_httpStatus :: Lens.Lens' GetHostResponse Prelude.Int
getHostResponse_httpStatus = Lens.lens (\GetHostResponse' {httpStatus} -> httpStatus) (\s@GetHostResponse' {} a -> s {httpStatus = a} :: GetHostResponse)

instance Prelude.NFData GetHostResponse where
  rnf GetHostResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf providerEndpoint
      `Prelude.seq` Prelude.rnf providerType
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf vpcConfiguration
      `Prelude.seq` Prelude.rnf httpStatus
