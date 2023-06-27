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
-- Module      : Amazonka.TNB.GetSolFunctionPackageDescriptor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a function package descriptor in a function package.
--
-- A function package descriptor is a .yaml file in a function package that
-- uses the TOSCA standard to describe how the network function in the
-- function package should run on your network.
--
-- A function package is a .zip file in CSAR (Cloud Service Archive) format
-- that contains a network function (an ETSI standard telecommunication
-- application) and function package descriptor that uses the TOSCA
-- standard to describe how the network functions should run on your
-- network.
module Amazonka.TNB.GetSolFunctionPackageDescriptor
  ( -- * Creating a Request
    GetSolFunctionPackageDescriptor (..),
    newGetSolFunctionPackageDescriptor,

    -- * Request Lenses
    getSolFunctionPackageDescriptor_accept,
    getSolFunctionPackageDescriptor_vnfPkgId,

    -- * Destructuring the Response
    GetSolFunctionPackageDescriptorResponse (..),
    newGetSolFunctionPackageDescriptorResponse,

    -- * Response Lenses
    getSolFunctionPackageDescriptorResponse_contentType,
    getSolFunctionPackageDescriptorResponse_vnfd,
    getSolFunctionPackageDescriptorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TNB.Types

-- | /See:/ 'newGetSolFunctionPackageDescriptor' smart constructor.
data GetSolFunctionPackageDescriptor = GetSolFunctionPackageDescriptor'
  { -- | Indicates which content types, expressed as MIME types, the client is
    -- able to understand.
    accept :: DescriptorContentType,
    -- | ID of the function package.
    vnfPkgId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSolFunctionPackageDescriptor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accept', 'getSolFunctionPackageDescriptor_accept' - Indicates which content types, expressed as MIME types, the client is
-- able to understand.
--
-- 'vnfPkgId', 'getSolFunctionPackageDescriptor_vnfPkgId' - ID of the function package.
newGetSolFunctionPackageDescriptor ::
  -- | 'accept'
  DescriptorContentType ->
  -- | 'vnfPkgId'
  Prelude.Text ->
  GetSolFunctionPackageDescriptor
newGetSolFunctionPackageDescriptor
  pAccept_
  pVnfPkgId_ =
    GetSolFunctionPackageDescriptor'
      { accept = pAccept_,
        vnfPkgId = pVnfPkgId_
      }

-- | Indicates which content types, expressed as MIME types, the client is
-- able to understand.
getSolFunctionPackageDescriptor_accept :: Lens.Lens' GetSolFunctionPackageDescriptor DescriptorContentType
getSolFunctionPackageDescriptor_accept = Lens.lens (\GetSolFunctionPackageDescriptor' {accept} -> accept) (\s@GetSolFunctionPackageDescriptor' {} a -> s {accept = a} :: GetSolFunctionPackageDescriptor)

-- | ID of the function package.
getSolFunctionPackageDescriptor_vnfPkgId :: Lens.Lens' GetSolFunctionPackageDescriptor Prelude.Text
getSolFunctionPackageDescriptor_vnfPkgId = Lens.lens (\GetSolFunctionPackageDescriptor' {vnfPkgId} -> vnfPkgId) (\s@GetSolFunctionPackageDescriptor' {} a -> s {vnfPkgId = a} :: GetSolFunctionPackageDescriptor)

instance
  Core.AWSRequest
    GetSolFunctionPackageDescriptor
  where
  type
    AWSResponse GetSolFunctionPackageDescriptor =
      GetSolFunctionPackageDescriptorResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveBytes
      ( \s h x ->
          GetSolFunctionPackageDescriptorResponse'
            Prelude.<$> (h Data..#? "Content-Type")
            Prelude.<*> (Prelude.pure (Prelude.Just (Prelude.coerce x)))
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetSolFunctionPackageDescriptor
  where
  hashWithSalt
    _salt
    GetSolFunctionPackageDescriptor' {..} =
      _salt
        `Prelude.hashWithSalt` accept
        `Prelude.hashWithSalt` vnfPkgId

instance
  Prelude.NFData
    GetSolFunctionPackageDescriptor
  where
  rnf GetSolFunctionPackageDescriptor' {..} =
    Prelude.rnf accept
      `Prelude.seq` Prelude.rnf vnfPkgId

instance
  Data.ToHeaders
    GetSolFunctionPackageDescriptor
  where
  toHeaders GetSolFunctionPackageDescriptor' {..} =
    Prelude.mconcat
      [ "Accept" Data.=# accept,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToPath GetSolFunctionPackageDescriptor where
  toPath GetSolFunctionPackageDescriptor' {..} =
    Prelude.mconcat
      [ "/sol/vnfpkgm/v1/vnf_packages/",
        Data.toBS vnfPkgId,
        "/vnfd"
      ]

instance Data.ToQuery GetSolFunctionPackageDescriptor where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSolFunctionPackageDescriptorResponse' smart constructor.
data GetSolFunctionPackageDescriptorResponse = GetSolFunctionPackageDescriptorResponse'
  { -- | Indicates the media type of the resource.
    contentType :: Prelude.Maybe DescriptorContentType,
    -- | Contents of the function package descriptor.
    vnfd :: Prelude.Maybe Prelude.ByteString,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSolFunctionPackageDescriptorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentType', 'getSolFunctionPackageDescriptorResponse_contentType' - Indicates the media type of the resource.
--
-- 'vnfd', 'getSolFunctionPackageDescriptorResponse_vnfd' - Contents of the function package descriptor.
--
-- 'httpStatus', 'getSolFunctionPackageDescriptorResponse_httpStatus' - The response's http status code.
newGetSolFunctionPackageDescriptorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSolFunctionPackageDescriptorResponse
newGetSolFunctionPackageDescriptorResponse
  pHttpStatus_ =
    GetSolFunctionPackageDescriptorResponse'
      { contentType =
          Prelude.Nothing,
        vnfd = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Indicates the media type of the resource.
getSolFunctionPackageDescriptorResponse_contentType :: Lens.Lens' GetSolFunctionPackageDescriptorResponse (Prelude.Maybe DescriptorContentType)
getSolFunctionPackageDescriptorResponse_contentType = Lens.lens (\GetSolFunctionPackageDescriptorResponse' {contentType} -> contentType) (\s@GetSolFunctionPackageDescriptorResponse' {} a -> s {contentType = a} :: GetSolFunctionPackageDescriptorResponse)

-- | Contents of the function package descriptor.
getSolFunctionPackageDescriptorResponse_vnfd :: Lens.Lens' GetSolFunctionPackageDescriptorResponse (Prelude.Maybe Prelude.ByteString)
getSolFunctionPackageDescriptorResponse_vnfd = Lens.lens (\GetSolFunctionPackageDescriptorResponse' {vnfd} -> vnfd) (\s@GetSolFunctionPackageDescriptorResponse' {} a -> s {vnfd = a} :: GetSolFunctionPackageDescriptorResponse)

-- | The response's http status code.
getSolFunctionPackageDescriptorResponse_httpStatus :: Lens.Lens' GetSolFunctionPackageDescriptorResponse Prelude.Int
getSolFunctionPackageDescriptorResponse_httpStatus = Lens.lens (\GetSolFunctionPackageDescriptorResponse' {httpStatus} -> httpStatus) (\s@GetSolFunctionPackageDescriptorResponse' {} a -> s {httpStatus = a} :: GetSolFunctionPackageDescriptorResponse)

instance
  Prelude.NFData
    GetSolFunctionPackageDescriptorResponse
  where
  rnf GetSolFunctionPackageDescriptorResponse' {..} =
    Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf vnfd
      `Prelude.seq` Prelude.rnf httpStatus
