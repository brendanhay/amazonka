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
-- Module      : Amazonka.TNB.GetSolNetworkPackageDescriptor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the content of the network service descriptor.
--
-- A network service descriptor is a .yaml file in a network package that
-- uses the TOSCA standard to describe the network functions you want to
-- deploy and the Amazon Web Services infrastructure you want to deploy the
-- network functions on.
module Amazonka.TNB.GetSolNetworkPackageDescriptor
  ( -- * Creating a Request
    GetSolNetworkPackageDescriptor (..),
    newGetSolNetworkPackageDescriptor,

    -- * Request Lenses
    getSolNetworkPackageDescriptor_nsdInfoId,

    -- * Destructuring the Response
    GetSolNetworkPackageDescriptorResponse (..),
    newGetSolNetworkPackageDescriptorResponse,

    -- * Response Lenses
    getSolNetworkPackageDescriptorResponse_contentType,
    getSolNetworkPackageDescriptorResponse_nsd,
    getSolNetworkPackageDescriptorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TNB.Types

-- | /See:/ 'newGetSolNetworkPackageDescriptor' smart constructor.
data GetSolNetworkPackageDescriptor = GetSolNetworkPackageDescriptor'
  { -- | ID of the network service descriptor in the network package.
    nsdInfoId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSolNetworkPackageDescriptor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nsdInfoId', 'getSolNetworkPackageDescriptor_nsdInfoId' - ID of the network service descriptor in the network package.
newGetSolNetworkPackageDescriptor ::
  -- | 'nsdInfoId'
  Prelude.Text ->
  GetSolNetworkPackageDescriptor
newGetSolNetworkPackageDescriptor pNsdInfoId_ =
  GetSolNetworkPackageDescriptor'
    { nsdInfoId =
        pNsdInfoId_
    }

-- | ID of the network service descriptor in the network package.
getSolNetworkPackageDescriptor_nsdInfoId :: Lens.Lens' GetSolNetworkPackageDescriptor Prelude.Text
getSolNetworkPackageDescriptor_nsdInfoId = Lens.lens (\GetSolNetworkPackageDescriptor' {nsdInfoId} -> nsdInfoId) (\s@GetSolNetworkPackageDescriptor' {} a -> s {nsdInfoId = a} :: GetSolNetworkPackageDescriptor)

instance
  Core.AWSRequest
    GetSolNetworkPackageDescriptor
  where
  type
    AWSResponse GetSolNetworkPackageDescriptor =
      GetSolNetworkPackageDescriptorResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveBytes
      ( \s h x ->
          GetSolNetworkPackageDescriptorResponse'
            Prelude.<$> (h Data..#? "Content-Type")
            Prelude.<*> (Prelude.pure (Prelude.Just (Prelude.coerce x)))
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetSolNetworkPackageDescriptor
  where
  hashWithSalt
    _salt
    GetSolNetworkPackageDescriptor' {..} =
      _salt `Prelude.hashWithSalt` nsdInfoId

instance
  Prelude.NFData
    GetSolNetworkPackageDescriptor
  where
  rnf GetSolNetworkPackageDescriptor' {..} =
    Prelude.rnf nsdInfoId

instance
  Data.ToHeaders
    GetSolNetworkPackageDescriptor
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

instance Data.ToPath GetSolNetworkPackageDescriptor where
  toPath GetSolNetworkPackageDescriptor' {..} =
    Prelude.mconcat
      [ "/sol/nsd/v1/ns_descriptors/",
        Data.toBS nsdInfoId,
        "/nsd"
      ]

instance Data.ToQuery GetSolNetworkPackageDescriptor where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSolNetworkPackageDescriptorResponse' smart constructor.
data GetSolNetworkPackageDescriptorResponse = GetSolNetworkPackageDescriptorResponse'
  { -- | Indicates the media type of the resource.
    contentType :: Prelude.Maybe DescriptorContentType,
    -- | Contents of the network service descriptor in the network package.
    nsd :: Prelude.Maybe Prelude.ByteString,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSolNetworkPackageDescriptorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentType', 'getSolNetworkPackageDescriptorResponse_contentType' - Indicates the media type of the resource.
--
-- 'nsd', 'getSolNetworkPackageDescriptorResponse_nsd' - Contents of the network service descriptor in the network package.
--
-- 'httpStatus', 'getSolNetworkPackageDescriptorResponse_httpStatus' - The response's http status code.
newGetSolNetworkPackageDescriptorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSolNetworkPackageDescriptorResponse
newGetSolNetworkPackageDescriptorResponse
  pHttpStatus_ =
    GetSolNetworkPackageDescriptorResponse'
      { contentType =
          Prelude.Nothing,
        nsd = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Indicates the media type of the resource.
getSolNetworkPackageDescriptorResponse_contentType :: Lens.Lens' GetSolNetworkPackageDescriptorResponse (Prelude.Maybe DescriptorContentType)
getSolNetworkPackageDescriptorResponse_contentType = Lens.lens (\GetSolNetworkPackageDescriptorResponse' {contentType} -> contentType) (\s@GetSolNetworkPackageDescriptorResponse' {} a -> s {contentType = a} :: GetSolNetworkPackageDescriptorResponse)

-- | Contents of the network service descriptor in the network package.
getSolNetworkPackageDescriptorResponse_nsd :: Lens.Lens' GetSolNetworkPackageDescriptorResponse (Prelude.Maybe Prelude.ByteString)
getSolNetworkPackageDescriptorResponse_nsd = Lens.lens (\GetSolNetworkPackageDescriptorResponse' {nsd} -> nsd) (\s@GetSolNetworkPackageDescriptorResponse' {} a -> s {nsd = a} :: GetSolNetworkPackageDescriptorResponse)

-- | The response's http status code.
getSolNetworkPackageDescriptorResponse_httpStatus :: Lens.Lens' GetSolNetworkPackageDescriptorResponse Prelude.Int
getSolNetworkPackageDescriptorResponse_httpStatus = Lens.lens (\GetSolNetworkPackageDescriptorResponse' {httpStatus} -> httpStatus) (\s@GetSolNetworkPackageDescriptorResponse' {} a -> s {httpStatus = a} :: GetSolNetworkPackageDescriptorResponse)

instance
  Prelude.NFData
    GetSolNetworkPackageDescriptorResponse
  where
  rnf GetSolNetworkPackageDescriptorResponse' {..} =
    Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf nsd
      `Prelude.seq` Prelude.rnf httpStatus
