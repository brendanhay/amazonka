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
-- Module      : Network.AWS.CloudFront.GetPublicKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a public key.
module Network.AWS.CloudFront.GetPublicKey
  ( -- * Creating a Request
    GetPublicKey (..),
    newGetPublicKey,

    -- * Request Lenses
    getPublicKey_id,

    -- * Destructuring the Response
    GetPublicKeyResponse (..),
    newGetPublicKeyResponse,

    -- * Response Lenses
    getPublicKeyResponse_eTag,
    getPublicKeyResponse_publicKey,
    getPublicKeyResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetPublicKey' smart constructor.
data GetPublicKey = GetPublicKey'
  { -- | The identifier of the public key you are getting.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetPublicKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getPublicKey_id' - The identifier of the public key you are getting.
newGetPublicKey ::
  -- | 'id'
  Core.Text ->
  GetPublicKey
newGetPublicKey pId_ = GetPublicKey' {id = pId_}

-- | The identifier of the public key you are getting.
getPublicKey_id :: Lens.Lens' GetPublicKey Core.Text
getPublicKey_id = Lens.lens (\GetPublicKey' {id} -> id) (\s@GetPublicKey' {} a -> s {id = a} :: GetPublicKey)

instance Core.AWSRequest GetPublicKey where
  type AWSResponse GetPublicKey = GetPublicKeyResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetPublicKeyResponse'
            Core.<$> (h Core..#? "ETag")
            Core.<*> (Core.parseXML x)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetPublicKey

instance Core.NFData GetPublicKey

instance Core.ToHeaders GetPublicKey where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetPublicKey where
  toPath GetPublicKey' {..} =
    Core.mconcat
      ["/2020-05-31/public-key/", Core.toBS id]

instance Core.ToQuery GetPublicKey where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetPublicKeyResponse' smart constructor.
data GetPublicKeyResponse = GetPublicKeyResponse'
  { -- | The identifier for this version of the public key.
    eTag :: Core.Maybe Core.Text,
    -- | The public key.
    publicKey :: Core.Maybe PublicKey,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetPublicKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'getPublicKeyResponse_eTag' - The identifier for this version of the public key.
--
-- 'publicKey', 'getPublicKeyResponse_publicKey' - The public key.
--
-- 'httpStatus', 'getPublicKeyResponse_httpStatus' - The response's http status code.
newGetPublicKeyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetPublicKeyResponse
newGetPublicKeyResponse pHttpStatus_ =
  GetPublicKeyResponse'
    { eTag = Core.Nothing,
      publicKey = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier for this version of the public key.
getPublicKeyResponse_eTag :: Lens.Lens' GetPublicKeyResponse (Core.Maybe Core.Text)
getPublicKeyResponse_eTag = Lens.lens (\GetPublicKeyResponse' {eTag} -> eTag) (\s@GetPublicKeyResponse' {} a -> s {eTag = a} :: GetPublicKeyResponse)

-- | The public key.
getPublicKeyResponse_publicKey :: Lens.Lens' GetPublicKeyResponse (Core.Maybe PublicKey)
getPublicKeyResponse_publicKey = Lens.lens (\GetPublicKeyResponse' {publicKey} -> publicKey) (\s@GetPublicKeyResponse' {} a -> s {publicKey = a} :: GetPublicKeyResponse)

-- | The response's http status code.
getPublicKeyResponse_httpStatus :: Lens.Lens' GetPublicKeyResponse Core.Int
getPublicKeyResponse_httpStatus = Lens.lens (\GetPublicKeyResponse' {httpStatus} -> httpStatus) (\s@GetPublicKeyResponse' {} a -> s {httpStatus = a} :: GetPublicKeyResponse)

instance Core.NFData GetPublicKeyResponse
