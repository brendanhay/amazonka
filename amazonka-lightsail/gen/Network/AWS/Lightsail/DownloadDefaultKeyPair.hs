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
-- Module      : Network.AWS.Lightsail.DownloadDefaultKeyPair
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Downloads the default SSH key pair from the user\'s account.
module Network.AWS.Lightsail.DownloadDefaultKeyPair
  ( -- * Creating a Request
    DownloadDefaultKeyPair (..),
    newDownloadDefaultKeyPair,

    -- * Destructuring the Response
    DownloadDefaultKeyPairResponse (..),
    newDownloadDefaultKeyPairResponse,

    -- * Response Lenses
    downloadDefaultKeyPairResponse_privateKeyBase64,
    downloadDefaultKeyPairResponse_publicKeyBase64,
    downloadDefaultKeyPairResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDownloadDefaultKeyPair' smart constructor.
data DownloadDefaultKeyPair = DownloadDefaultKeyPair'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DownloadDefaultKeyPair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDownloadDefaultKeyPair ::
  DownloadDefaultKeyPair
newDownloadDefaultKeyPair = DownloadDefaultKeyPair'

instance Core.AWSRequest DownloadDefaultKeyPair where
  type
    AWSResponse DownloadDefaultKeyPair =
      DownloadDefaultKeyPairResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DownloadDefaultKeyPairResponse'
            Core.<$> (x Core..?> "privateKeyBase64")
            Core.<*> (x Core..?> "publicKeyBase64")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DownloadDefaultKeyPair

instance Core.NFData DownloadDefaultKeyPair

instance Core.ToHeaders DownloadDefaultKeyPair where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.DownloadDefaultKeyPair" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DownloadDefaultKeyPair where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath DownloadDefaultKeyPair where
  toPath = Core.const "/"

instance Core.ToQuery DownloadDefaultKeyPair where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDownloadDefaultKeyPairResponse' smart constructor.
data DownloadDefaultKeyPairResponse = DownloadDefaultKeyPairResponse'
  { -- | A base64-encoded RSA private key.
    privateKeyBase64 :: Core.Maybe Core.Text,
    -- | A base64-encoded public key of the @ssh-rsa@ type.
    publicKeyBase64 :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DownloadDefaultKeyPairResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'privateKeyBase64', 'downloadDefaultKeyPairResponse_privateKeyBase64' - A base64-encoded RSA private key.
--
-- 'publicKeyBase64', 'downloadDefaultKeyPairResponse_publicKeyBase64' - A base64-encoded public key of the @ssh-rsa@ type.
--
-- 'httpStatus', 'downloadDefaultKeyPairResponse_httpStatus' - The response's http status code.
newDownloadDefaultKeyPairResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DownloadDefaultKeyPairResponse
newDownloadDefaultKeyPairResponse pHttpStatus_ =
  DownloadDefaultKeyPairResponse'
    { privateKeyBase64 =
        Core.Nothing,
      publicKeyBase64 = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A base64-encoded RSA private key.
downloadDefaultKeyPairResponse_privateKeyBase64 :: Lens.Lens' DownloadDefaultKeyPairResponse (Core.Maybe Core.Text)
downloadDefaultKeyPairResponse_privateKeyBase64 = Lens.lens (\DownloadDefaultKeyPairResponse' {privateKeyBase64} -> privateKeyBase64) (\s@DownloadDefaultKeyPairResponse' {} a -> s {privateKeyBase64 = a} :: DownloadDefaultKeyPairResponse)

-- | A base64-encoded public key of the @ssh-rsa@ type.
downloadDefaultKeyPairResponse_publicKeyBase64 :: Lens.Lens' DownloadDefaultKeyPairResponse (Core.Maybe Core.Text)
downloadDefaultKeyPairResponse_publicKeyBase64 = Lens.lens (\DownloadDefaultKeyPairResponse' {publicKeyBase64} -> publicKeyBase64) (\s@DownloadDefaultKeyPairResponse' {} a -> s {publicKeyBase64 = a} :: DownloadDefaultKeyPairResponse)

-- | The response's http status code.
downloadDefaultKeyPairResponse_httpStatus :: Lens.Lens' DownloadDefaultKeyPairResponse Core.Int
downloadDefaultKeyPairResponse_httpStatus = Lens.lens (\DownloadDefaultKeyPairResponse' {httpStatus} -> httpStatus) (\s@DownloadDefaultKeyPairResponse' {} a -> s {httpStatus = a} :: DownloadDefaultKeyPairResponse)

instance Core.NFData DownloadDefaultKeyPairResponse
