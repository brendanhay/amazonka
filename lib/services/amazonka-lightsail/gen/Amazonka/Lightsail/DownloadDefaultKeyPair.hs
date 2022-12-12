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
-- Module      : Amazonka.Lightsail.DownloadDefaultKeyPair
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Downloads the regional Amazon Lightsail default key pair.
--
-- This action also creates a Lightsail default key pair if a default key
-- pair does not currently exist in the Amazon Web Services Region.
module Amazonka.Lightsail.DownloadDefaultKeyPair
  ( -- * Creating a Request
    DownloadDefaultKeyPair (..),
    newDownloadDefaultKeyPair,

    -- * Destructuring the Response
    DownloadDefaultKeyPairResponse (..),
    newDownloadDefaultKeyPairResponse,

    -- * Response Lenses
    downloadDefaultKeyPairResponse_createdAt,
    downloadDefaultKeyPairResponse_privateKeyBase64,
    downloadDefaultKeyPairResponse_publicKeyBase64,
    downloadDefaultKeyPairResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDownloadDefaultKeyPair' smart constructor.
data DownloadDefaultKeyPair = DownloadDefaultKeyPair'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DownloadDefaultKeyPairResponse'
            Prelude.<$> (x Data..?> "createdAt")
            Prelude.<*> (x Data..?> "privateKeyBase64")
            Prelude.<*> (x Data..?> "publicKeyBase64")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DownloadDefaultKeyPair where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DownloadDefaultKeyPair where
  rnf _ = ()

instance Data.ToHeaders DownloadDefaultKeyPair where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.DownloadDefaultKeyPair" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DownloadDefaultKeyPair where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath DownloadDefaultKeyPair where
  toPath = Prelude.const "/"

instance Data.ToQuery DownloadDefaultKeyPair where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDownloadDefaultKeyPairResponse' smart constructor.
data DownloadDefaultKeyPairResponse = DownloadDefaultKeyPairResponse'
  { -- | The timestamp when the default key pair was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | A base64-encoded RSA private key.
    privateKeyBase64 :: Prelude.Maybe Prelude.Text,
    -- | A base64-encoded public key of the @ssh-rsa@ type.
    publicKeyBase64 :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DownloadDefaultKeyPairResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'downloadDefaultKeyPairResponse_createdAt' - The timestamp when the default key pair was created.
--
-- 'privateKeyBase64', 'downloadDefaultKeyPairResponse_privateKeyBase64' - A base64-encoded RSA private key.
--
-- 'publicKeyBase64', 'downloadDefaultKeyPairResponse_publicKeyBase64' - A base64-encoded public key of the @ssh-rsa@ type.
--
-- 'httpStatus', 'downloadDefaultKeyPairResponse_httpStatus' - The response's http status code.
newDownloadDefaultKeyPairResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DownloadDefaultKeyPairResponse
newDownloadDefaultKeyPairResponse pHttpStatus_ =
  DownloadDefaultKeyPairResponse'
    { createdAt =
        Prelude.Nothing,
      privateKeyBase64 = Prelude.Nothing,
      publicKeyBase64 = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The timestamp when the default key pair was created.
downloadDefaultKeyPairResponse_createdAt :: Lens.Lens' DownloadDefaultKeyPairResponse (Prelude.Maybe Prelude.UTCTime)
downloadDefaultKeyPairResponse_createdAt = Lens.lens (\DownloadDefaultKeyPairResponse' {createdAt} -> createdAt) (\s@DownloadDefaultKeyPairResponse' {} a -> s {createdAt = a} :: DownloadDefaultKeyPairResponse) Prelude.. Lens.mapping Data._Time

-- | A base64-encoded RSA private key.
downloadDefaultKeyPairResponse_privateKeyBase64 :: Lens.Lens' DownloadDefaultKeyPairResponse (Prelude.Maybe Prelude.Text)
downloadDefaultKeyPairResponse_privateKeyBase64 = Lens.lens (\DownloadDefaultKeyPairResponse' {privateKeyBase64} -> privateKeyBase64) (\s@DownloadDefaultKeyPairResponse' {} a -> s {privateKeyBase64 = a} :: DownloadDefaultKeyPairResponse)

-- | A base64-encoded public key of the @ssh-rsa@ type.
downloadDefaultKeyPairResponse_publicKeyBase64 :: Lens.Lens' DownloadDefaultKeyPairResponse (Prelude.Maybe Prelude.Text)
downloadDefaultKeyPairResponse_publicKeyBase64 = Lens.lens (\DownloadDefaultKeyPairResponse' {publicKeyBase64} -> publicKeyBase64) (\s@DownloadDefaultKeyPairResponse' {} a -> s {publicKeyBase64 = a} :: DownloadDefaultKeyPairResponse)

-- | The response's http status code.
downloadDefaultKeyPairResponse_httpStatus :: Lens.Lens' DownloadDefaultKeyPairResponse Prelude.Int
downloadDefaultKeyPairResponse_httpStatus = Lens.lens (\DownloadDefaultKeyPairResponse' {httpStatus} -> httpStatus) (\s@DownloadDefaultKeyPairResponse' {} a -> s {httpStatus = a} :: DownloadDefaultKeyPairResponse)

instance
  Prelude.NFData
    DownloadDefaultKeyPairResponse
  where
  rnf DownloadDefaultKeyPairResponse' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf privateKeyBase64
      `Prelude.seq` Prelude.rnf publicKeyBase64
      `Prelude.seq` Prelude.rnf httpStatus
