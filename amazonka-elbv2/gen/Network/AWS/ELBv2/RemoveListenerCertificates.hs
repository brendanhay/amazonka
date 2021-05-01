{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ELBv2.RemoveListenerCertificates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified certificate from the certificate list for the
-- specified HTTPS or TLS listener.
module Network.AWS.ELBv2.RemoveListenerCertificates
  ( -- * Creating a Request
    RemoveListenerCertificates (..),
    newRemoveListenerCertificates,

    -- * Request Lenses
    removeListenerCertificates_listenerArn,
    removeListenerCertificates_certificates,

    -- * Destructuring the Response
    RemoveListenerCertificatesResponse (..),
    newRemoveListenerCertificatesResponse,

    -- * Response Lenses
    removeListenerCertificatesResponse_httpStatus,
  )
where

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRemoveListenerCertificates' smart constructor.
data RemoveListenerCertificates = RemoveListenerCertificates'
  { -- | The Amazon Resource Name (ARN) of the listener.
    listenerArn :: Prelude.Text,
    -- | The certificate to remove. You can specify one certificate per call. Set
    -- @CertificateArn@ to the certificate ARN but do not set @IsDefault@.
    certificates :: [Certificate]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RemoveListenerCertificates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'listenerArn', 'removeListenerCertificates_listenerArn' - The Amazon Resource Name (ARN) of the listener.
--
-- 'certificates', 'removeListenerCertificates_certificates' - The certificate to remove. You can specify one certificate per call. Set
-- @CertificateArn@ to the certificate ARN but do not set @IsDefault@.
newRemoveListenerCertificates ::
  -- | 'listenerArn'
  Prelude.Text ->
  RemoveListenerCertificates
newRemoveListenerCertificates pListenerArn_ =
  RemoveListenerCertificates'
    { listenerArn =
        pListenerArn_,
      certificates = Prelude.mempty
    }

-- | The Amazon Resource Name (ARN) of the listener.
removeListenerCertificates_listenerArn :: Lens.Lens' RemoveListenerCertificates Prelude.Text
removeListenerCertificates_listenerArn = Lens.lens (\RemoveListenerCertificates' {listenerArn} -> listenerArn) (\s@RemoveListenerCertificates' {} a -> s {listenerArn = a} :: RemoveListenerCertificates)

-- | The certificate to remove. You can specify one certificate per call. Set
-- @CertificateArn@ to the certificate ARN but do not set @IsDefault@.
removeListenerCertificates_certificates :: Lens.Lens' RemoveListenerCertificates [Certificate]
removeListenerCertificates_certificates = Lens.lens (\RemoveListenerCertificates' {certificates} -> certificates) (\s@RemoveListenerCertificates' {} a -> s {certificates = a} :: RemoveListenerCertificates) Prelude.. Prelude._Coerce

instance
  Prelude.AWSRequest
    RemoveListenerCertificates
  where
  type
    Rs RemoveListenerCertificates =
      RemoveListenerCertificatesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "RemoveListenerCertificatesResult"
      ( \s h x ->
          RemoveListenerCertificatesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveListenerCertificates

instance Prelude.NFData RemoveListenerCertificates

instance Prelude.ToHeaders RemoveListenerCertificates where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath RemoveListenerCertificates where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RemoveListenerCertificates where
  toQuery RemoveListenerCertificates' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("RemoveListenerCertificates" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2015-12-01" :: Prelude.ByteString),
        "ListenerArn" Prelude.=: listenerArn,
        "Certificates"
          Prelude.=: Prelude.toQueryList "member" certificates
      ]

-- | /See:/ 'newRemoveListenerCertificatesResponse' smart constructor.
data RemoveListenerCertificatesResponse = RemoveListenerCertificatesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RemoveListenerCertificatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'removeListenerCertificatesResponse_httpStatus' - The response's http status code.
newRemoveListenerCertificatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RemoveListenerCertificatesResponse
newRemoveListenerCertificatesResponse pHttpStatus_ =
  RemoveListenerCertificatesResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
removeListenerCertificatesResponse_httpStatus :: Lens.Lens' RemoveListenerCertificatesResponse Prelude.Int
removeListenerCertificatesResponse_httpStatus = Lens.lens (\RemoveListenerCertificatesResponse' {httpStatus} -> httpStatus) (\s@RemoveListenerCertificatesResponse' {} a -> s {httpStatus = a} :: RemoveListenerCertificatesResponse)

instance
  Prelude.NFData
    RemoveListenerCertificatesResponse
