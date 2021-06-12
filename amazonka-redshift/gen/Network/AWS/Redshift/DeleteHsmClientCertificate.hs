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
-- Module      : Network.AWS.Redshift.DeleteHsmClientCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified HSM client certificate.
module Network.AWS.Redshift.DeleteHsmClientCertificate
  ( -- * Creating a Request
    DeleteHsmClientCertificate (..),
    newDeleteHsmClientCertificate,

    -- * Request Lenses
    deleteHsmClientCertificate_hsmClientCertificateIdentifier,

    -- * Destructuring the Response
    DeleteHsmClientCertificateResponse (..),
    newDeleteHsmClientCertificateResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDeleteHsmClientCertificate' smart constructor.
data DeleteHsmClientCertificate = DeleteHsmClientCertificate'
  { -- | The identifier of the HSM client certificate to be deleted.
    hsmClientCertificateIdentifier :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteHsmClientCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hsmClientCertificateIdentifier', 'deleteHsmClientCertificate_hsmClientCertificateIdentifier' - The identifier of the HSM client certificate to be deleted.
newDeleteHsmClientCertificate ::
  -- | 'hsmClientCertificateIdentifier'
  Core.Text ->
  DeleteHsmClientCertificate
newDeleteHsmClientCertificate
  pHsmClientCertificateIdentifier_ =
    DeleteHsmClientCertificate'
      { hsmClientCertificateIdentifier =
          pHsmClientCertificateIdentifier_
      }

-- | The identifier of the HSM client certificate to be deleted.
deleteHsmClientCertificate_hsmClientCertificateIdentifier :: Lens.Lens' DeleteHsmClientCertificate Core.Text
deleteHsmClientCertificate_hsmClientCertificateIdentifier = Lens.lens (\DeleteHsmClientCertificate' {hsmClientCertificateIdentifier} -> hsmClientCertificateIdentifier) (\s@DeleteHsmClientCertificate' {} a -> s {hsmClientCertificateIdentifier = a} :: DeleteHsmClientCertificate)

instance Core.AWSRequest DeleteHsmClientCertificate where
  type
    AWSResponse DeleteHsmClientCertificate =
      DeleteHsmClientCertificateResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DeleteHsmClientCertificateResponse'

instance Core.Hashable DeleteHsmClientCertificate

instance Core.NFData DeleteHsmClientCertificate

instance Core.ToHeaders DeleteHsmClientCertificate where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteHsmClientCertificate where
  toPath = Core.const "/"

instance Core.ToQuery DeleteHsmClientCertificate where
  toQuery DeleteHsmClientCertificate' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteHsmClientCertificate" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "HsmClientCertificateIdentifier"
          Core.=: hsmClientCertificateIdentifier
      ]

-- | /See:/ 'newDeleteHsmClientCertificateResponse' smart constructor.
data DeleteHsmClientCertificateResponse = DeleteHsmClientCertificateResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteHsmClientCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteHsmClientCertificateResponse ::
  DeleteHsmClientCertificateResponse
newDeleteHsmClientCertificateResponse =
  DeleteHsmClientCertificateResponse'

instance
  Core.NFData
    DeleteHsmClientCertificateResponse
