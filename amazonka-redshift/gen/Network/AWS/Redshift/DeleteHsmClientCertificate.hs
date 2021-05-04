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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDeleteHsmClientCertificate' smart constructor.
data DeleteHsmClientCertificate = DeleteHsmClientCertificate'
  { -- | The identifier of the HSM client certificate to be deleted.
    hsmClientCertificateIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteHsmClientCertificate
newDeleteHsmClientCertificate
  pHsmClientCertificateIdentifier_ =
    DeleteHsmClientCertificate'
      { hsmClientCertificateIdentifier =
          pHsmClientCertificateIdentifier_
      }

-- | The identifier of the HSM client certificate to be deleted.
deleteHsmClientCertificate_hsmClientCertificateIdentifier :: Lens.Lens' DeleteHsmClientCertificate Prelude.Text
deleteHsmClientCertificate_hsmClientCertificateIdentifier = Lens.lens (\DeleteHsmClientCertificate' {hsmClientCertificateIdentifier} -> hsmClientCertificateIdentifier) (\s@DeleteHsmClientCertificate' {} a -> s {hsmClientCertificateIdentifier = a} :: DeleteHsmClientCertificate)

instance
  Prelude.AWSRequest
    DeleteHsmClientCertificate
  where
  type
    Rs DeleteHsmClientCertificate =
      DeleteHsmClientCertificateResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DeleteHsmClientCertificateResponse'

instance Prelude.Hashable DeleteHsmClientCertificate

instance Prelude.NFData DeleteHsmClientCertificate

instance Prelude.ToHeaders DeleteHsmClientCertificate where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteHsmClientCertificate where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteHsmClientCertificate where
  toQuery DeleteHsmClientCertificate' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteHsmClientCertificate" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2012-12-01" :: Prelude.ByteString),
        "HsmClientCertificateIdentifier"
          Prelude.=: hsmClientCertificateIdentifier
      ]

-- | /See:/ 'newDeleteHsmClientCertificateResponse' smart constructor.
data DeleteHsmClientCertificateResponse = DeleteHsmClientCertificateResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteHsmClientCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteHsmClientCertificateResponse ::
  DeleteHsmClientCertificateResponse
newDeleteHsmClientCertificateResponse =
  DeleteHsmClientCertificateResponse'

instance
  Prelude.NFData
    DeleteHsmClientCertificateResponse
