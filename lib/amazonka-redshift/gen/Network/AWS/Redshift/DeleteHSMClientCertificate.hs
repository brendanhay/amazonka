{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DeleteHSMClientCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified HSM client certificate.
module Network.AWS.Redshift.DeleteHSMClientCertificate
  ( -- * Creating a request
    DeleteHSMClientCertificate (..),
    mkDeleteHSMClientCertificate,

    -- ** Request lenses
    dhsmccHSMClientCertificateIdentifier,

    -- * Destructuring the response
    DeleteHSMClientCertificateResponse (..),
    mkDeleteHSMClientCertificateResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDeleteHSMClientCertificate' smart constructor.
newtype DeleteHSMClientCertificate = DeleteHSMClientCertificate'
  { -- | The identifier of the HSM client certificate to be deleted.
    hsmClientCertificateIdentifier :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteHSMClientCertificate' with the minimum fields required to make a request.
--
-- * 'hsmClientCertificateIdentifier' - The identifier of the HSM client certificate to be deleted.
mkDeleteHSMClientCertificate ::
  -- | 'hsmClientCertificateIdentifier'
  Lude.Text ->
  DeleteHSMClientCertificate
mkDeleteHSMClientCertificate pHSMClientCertificateIdentifier_ =
  DeleteHSMClientCertificate'
    { hsmClientCertificateIdentifier =
        pHSMClientCertificateIdentifier_
    }

-- | The identifier of the HSM client certificate to be deleted.
--
-- /Note:/ Consider using 'hsmClientCertificateIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhsmccHSMClientCertificateIdentifier :: Lens.Lens' DeleteHSMClientCertificate Lude.Text
dhsmccHSMClientCertificateIdentifier = Lens.lens (hsmClientCertificateIdentifier :: DeleteHSMClientCertificate -> Lude.Text) (\s a -> s {hsmClientCertificateIdentifier = a} :: DeleteHSMClientCertificate)
{-# DEPRECATED dhsmccHSMClientCertificateIdentifier "Use generic-lens or generic-optics with 'hsmClientCertificateIdentifier' instead." #-}

instance Lude.AWSRequest DeleteHSMClientCertificate where
  type
    Rs DeleteHSMClientCertificate =
      DeleteHSMClientCertificateResponse
  request = Req.postQuery redshiftService
  response = Res.receiveNull DeleteHSMClientCertificateResponse'

instance Lude.ToHeaders DeleteHSMClientCertificate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteHSMClientCertificate where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteHSMClientCertificate where
  toQuery DeleteHSMClientCertificate' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DeleteHsmClientCertificate" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "HsmClientCertificateIdentifier"
          Lude.=: hsmClientCertificateIdentifier
      ]

-- | /See:/ 'mkDeleteHSMClientCertificateResponse' smart constructor.
data DeleteHSMClientCertificateResponse = DeleteHSMClientCertificateResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteHSMClientCertificateResponse' with the minimum fields required to make a request.
mkDeleteHSMClientCertificateResponse ::
  DeleteHSMClientCertificateResponse
mkDeleteHSMClientCertificateResponse =
  DeleteHSMClientCertificateResponse'
