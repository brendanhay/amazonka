{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.UpdateCertificateOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a certificate. Currently, you can use this function to specify whether to opt in to or out of recording your certificate in a certificate transparency log. For more information, see <https://docs.aws.amazon.com/acm/latest/userguide/acm-bestpractices.html#best-practices-transparency Opting Out of Certificate Transparency Logging> .
module Network.AWS.CertificateManager.UpdateCertificateOptions
  ( -- * Creating a request
    UpdateCertificateOptions (..),
    mkUpdateCertificateOptions,

    -- ** Request lenses
    ucoCertificateARN,
    ucoOptions,

    -- * Destructuring the response
    UpdateCertificateOptionsResponse (..),
    mkUpdateCertificateOptionsResponse,
  )
where

import Network.AWS.CertificateManager.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateCertificateOptions' smart constructor.
data UpdateCertificateOptions = UpdateCertificateOptions'
  { -- | ARN of the requested certificate to update. This must be of the form:
    --
    -- @arn:aws:acm:us-east-1:/account/ :certificate//12345678-1234-1234-1234-123456789012/ @
    certificateARN :: Lude.Text,
    -- | Use to update the options for your certificate. Currently, you can specify whether to add your certificate to a transparency log. Certificate transparency makes it possible to detect SSL/TLS certificates that have been mistakenly or maliciously issued. Certificates that have not been logged typically produce an error message in a browser.
    options :: CertificateOptions
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateCertificateOptions' with the minimum fields required to make a request.
--
-- * 'certificateARN' - ARN of the requested certificate to update. This must be of the form:
--
-- @arn:aws:acm:us-east-1:/account/ :certificate//12345678-1234-1234-1234-123456789012/ @
-- * 'options' - Use to update the options for your certificate. Currently, you can specify whether to add your certificate to a transparency log. Certificate transparency makes it possible to detect SSL/TLS certificates that have been mistakenly or maliciously issued. Certificates that have not been logged typically produce an error message in a browser.
mkUpdateCertificateOptions ::
  -- | 'certificateARN'
  Lude.Text ->
  -- | 'options'
  CertificateOptions ->
  UpdateCertificateOptions
mkUpdateCertificateOptions pCertificateARN_ pOptions_ =
  UpdateCertificateOptions'
    { certificateARN = pCertificateARN_,
      options = pOptions_
    }

-- | ARN of the requested certificate to update. This must be of the form:
--
-- @arn:aws:acm:us-east-1:/account/ :certificate//12345678-1234-1234-1234-123456789012/ @
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucoCertificateARN :: Lens.Lens' UpdateCertificateOptions Lude.Text
ucoCertificateARN = Lens.lens (certificateARN :: UpdateCertificateOptions -> Lude.Text) (\s a -> s {certificateARN = a} :: UpdateCertificateOptions)
{-# DEPRECATED ucoCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | Use to update the options for your certificate. Currently, you can specify whether to add your certificate to a transparency log. Certificate transparency makes it possible to detect SSL/TLS certificates that have been mistakenly or maliciously issued. Certificates that have not been logged typically produce an error message in a browser.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucoOptions :: Lens.Lens' UpdateCertificateOptions CertificateOptions
ucoOptions = Lens.lens (options :: UpdateCertificateOptions -> CertificateOptions) (\s a -> s {options = a} :: UpdateCertificateOptions)
{-# DEPRECATED ucoOptions "Use generic-lens or generic-optics with 'options' instead." #-}

instance Lude.AWSRequest UpdateCertificateOptions where
  type Rs UpdateCertificateOptions = UpdateCertificateOptionsResponse
  request = Req.postJSON certificateManagerService
  response = Res.receiveNull UpdateCertificateOptionsResponse'

instance Lude.ToHeaders UpdateCertificateOptions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CertificateManager.UpdateCertificateOptions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateCertificateOptions where
  toJSON UpdateCertificateOptions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("CertificateArn" Lude..= certificateARN),
            Lude.Just ("Options" Lude..= options)
          ]
      )

instance Lude.ToPath UpdateCertificateOptions where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateCertificateOptions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateCertificateOptionsResponse' smart constructor.
data UpdateCertificateOptionsResponse = UpdateCertificateOptionsResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateCertificateOptionsResponse' with the minimum fields required to make a request.
mkUpdateCertificateOptionsResponse ::
  UpdateCertificateOptionsResponse
mkUpdateCertificateOptionsResponse =
  UpdateCertificateOptionsResponse'
