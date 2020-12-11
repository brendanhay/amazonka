{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateProvisioningClaim
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a provisioning claim.
module Network.AWS.IoT.CreateProvisioningClaim
  ( -- * Creating a request
    CreateProvisioningClaim (..),
    mkCreateProvisioningClaim,

    -- ** Request lenses
    cpcTemplateName,

    -- * Destructuring the response
    CreateProvisioningClaimResponse (..),
    mkCreateProvisioningClaimResponse,

    -- ** Response lenses
    cpcrsKeyPair,
    cpcrsCertificatePem,
    cpcrsCertificateId,
    cpcrsExpiration,
    cpcrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateProvisioningClaim' smart constructor.
newtype CreateProvisioningClaim = CreateProvisioningClaim'
  { templateName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateProvisioningClaim' with the minimum fields required to make a request.
--
-- * 'templateName' - The name of the provisioning template to use.
mkCreateProvisioningClaim ::
  -- | 'templateName'
  Lude.Text ->
  CreateProvisioningClaim
mkCreateProvisioningClaim pTemplateName_ =
  CreateProvisioningClaim' {templateName = pTemplateName_}

-- | The name of the provisioning template to use.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcTemplateName :: Lens.Lens' CreateProvisioningClaim Lude.Text
cpcTemplateName = Lens.lens (templateName :: CreateProvisioningClaim -> Lude.Text) (\s a -> s {templateName = a} :: CreateProvisioningClaim)
{-# DEPRECATED cpcTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

instance Lude.AWSRequest CreateProvisioningClaim where
  type Rs CreateProvisioningClaim = CreateProvisioningClaimResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateProvisioningClaimResponse'
            Lude.<$> (x Lude..?> "keyPair")
            Lude.<*> (x Lude..?> "certificatePem")
            Lude.<*> (x Lude..?> "certificateId")
            Lude.<*> (x Lude..?> "expiration")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateProvisioningClaim where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateProvisioningClaim where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath CreateProvisioningClaim where
  toPath CreateProvisioningClaim' {..} =
    Lude.mconcat
      [ "/provisioning-templates/",
        Lude.toBS templateName,
        "/provisioning-claim"
      ]

instance Lude.ToQuery CreateProvisioningClaim where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateProvisioningClaimResponse' smart constructor.
data CreateProvisioningClaimResponse = CreateProvisioningClaimResponse'
  { keyPair ::
      Lude.Maybe KeyPair,
    certificatePem ::
      Lude.Maybe Lude.Text,
    certificateId ::
      Lude.Maybe Lude.Text,
    expiration ::
      Lude.Maybe Lude.Timestamp,
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateProvisioningClaimResponse' with the minimum fields required to make a request.
--
-- * 'certificateId' - The ID of the certificate.
-- * 'certificatePem' - The provisioning claim certificate.
-- * 'expiration' - The provisioning claim expiration time.
-- * 'keyPair' - The provisioning claim key pair.
-- * 'responseStatus' - The response status code.
mkCreateProvisioningClaimResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateProvisioningClaimResponse
mkCreateProvisioningClaimResponse pResponseStatus_ =
  CreateProvisioningClaimResponse'
    { keyPair = Lude.Nothing,
      certificatePem = Lude.Nothing,
      certificateId = Lude.Nothing,
      expiration = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The provisioning claim key pair.
--
-- /Note:/ Consider using 'keyPair' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcrsKeyPair :: Lens.Lens' CreateProvisioningClaimResponse (Lude.Maybe KeyPair)
cpcrsKeyPair = Lens.lens (keyPair :: CreateProvisioningClaimResponse -> Lude.Maybe KeyPair) (\s a -> s {keyPair = a} :: CreateProvisioningClaimResponse)
{-# DEPRECATED cpcrsKeyPair "Use generic-lens or generic-optics with 'keyPair' instead." #-}

-- | The provisioning claim certificate.
--
-- /Note:/ Consider using 'certificatePem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcrsCertificatePem :: Lens.Lens' CreateProvisioningClaimResponse (Lude.Maybe Lude.Text)
cpcrsCertificatePem = Lens.lens (certificatePem :: CreateProvisioningClaimResponse -> Lude.Maybe Lude.Text) (\s a -> s {certificatePem = a} :: CreateProvisioningClaimResponse)
{-# DEPRECATED cpcrsCertificatePem "Use generic-lens or generic-optics with 'certificatePem' instead." #-}

-- | The ID of the certificate.
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcrsCertificateId :: Lens.Lens' CreateProvisioningClaimResponse (Lude.Maybe Lude.Text)
cpcrsCertificateId = Lens.lens (certificateId :: CreateProvisioningClaimResponse -> Lude.Maybe Lude.Text) (\s a -> s {certificateId = a} :: CreateProvisioningClaimResponse)
{-# DEPRECATED cpcrsCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

-- | The provisioning claim expiration time.
--
-- /Note:/ Consider using 'expiration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcrsExpiration :: Lens.Lens' CreateProvisioningClaimResponse (Lude.Maybe Lude.Timestamp)
cpcrsExpiration = Lens.lens (expiration :: CreateProvisioningClaimResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {expiration = a} :: CreateProvisioningClaimResponse)
{-# DEPRECATED cpcrsExpiration "Use generic-lens or generic-optics with 'expiration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcrsResponseStatus :: Lens.Lens' CreateProvisioningClaimResponse Lude.Int
cpcrsResponseStatus = Lens.lens (responseStatus :: CreateProvisioningClaimResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateProvisioningClaimResponse)
{-# DEPRECATED cpcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
