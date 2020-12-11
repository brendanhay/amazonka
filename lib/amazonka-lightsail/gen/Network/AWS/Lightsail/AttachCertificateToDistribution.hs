{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.AttachCertificateToDistribution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches an SSL/TLS certificate to your Amazon Lightsail content delivery network (CDN) distribution.
--
-- After the certificate is attached, your distribution accepts HTTPS traffic for all of the domains that are associated with the certificate.
-- Use the @CreateCertificate@ action to create a certificate that you can attach to your distribution.
-- /Important:/ Only certificates created in the @us-east-1@ AWS Region can be attached to Lightsail distributions. Lightsail distributions are global resources that can reference an origin in any AWS Region, and distribute its content globally. However, all distributions are located in the @us-east-1@ Region.
module Network.AWS.Lightsail.AttachCertificateToDistribution
  ( -- * Creating a request
    AttachCertificateToDistribution (..),
    mkAttachCertificateToDistribution,

    -- ** Request lenses
    actdDistributionName,
    actdCertificateName,

    -- * Destructuring the response
    AttachCertificateToDistributionResponse (..),
    mkAttachCertificateToDistributionResponse,

    -- ** Response lenses
    actdrsOperation,
    actdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAttachCertificateToDistribution' smart constructor.
data AttachCertificateToDistribution = AttachCertificateToDistribution'
  { distributionName ::
      Lude.Text,
    certificateName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachCertificateToDistribution' with the minimum fields required to make a request.
--
-- * 'certificateName' - The name of the certificate to attach to a distribution.
--
-- Only certificates with a status of @ISSUED@ can be attached to a distribution.
-- Use the @GetCertificates@ action to get a list of certificate names that you can specify.
-- * 'distributionName' - The name of the distribution that the certificate will be attached to.
--
-- Use the @GetDistributions@ action to get a list of distribution names that you can specify.
mkAttachCertificateToDistribution ::
  -- | 'distributionName'
  Lude.Text ->
  -- | 'certificateName'
  Lude.Text ->
  AttachCertificateToDistribution
mkAttachCertificateToDistribution
  pDistributionName_
  pCertificateName_ =
    AttachCertificateToDistribution'
      { distributionName =
          pDistributionName_,
        certificateName = pCertificateName_
      }

-- | The name of the distribution that the certificate will be attached to.
--
-- Use the @GetDistributions@ action to get a list of distribution names that you can specify.
--
-- /Note:/ Consider using 'distributionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
actdDistributionName :: Lens.Lens' AttachCertificateToDistribution Lude.Text
actdDistributionName = Lens.lens (distributionName :: AttachCertificateToDistribution -> Lude.Text) (\s a -> s {distributionName = a} :: AttachCertificateToDistribution)
{-# DEPRECATED actdDistributionName "Use generic-lens or generic-optics with 'distributionName' instead." #-}

-- | The name of the certificate to attach to a distribution.
--
-- Only certificates with a status of @ISSUED@ can be attached to a distribution.
-- Use the @GetCertificates@ action to get a list of certificate names that you can specify.
--
-- /Note:/ Consider using 'certificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
actdCertificateName :: Lens.Lens' AttachCertificateToDistribution Lude.Text
actdCertificateName = Lens.lens (certificateName :: AttachCertificateToDistribution -> Lude.Text) (\s a -> s {certificateName = a} :: AttachCertificateToDistribution)
{-# DEPRECATED actdCertificateName "Use generic-lens or generic-optics with 'certificateName' instead." #-}

instance Lude.AWSRequest AttachCertificateToDistribution where
  type
    Rs AttachCertificateToDistribution =
      AttachCertificateToDistributionResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          AttachCertificateToDistributionResponse'
            Lude.<$> (x Lude..?> "operation") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AttachCertificateToDistribution where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Lightsail_20161128.AttachCertificateToDistribution" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AttachCertificateToDistribution where
  toJSON AttachCertificateToDistribution' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("distributionName" Lude..= distributionName),
            Lude.Just ("certificateName" Lude..= certificateName)
          ]
      )

instance Lude.ToPath AttachCertificateToDistribution where
  toPath = Lude.const "/"

instance Lude.ToQuery AttachCertificateToDistribution where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAttachCertificateToDistributionResponse' smart constructor.
data AttachCertificateToDistributionResponse = AttachCertificateToDistributionResponse'
  { operation ::
      Lude.Maybe
        Operation,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachCertificateToDistributionResponse' with the minimum fields required to make a request.
--
-- * 'operation' - An object that describes the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkAttachCertificateToDistributionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AttachCertificateToDistributionResponse
mkAttachCertificateToDistributionResponse pResponseStatus_ =
  AttachCertificateToDistributionResponse'
    { operation =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that describes the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
actdrsOperation :: Lens.Lens' AttachCertificateToDistributionResponse (Lude.Maybe Operation)
actdrsOperation = Lens.lens (operation :: AttachCertificateToDistributionResponse -> Lude.Maybe Operation) (\s a -> s {operation = a} :: AttachCertificateToDistributionResponse)
{-# DEPRECATED actdrsOperation "Use generic-lens or generic-optics with 'operation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
actdrsResponseStatus :: Lens.Lens' AttachCertificateToDistributionResponse Lude.Int
actdrsResponseStatus = Lens.lens (responseStatus :: AttachCertificateToDistributionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AttachCertificateToDistributionResponse)
{-# DEPRECATED actdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
