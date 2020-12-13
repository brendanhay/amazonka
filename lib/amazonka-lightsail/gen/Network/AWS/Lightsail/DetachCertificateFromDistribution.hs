{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DetachCertificateFromDistribution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches an SSL/TLS certificate from your Amazon Lightsail content delivery network (CDN) distribution.
--
-- After the certificate is detached, your distribution stops accepting traffic for all of the domains that are associated with the certificate.
module Network.AWS.Lightsail.DetachCertificateFromDistribution
  ( -- * Creating a request
    DetachCertificateFromDistribution (..),
    mkDetachCertificateFromDistribution,

    -- ** Request lenses
    dcfdDistributionName,

    -- * Destructuring the response
    DetachCertificateFromDistributionResponse (..),
    mkDetachCertificateFromDistributionResponse,

    -- ** Response lenses
    dcfdrsOperation,
    dcfdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDetachCertificateFromDistribution' smart constructor.
newtype DetachCertificateFromDistribution = DetachCertificateFromDistribution'
  { -- | The name of the distribution from which to detach the certificate.
    --
    -- Use the @GetDistributions@ action to get a list of distribution names that you can specify.
    distributionName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachCertificateFromDistribution' with the minimum fields required to make a request.
--
-- * 'distributionName' - The name of the distribution from which to detach the certificate.
--
-- Use the @GetDistributions@ action to get a list of distribution names that you can specify.
mkDetachCertificateFromDistribution ::
  -- | 'distributionName'
  Lude.Text ->
  DetachCertificateFromDistribution
mkDetachCertificateFromDistribution pDistributionName_ =
  DetachCertificateFromDistribution'
    { distributionName =
        pDistributionName_
    }

-- | The name of the distribution from which to detach the certificate.
--
-- Use the @GetDistributions@ action to get a list of distribution names that you can specify.
--
-- /Note:/ Consider using 'distributionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfdDistributionName :: Lens.Lens' DetachCertificateFromDistribution Lude.Text
dcfdDistributionName = Lens.lens (distributionName :: DetachCertificateFromDistribution -> Lude.Text) (\s a -> s {distributionName = a} :: DetachCertificateFromDistribution)
{-# DEPRECATED dcfdDistributionName "Use generic-lens or generic-optics with 'distributionName' instead." #-}

instance Lude.AWSRequest DetachCertificateFromDistribution where
  type
    Rs DetachCertificateFromDistribution =
      DetachCertificateFromDistributionResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          DetachCertificateFromDistributionResponse'
            Lude.<$> (x Lude..?> "operation") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DetachCertificateFromDistribution where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Lightsail_20161128.DetachCertificateFromDistribution" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DetachCertificateFromDistribution where
  toJSON DetachCertificateFromDistribution' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("distributionName" Lude..= distributionName)]
      )

instance Lude.ToPath DetachCertificateFromDistribution where
  toPath = Lude.const "/"

instance Lude.ToQuery DetachCertificateFromDistribution where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDetachCertificateFromDistributionResponse' smart constructor.
data DetachCertificateFromDistributionResponse = DetachCertificateFromDistributionResponse'
  { -- | An object that describes the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operation :: Lude.Maybe Operation,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachCertificateFromDistributionResponse' with the minimum fields required to make a request.
--
-- * 'operation' - An object that describes the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkDetachCertificateFromDistributionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DetachCertificateFromDistributionResponse
mkDetachCertificateFromDistributionResponse pResponseStatus_ =
  DetachCertificateFromDistributionResponse'
    { operation =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that describes the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfdrsOperation :: Lens.Lens' DetachCertificateFromDistributionResponse (Lude.Maybe Operation)
dcfdrsOperation = Lens.lens (operation :: DetachCertificateFromDistributionResponse -> Lude.Maybe Operation) (\s a -> s {operation = a} :: DetachCertificateFromDistributionResponse)
{-# DEPRECATED dcfdrsOperation "Use generic-lens or generic-optics with 'operation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfdrsResponseStatus :: Lens.Lens' DetachCertificateFromDistributionResponse Lude.Int
dcfdrsResponseStatus = Lens.lens (responseStatus :: DetachCertificateFromDistributionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DetachCertificateFromDistributionResponse)
{-# DEPRECATED dcfdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
