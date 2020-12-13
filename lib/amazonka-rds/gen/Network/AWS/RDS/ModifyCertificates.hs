{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ModifyCertificates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Override the system-default Secure Sockets Layer/Transport Layer Security (SSL/TLS) certificate for Amazon RDS for new DB instances temporarily, or remove the override.
--
-- By using this operation, you can specify an RDS-approved SSL/TLS certificate for new DB instances that is different from the default certificate provided by RDS. You can also use this operation to remove the override, so that new DB instances use the default certificate provided by RDS.
-- You might need to override the default certificate in the following situations:
--
--     * You already migrated your applications to support the latest certificate authority (CA) certificate, but the new CA certificate is not yet the RDS default CA certificate for the specified AWS Region.
--
--
--     * RDS has already moved to a new default CA certificate for the specified AWS Region, but you are still in the process of supporting the new CA certificate. In this case, you temporarily need additional time to finish your application changes.
--
--
-- For more information about rotating your SSL/TLS certificate for RDS DB engines, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.SSL-certificate-rotation.html Rotating Your SSL/TLS Certificate> in the /Amazon RDS User Guide/ .
-- For more information about rotating your SSL/TLS certificate for Aurora DB engines, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.SSL-certificate-rotation.html Rotating Your SSL/TLS Certificate> in the /Amazon Aurora User Guide/ .
module Network.AWS.RDS.ModifyCertificates
  ( -- * Creating a request
    ModifyCertificates (..),
    mkModifyCertificates,

    -- ** Request lenses
    mcCertificateIdentifier,
    mcRemoveCustomerOverride,

    -- * Destructuring the response
    ModifyCertificatesResponse (..),
    mkModifyCertificatesResponse,

    -- ** Response lenses
    mcrsCertificate,
    mcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyCertificates' smart constructor.
data ModifyCertificates = ModifyCertificates'
  { -- | The new default certificate identifier to override the current one with.
    --
    -- To determine the valid values, use the @describe-certificates@ AWS CLI command or the @DescribeCertificates@ API operation.
    certificateIdentifier :: Lude.Maybe Lude.Text,
    -- | A value that indicates whether to remove the override for the default certificate. If the override is removed, the default certificate is the system default.
    removeCustomerOverride :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyCertificates' with the minimum fields required to make a request.
--
-- * 'certificateIdentifier' - The new default certificate identifier to override the current one with.
--
-- To determine the valid values, use the @describe-certificates@ AWS CLI command or the @DescribeCertificates@ API operation.
-- * 'removeCustomerOverride' - A value that indicates whether to remove the override for the default certificate. If the override is removed, the default certificate is the system default.
mkModifyCertificates ::
  ModifyCertificates
mkModifyCertificates =
  ModifyCertificates'
    { certificateIdentifier = Lude.Nothing,
      removeCustomerOverride = Lude.Nothing
    }

-- | The new default certificate identifier to override the current one with.
--
-- To determine the valid values, use the @describe-certificates@ AWS CLI command or the @DescribeCertificates@ API operation.
--
-- /Note:/ Consider using 'certificateIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcCertificateIdentifier :: Lens.Lens' ModifyCertificates (Lude.Maybe Lude.Text)
mcCertificateIdentifier = Lens.lens (certificateIdentifier :: ModifyCertificates -> Lude.Maybe Lude.Text) (\s a -> s {certificateIdentifier = a} :: ModifyCertificates)
{-# DEPRECATED mcCertificateIdentifier "Use generic-lens or generic-optics with 'certificateIdentifier' instead." #-}

-- | A value that indicates whether to remove the override for the default certificate. If the override is removed, the default certificate is the system default.
--
-- /Note:/ Consider using 'removeCustomerOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcRemoveCustomerOverride :: Lens.Lens' ModifyCertificates (Lude.Maybe Lude.Bool)
mcRemoveCustomerOverride = Lens.lens (removeCustomerOverride :: ModifyCertificates -> Lude.Maybe Lude.Bool) (\s a -> s {removeCustomerOverride = a} :: ModifyCertificates)
{-# DEPRECATED mcRemoveCustomerOverride "Use generic-lens or generic-optics with 'removeCustomerOverride' instead." #-}

instance Lude.AWSRequest ModifyCertificates where
  type Rs ModifyCertificates = ModifyCertificatesResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "ModifyCertificatesResult"
      ( \s h x ->
          ModifyCertificatesResponse'
            Lude.<$> (x Lude..@? "Certificate") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyCertificates where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyCertificates where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyCertificates where
  toQuery ModifyCertificates' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyCertificates" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "CertificateIdentifier" Lude.=: certificateIdentifier,
        "RemoveCustomerOverride" Lude.=: removeCustomerOverride
      ]

-- | /See:/ 'mkModifyCertificatesResponse' smart constructor.
data ModifyCertificatesResponse = ModifyCertificatesResponse'
  { certificate :: Lude.Maybe Certificate,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyCertificatesResponse' with the minimum fields required to make a request.
--
-- * 'certificate' -
-- * 'responseStatus' - The response status code.
mkModifyCertificatesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyCertificatesResponse
mkModifyCertificatesResponse pResponseStatus_ =
  ModifyCertificatesResponse'
    { certificate = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcrsCertificate :: Lens.Lens' ModifyCertificatesResponse (Lude.Maybe Certificate)
mcrsCertificate = Lens.lens (certificate :: ModifyCertificatesResponse -> Lude.Maybe Certificate) (\s a -> s {certificate = a} :: ModifyCertificatesResponse)
{-# DEPRECATED mcrsCertificate "Use generic-lens or generic-optics with 'certificate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcrsResponseStatus :: Lens.Lens' ModifyCertificatesResponse Lude.Int
mcrsResponseStatus = Lens.lens (responseStatus :: ModifyCertificatesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyCertificatesResponse)
{-# DEPRECATED mcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
