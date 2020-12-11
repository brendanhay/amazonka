{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.DescribeLunaClient
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
-- Retrieves information about an HSM client.
module Network.AWS.CloudHSM.DescribeLunaClient
  ( -- * Creating a request
    DescribeLunaClient (..),
    mkDescribeLunaClient,

    -- ** Request lenses
    dlcClientARN,
    dlcCertificateFingerprint,

    -- * Destructuring the response
    DescribeLunaClientResponse (..),
    mkDescribeLunaClientResponse,

    -- ** Response lenses
    drsClientARN,
    drsLastModifiedTimestamp,
    drsCertificateFingerprint,
    drsCertificate,
    drsLabel,
    drsResponseStatus,
  )
where

import Network.AWS.CloudHSM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeLunaClient' smart constructor.
data DescribeLunaClient = DescribeLunaClient'
  { clientARN ::
      Lude.Maybe Lude.Text,
    certificateFingerprint :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLunaClient' with the minimum fields required to make a request.
--
-- * 'certificateFingerprint' - The certificate fingerprint.
-- * 'clientARN' - The ARN of the client.
mkDescribeLunaClient ::
  DescribeLunaClient
mkDescribeLunaClient =
  DescribeLunaClient'
    { clientARN = Lude.Nothing,
      certificateFingerprint = Lude.Nothing
    }

-- | The ARN of the client.
--
-- /Note:/ Consider using 'clientARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcClientARN :: Lens.Lens' DescribeLunaClient (Lude.Maybe Lude.Text)
dlcClientARN = Lens.lens (clientARN :: DescribeLunaClient -> Lude.Maybe Lude.Text) (\s a -> s {clientARN = a} :: DescribeLunaClient)
{-# DEPRECATED dlcClientARN "Use generic-lens or generic-optics with 'clientARN' instead." #-}

-- | The certificate fingerprint.
--
-- /Note:/ Consider using 'certificateFingerprint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcCertificateFingerprint :: Lens.Lens' DescribeLunaClient (Lude.Maybe Lude.Text)
dlcCertificateFingerprint = Lens.lens (certificateFingerprint :: DescribeLunaClient -> Lude.Maybe Lude.Text) (\s a -> s {certificateFingerprint = a} :: DescribeLunaClient)
{-# DEPRECATED dlcCertificateFingerprint "Use generic-lens or generic-optics with 'certificateFingerprint' instead." #-}

instance Lude.AWSRequest DescribeLunaClient where
  type Rs DescribeLunaClient = DescribeLunaClientResponse
  request = Req.postJSON cloudHSMService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeLunaClientResponse'
            Lude.<$> (x Lude..?> "ClientArn")
            Lude.<*> (x Lude..?> "LastModifiedTimestamp")
            Lude.<*> (x Lude..?> "CertificateFingerprint")
            Lude.<*> (x Lude..?> "Certificate")
            Lude.<*> (x Lude..?> "Label")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeLunaClient where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CloudHsmFrontendService.DescribeLunaClient" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeLunaClient where
  toJSON DescribeLunaClient' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ClientArn" Lude..=) Lude.<$> clientARN,
            ("CertificateFingerprint" Lude..=)
              Lude.<$> certificateFingerprint
          ]
      )

instance Lude.ToPath DescribeLunaClient where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeLunaClient where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeLunaClientResponse' smart constructor.
data DescribeLunaClientResponse = DescribeLunaClientResponse'
  { clientARN ::
      Lude.Maybe Lude.Text,
    lastModifiedTimestamp ::
      Lude.Maybe Lude.Text,
    certificateFingerprint ::
      Lude.Maybe Lude.Text,
    certificate :: Lude.Maybe Lude.Text,
    label :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLunaClientResponse' with the minimum fields required to make a request.
--
-- * 'certificate' - The certificate installed on the HSMs used by this client.
-- * 'certificateFingerprint' - The certificate fingerprint.
-- * 'clientARN' - The ARN of the client.
-- * 'label' - The label of the client.
-- * 'lastModifiedTimestamp' - The date and time the client was last modified.
-- * 'responseStatus' - The response status code.
mkDescribeLunaClientResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeLunaClientResponse
mkDescribeLunaClientResponse pResponseStatus_ =
  DescribeLunaClientResponse'
    { clientARN = Lude.Nothing,
      lastModifiedTimestamp = Lude.Nothing,
      certificateFingerprint = Lude.Nothing,
      certificate = Lude.Nothing,
      label = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the client.
--
-- /Note:/ Consider using 'clientARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsClientARN :: Lens.Lens' DescribeLunaClientResponse (Lude.Maybe Lude.Text)
drsClientARN = Lens.lens (clientARN :: DescribeLunaClientResponse -> Lude.Maybe Lude.Text) (\s a -> s {clientARN = a} :: DescribeLunaClientResponse)
{-# DEPRECATED drsClientARN "Use generic-lens or generic-optics with 'clientARN' instead." #-}

-- | The date and time the client was last modified.
--
-- /Note:/ Consider using 'lastModifiedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsLastModifiedTimestamp :: Lens.Lens' DescribeLunaClientResponse (Lude.Maybe Lude.Text)
drsLastModifiedTimestamp = Lens.lens (lastModifiedTimestamp :: DescribeLunaClientResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedTimestamp = a} :: DescribeLunaClientResponse)
{-# DEPRECATED drsLastModifiedTimestamp "Use generic-lens or generic-optics with 'lastModifiedTimestamp' instead." #-}

-- | The certificate fingerprint.
--
-- /Note:/ Consider using 'certificateFingerprint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsCertificateFingerprint :: Lens.Lens' DescribeLunaClientResponse (Lude.Maybe Lude.Text)
drsCertificateFingerprint = Lens.lens (certificateFingerprint :: DescribeLunaClientResponse -> Lude.Maybe Lude.Text) (\s a -> s {certificateFingerprint = a} :: DescribeLunaClientResponse)
{-# DEPRECATED drsCertificateFingerprint "Use generic-lens or generic-optics with 'certificateFingerprint' instead." #-}

-- | The certificate installed on the HSMs used by this client.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsCertificate :: Lens.Lens' DescribeLunaClientResponse (Lude.Maybe Lude.Text)
drsCertificate = Lens.lens (certificate :: DescribeLunaClientResponse -> Lude.Maybe Lude.Text) (\s a -> s {certificate = a} :: DescribeLunaClientResponse)
{-# DEPRECATED drsCertificate "Use generic-lens or generic-optics with 'certificate' instead." #-}

-- | The label of the client.
--
-- /Note:/ Consider using 'label' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsLabel :: Lens.Lens' DescribeLunaClientResponse (Lude.Maybe Lude.Text)
drsLabel = Lens.lens (label :: DescribeLunaClientResponse -> Lude.Maybe Lude.Text) (\s a -> s {label = a} :: DescribeLunaClientResponse)
{-# DEPRECATED drsLabel "Use generic-lens or generic-optics with 'label' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeLunaClientResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeLunaClientResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeLunaClientResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
