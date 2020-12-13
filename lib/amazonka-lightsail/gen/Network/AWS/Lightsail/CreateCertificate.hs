{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an SSL/TLS certificate for a Amazon Lightsail content delivery network (CDN) distribution.
--
-- After the certificate is created, use the @AttachCertificateToDistribution@ action to attach the certificate to your distribution.
-- /Important:/ Only certificates created in the @us-east-1@ AWS Region can be attached to Lightsail distributions. Lightsail distributions are global resources that can reference an origin in any AWS Region, and distribute its content globally. However, all distributions are located in the @us-east-1@ Region.
module Network.AWS.Lightsail.CreateCertificate
  ( -- * Creating a request
    CreateCertificate (..),
    mkCreateCertificate,

    -- ** Request lenses
    ccSubjectAlternativeNames,
    ccCertificateName,
    ccDomainName,
    ccTags,

    -- * Destructuring the response
    CreateCertificateResponse (..),
    mkCreateCertificateResponse,

    -- ** Response lenses
    ccrsCertificate,
    ccrsOperations,
    ccrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateCertificate' smart constructor.
data CreateCertificate = CreateCertificate'
  { -- | An array of strings that specify the alternate domains (e.g., @example2.com@ ) and subdomains (e.g., @blog.example.com@ ) for the certificate.
    --
    -- You can specify a maximum of nine alternate domains (in addition to the primary domain name).
    -- Wildcard domain entries (e.g., @*.example.com@ ) are not supported.
    subjectAlternativeNames :: Lude.Maybe [Lude.Text],
    -- | The name for the certificate.
    certificateName :: Lude.Text,
    -- | The domain name (e.g., @example.com@ ) for the certificate.
    domainName :: Lude.Text,
    -- | The tag keys and optional values to add to the certificate during create.
    --
    -- Use the @TagResource@ action to tag a resource after it's created.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCertificate' with the minimum fields required to make a request.
--
-- * 'subjectAlternativeNames' - An array of strings that specify the alternate domains (e.g., @example2.com@ ) and subdomains (e.g., @blog.example.com@ ) for the certificate.
--
-- You can specify a maximum of nine alternate domains (in addition to the primary domain name).
-- Wildcard domain entries (e.g., @*.example.com@ ) are not supported.
-- * 'certificateName' - The name for the certificate.
-- * 'domainName' - The domain name (e.g., @example.com@ ) for the certificate.
-- * 'tags' - The tag keys and optional values to add to the certificate during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
mkCreateCertificate ::
  -- | 'certificateName'
  Lude.Text ->
  -- | 'domainName'
  Lude.Text ->
  CreateCertificate
mkCreateCertificate pCertificateName_ pDomainName_ =
  CreateCertificate'
    { subjectAlternativeNames = Lude.Nothing,
      certificateName = pCertificateName_,
      domainName = pDomainName_,
      tags = Lude.Nothing
    }

-- | An array of strings that specify the alternate domains (e.g., @example2.com@ ) and subdomains (e.g., @blog.example.com@ ) for the certificate.
--
-- You can specify a maximum of nine alternate domains (in addition to the primary domain name).
-- Wildcard domain entries (e.g., @*.example.com@ ) are not supported.
--
-- /Note:/ Consider using 'subjectAlternativeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccSubjectAlternativeNames :: Lens.Lens' CreateCertificate (Lude.Maybe [Lude.Text])
ccSubjectAlternativeNames = Lens.lens (subjectAlternativeNames :: CreateCertificate -> Lude.Maybe [Lude.Text]) (\s a -> s {subjectAlternativeNames = a} :: CreateCertificate)
{-# DEPRECATED ccSubjectAlternativeNames "Use generic-lens or generic-optics with 'subjectAlternativeNames' instead." #-}

-- | The name for the certificate.
--
-- /Note:/ Consider using 'certificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCertificateName :: Lens.Lens' CreateCertificate Lude.Text
ccCertificateName = Lens.lens (certificateName :: CreateCertificate -> Lude.Text) (\s a -> s {certificateName = a} :: CreateCertificate)
{-# DEPRECATED ccCertificateName "Use generic-lens or generic-optics with 'certificateName' instead." #-}

-- | The domain name (e.g., @example.com@ ) for the certificate.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDomainName :: Lens.Lens' CreateCertificate Lude.Text
ccDomainName = Lens.lens (domainName :: CreateCertificate -> Lude.Text) (\s a -> s {domainName = a} :: CreateCertificate)
{-# DEPRECATED ccDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The tag keys and optional values to add to the certificate during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccTags :: Lens.Lens' CreateCertificate (Lude.Maybe [Tag])
ccTags = Lens.lens (tags :: CreateCertificate -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateCertificate)
{-# DEPRECATED ccTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateCertificate where
  type Rs CreateCertificate = CreateCertificateResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateCertificateResponse'
            Lude.<$> (x Lude..?> "certificate")
            Lude.<*> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateCertificate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.CreateCertificate" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateCertificate where
  toJSON CreateCertificate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("subjectAlternativeNames" Lude..=)
              Lude.<$> subjectAlternativeNames,
            Lude.Just ("certificateName" Lude..= certificateName),
            Lude.Just ("domainName" Lude..= domainName),
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateCertificate where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateCertificate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateCertificateResponse' smart constructor.
data CreateCertificateResponse = CreateCertificateResponse'
  { -- | An object that describes the certificate created.
    certificate :: Lude.Maybe CertificateSummary,
    -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Lude.Maybe [Operation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCertificateResponse' with the minimum fields required to make a request.
--
-- * 'certificate' - An object that describes the certificate created.
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkCreateCertificateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateCertificateResponse
mkCreateCertificateResponse pResponseStatus_ =
  CreateCertificateResponse'
    { certificate = Lude.Nothing,
      operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that describes the certificate created.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsCertificate :: Lens.Lens' CreateCertificateResponse (Lude.Maybe CertificateSummary)
ccrsCertificate = Lens.lens (certificate :: CreateCertificateResponse -> Lude.Maybe CertificateSummary) (\s a -> s {certificate = a} :: CreateCertificateResponse)
{-# DEPRECATED ccrsCertificate "Use generic-lens or generic-optics with 'certificate' instead." #-}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsOperations :: Lens.Lens' CreateCertificateResponse (Lude.Maybe [Operation])
ccrsOperations = Lens.lens (operations :: CreateCertificateResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: CreateCertificateResponse)
{-# DEPRECATED ccrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsResponseStatus :: Lens.Lens' CreateCertificateResponse Lude.Int
ccrsResponseStatus = Lens.lens (responseStatus :: CreateCertificateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateCertificateResponse)
{-# DEPRECATED ccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
