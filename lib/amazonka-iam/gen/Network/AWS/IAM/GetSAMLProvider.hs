{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetSAMLProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the SAML provider metadocument that was uploaded when the IAM SAML provider resource object was created or updated.
module Network.AWS.IAM.GetSAMLProvider
  ( -- * Creating a request
    GetSAMLProvider (..),
    mkGetSAMLProvider,

    -- ** Request lenses
    gsamlpSAMLProviderARN,

    -- * Destructuring the response
    GetSAMLProviderResponse (..),
    mkGetSAMLProviderResponse,

    -- ** Response lenses
    gsamlprsCreateDate,
    gsamlprsValidUntil,
    gsamlprsSAMLMetadataDocument,
    gsamlprsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetSAMLProvider' smart constructor.
newtype GetSAMLProvider = GetSAMLProvider'
  { sAMLProviderARN ::
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

-- | Creates a value of 'GetSAMLProvider' with the minimum fields required to make a request.
--
-- * 'sAMLProviderARN' - The Amazon Resource Name (ARN) of the SAML provider resource object in IAM to get information about.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
mkGetSAMLProvider ::
  -- | 'sAMLProviderARN'
  Lude.Text ->
  GetSAMLProvider
mkGetSAMLProvider pSAMLProviderARN_ =
  GetSAMLProvider' {sAMLProviderARN = pSAMLProviderARN_}

-- | The Amazon Resource Name (ARN) of the SAML provider resource object in IAM to get information about.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'sAMLProviderARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsamlpSAMLProviderARN :: Lens.Lens' GetSAMLProvider Lude.Text
gsamlpSAMLProviderARN = Lens.lens (sAMLProviderARN :: GetSAMLProvider -> Lude.Text) (\s a -> s {sAMLProviderARN = a} :: GetSAMLProvider)
{-# DEPRECATED gsamlpSAMLProviderARN "Use generic-lens or generic-optics with 'sAMLProviderARN' instead." #-}

instance Lude.AWSRequest GetSAMLProvider where
  type Rs GetSAMLProvider = GetSAMLProviderResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "GetSAMLProviderResult"
      ( \s h x ->
          GetSAMLProviderResponse'
            Lude.<$> (x Lude..@? "CreateDate")
            Lude.<*> (x Lude..@? "ValidUntil")
            Lude.<*> (x Lude..@? "SAMLMetadataDocument")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSAMLProvider where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetSAMLProvider where
  toPath = Lude.const "/"

instance Lude.ToQuery GetSAMLProvider where
  toQuery GetSAMLProvider' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetSAMLProvider" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "SAMLProviderArn" Lude.=: sAMLProviderARN
      ]

-- | Contains the response to a successful 'GetSAMLProvider' request.
--
-- /See:/ 'mkGetSAMLProviderResponse' smart constructor.
data GetSAMLProviderResponse = GetSAMLProviderResponse'
  { createDate ::
      Lude.Maybe Lude.DateTime,
    validUntil :: Lude.Maybe Lude.DateTime,
    sAMLMetadataDocument ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetSAMLProviderResponse' with the minimum fields required to make a request.
--
-- * 'createDate' - The date and time when the SAML provider was created.
-- * 'responseStatus' - The response status code.
-- * 'sAMLMetadataDocument' - The XML metadata document that includes information about an identity provider.
-- * 'validUntil' - The expiration date and time for the SAML provider.
mkGetSAMLProviderResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSAMLProviderResponse
mkGetSAMLProviderResponse pResponseStatus_ =
  GetSAMLProviderResponse'
    { createDate = Lude.Nothing,
      validUntil = Lude.Nothing,
      sAMLMetadataDocument = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The date and time when the SAML provider was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsamlprsCreateDate :: Lens.Lens' GetSAMLProviderResponse (Lude.Maybe Lude.DateTime)
gsamlprsCreateDate = Lens.lens (createDate :: GetSAMLProviderResponse -> Lude.Maybe Lude.DateTime) (\s a -> s {createDate = a} :: GetSAMLProviderResponse)
{-# DEPRECATED gsamlprsCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | The expiration date and time for the SAML provider.
--
-- /Note:/ Consider using 'validUntil' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsamlprsValidUntil :: Lens.Lens' GetSAMLProviderResponse (Lude.Maybe Lude.DateTime)
gsamlprsValidUntil = Lens.lens (validUntil :: GetSAMLProviderResponse -> Lude.Maybe Lude.DateTime) (\s a -> s {validUntil = a} :: GetSAMLProviderResponse)
{-# DEPRECATED gsamlprsValidUntil "Use generic-lens or generic-optics with 'validUntil' instead." #-}

-- | The XML metadata document that includes information about an identity provider.
--
-- /Note:/ Consider using 'sAMLMetadataDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsamlprsSAMLMetadataDocument :: Lens.Lens' GetSAMLProviderResponse (Lude.Maybe Lude.Text)
gsamlprsSAMLMetadataDocument = Lens.lens (sAMLMetadataDocument :: GetSAMLProviderResponse -> Lude.Maybe Lude.Text) (\s a -> s {sAMLMetadataDocument = a} :: GetSAMLProviderResponse)
{-# DEPRECATED gsamlprsSAMLMetadataDocument "Use generic-lens or generic-optics with 'sAMLMetadataDocument' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsamlprsResponseStatus :: Lens.Lens' GetSAMLProviderResponse Lude.Int
gsamlprsResponseStatus = Lens.lens (responseStatus :: GetSAMLProviderResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSAMLProviderResponse)
{-# DEPRECATED gsamlprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
