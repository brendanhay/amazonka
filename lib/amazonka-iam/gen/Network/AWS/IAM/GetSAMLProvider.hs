{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    gsamlpSAMLProviderArn,

    -- * Destructuring the response
    GetSAMLProviderResponse (..),
    mkGetSAMLProviderResponse,

    -- ** Response lenses
    gsamlprrsCreateDate,
    gsamlprrsSAMLMetadataDocument,
    gsamlprrsValidUntil,
    gsamlprrsResponseStatus,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetSAMLProvider' smart constructor.
newtype GetSAMLProvider = GetSAMLProvider'
  { -- | The Amazon Resource Name (ARN) of the SAML provider resource object in IAM to get information about.
    --
    -- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
    sAMLProviderArn :: Types.SAMLProviderArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetSAMLProvider' value with any optional fields omitted.
mkGetSAMLProvider ::
  -- | 'sAMLProviderArn'
  Types.SAMLProviderArn ->
  GetSAMLProvider
mkGetSAMLProvider sAMLProviderArn =
  GetSAMLProvider' {sAMLProviderArn}

-- | The Amazon Resource Name (ARN) of the SAML provider resource object in IAM to get information about.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'sAMLProviderArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsamlpSAMLProviderArn :: Lens.Lens' GetSAMLProvider Types.SAMLProviderArn
gsamlpSAMLProviderArn = Lens.field @"sAMLProviderArn"
{-# DEPRECATED gsamlpSAMLProviderArn "Use generic-lens or generic-optics with 'sAMLProviderArn' instead." #-}

instance Core.AWSRequest GetSAMLProvider where
  type Rs GetSAMLProvider = GetSAMLProviderResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "GetSAMLProvider")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "SAMLProviderArn" sAMLProviderArn)
            )
      }
  response =
    Response.receiveXMLWrapper
      "GetSAMLProviderResult"
      ( \s h x ->
          GetSAMLProviderResponse'
            Core.<$> (x Core..@? "CreateDate")
            Core.<*> (x Core..@? "SAMLMetadataDocument")
            Core.<*> (x Core..@? "ValidUntil")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to a successful 'GetSAMLProvider' request.
--
-- /See:/ 'mkGetSAMLProviderResponse' smart constructor.
data GetSAMLProviderResponse = GetSAMLProviderResponse'
  { -- | The date and time when the SAML provider was created.
    createDate :: Core.Maybe Core.UTCTime,
    -- | The XML metadata document that includes information about an identity provider.
    sAMLMetadataDocument :: Core.Maybe Types.SAMLMetadataDocumentType,
    -- | The expiration date and time for the SAML provider.
    validUntil :: Core.Maybe Core.UTCTime,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetSAMLProviderResponse' value with any optional fields omitted.
mkGetSAMLProviderResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetSAMLProviderResponse
mkGetSAMLProviderResponse responseStatus =
  GetSAMLProviderResponse'
    { createDate = Core.Nothing,
      sAMLMetadataDocument = Core.Nothing,
      validUntil = Core.Nothing,
      responseStatus
    }

-- | The date and time when the SAML provider was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsamlprrsCreateDate :: Lens.Lens' GetSAMLProviderResponse (Core.Maybe Core.UTCTime)
gsamlprrsCreateDate = Lens.field @"createDate"
{-# DEPRECATED gsamlprrsCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | The XML metadata document that includes information about an identity provider.
--
-- /Note:/ Consider using 'sAMLMetadataDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsamlprrsSAMLMetadataDocument :: Lens.Lens' GetSAMLProviderResponse (Core.Maybe Types.SAMLMetadataDocumentType)
gsamlprrsSAMLMetadataDocument = Lens.field @"sAMLMetadataDocument"
{-# DEPRECATED gsamlprrsSAMLMetadataDocument "Use generic-lens or generic-optics with 'sAMLMetadataDocument' instead." #-}

-- | The expiration date and time for the SAML provider.
--
-- /Note:/ Consider using 'validUntil' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsamlprrsValidUntil :: Lens.Lens' GetSAMLProviderResponse (Core.Maybe Core.UTCTime)
gsamlprrsValidUntil = Lens.field @"validUntil"
{-# DEPRECATED gsamlprrsValidUntil "Use generic-lens or generic-optics with 'validUntil' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsamlprrsResponseStatus :: Lens.Lens' GetSAMLProviderResponse Core.Int
gsamlprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gsamlprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
