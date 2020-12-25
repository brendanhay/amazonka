{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ImportKeyPair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports the public key from an RSA key pair that you created with a third-party tool. Compare this with 'CreateKeyPair' , in which AWS creates the key pair and gives the keys to you (AWS keeps a copy of the public key). With ImportKeyPair, you create the key pair and give AWS just the public key. The private key is never transferred between you and AWS.
--
-- For more information about key pairs, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Key Pairs> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.ImportKeyPair
  ( -- * Creating a request
    ImportKeyPair (..),
    mkImportKeyPair,

    -- ** Request lenses
    ikpKeyName,
    ikpPublicKeyMaterial,
    ikpDryRun,
    ikpTagSpecifications,

    -- * Destructuring the response
    ImportKeyPairResponse (..),
    mkImportKeyPairResponse,

    -- ** Response lenses
    ikprrsKeyFingerprint,
    ikprrsKeyName,
    ikprrsKeyPairId,
    ikprrsTags,
    ikprrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkImportKeyPair' smart constructor.
data ImportKeyPair = ImportKeyPair'
  { -- | A unique name for the key pair.
    keyName :: Types.String,
    -- | The public key. For API calls, the text must be base64-encoded. For command line tools, base64 encoding is performed for you.
    publicKeyMaterial :: Core.Base64,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The tags to apply to the imported key pair.
    tagSpecifications :: Core.Maybe [Types.TagSpecification]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportKeyPair' value with any optional fields omitted.
mkImportKeyPair ::
  -- | 'keyName'
  Types.String ->
  -- | 'publicKeyMaterial'
  Core.Base64 ->
  ImportKeyPair
mkImportKeyPair keyName publicKeyMaterial =
  ImportKeyPair'
    { keyName,
      publicKeyMaterial,
      dryRun = Core.Nothing,
      tagSpecifications = Core.Nothing
    }

-- | A unique name for the key pair.
--
-- /Note:/ Consider using 'keyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikpKeyName :: Lens.Lens' ImportKeyPair Types.String
ikpKeyName = Lens.field @"keyName"
{-# DEPRECATED ikpKeyName "Use generic-lens or generic-optics with 'keyName' instead." #-}

-- | The public key. For API calls, the text must be base64-encoded. For command line tools, base64 encoding is performed for you.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'publicKeyMaterial' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikpPublicKeyMaterial :: Lens.Lens' ImportKeyPair Core.Base64
ikpPublicKeyMaterial = Lens.field @"publicKeyMaterial"
{-# DEPRECATED ikpPublicKeyMaterial "Use generic-lens or generic-optics with 'publicKeyMaterial' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikpDryRun :: Lens.Lens' ImportKeyPair (Core.Maybe Core.Bool)
ikpDryRun = Lens.field @"dryRun"
{-# DEPRECATED ikpDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The tags to apply to the imported key pair.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikpTagSpecifications :: Lens.Lens' ImportKeyPair (Core.Maybe [Types.TagSpecification])
ikpTagSpecifications = Lens.field @"tagSpecifications"
{-# DEPRECATED ikpTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

instance Core.AWSRequest ImportKeyPair where
  type Rs ImportKeyPair = ImportKeyPairResponse
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
            ( Core.pure ("Action", "ImportKeyPair")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "KeyName" keyName)
                Core.<> (Core.toQueryValue "PublicKeyMaterial" publicKeyMaterial)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "TagSpecification" Core.<$> tagSpecifications)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ImportKeyPairResponse'
            Core.<$> (x Core..@? "keyFingerprint")
            Core.<*> (x Core..@? "keyName")
            Core.<*> (x Core..@? "keyPairId")
            Core.<*> (x Core..@? "tagSet" Core..<@> Core.parseXMLList "item")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkImportKeyPairResponse' smart constructor.
data ImportKeyPairResponse = ImportKeyPairResponse'
  { -- | The MD5 public key fingerprint as specified in section 4 of RFC 4716.
    keyFingerprint :: Core.Maybe Types.String,
    -- | The key pair name you provided.
    keyName :: Core.Maybe Types.String,
    -- | The ID of the resulting key pair.
    keyPairId :: Core.Maybe Types.String,
    -- | The tags applied to the imported key pair.
    tags :: Core.Maybe [Types.Tag],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportKeyPairResponse' value with any optional fields omitted.
mkImportKeyPairResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ImportKeyPairResponse
mkImportKeyPairResponse responseStatus =
  ImportKeyPairResponse'
    { keyFingerprint = Core.Nothing,
      keyName = Core.Nothing,
      keyPairId = Core.Nothing,
      tags = Core.Nothing,
      responseStatus
    }

-- | The MD5 public key fingerprint as specified in section 4 of RFC 4716.
--
-- /Note:/ Consider using 'keyFingerprint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikprrsKeyFingerprint :: Lens.Lens' ImportKeyPairResponse (Core.Maybe Types.String)
ikprrsKeyFingerprint = Lens.field @"keyFingerprint"
{-# DEPRECATED ikprrsKeyFingerprint "Use generic-lens or generic-optics with 'keyFingerprint' instead." #-}

-- | The key pair name you provided.
--
-- /Note:/ Consider using 'keyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikprrsKeyName :: Lens.Lens' ImportKeyPairResponse (Core.Maybe Types.String)
ikprrsKeyName = Lens.field @"keyName"
{-# DEPRECATED ikprrsKeyName "Use generic-lens or generic-optics with 'keyName' instead." #-}

-- | The ID of the resulting key pair.
--
-- /Note:/ Consider using 'keyPairId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikprrsKeyPairId :: Lens.Lens' ImportKeyPairResponse (Core.Maybe Types.String)
ikprrsKeyPairId = Lens.field @"keyPairId"
{-# DEPRECATED ikprrsKeyPairId "Use generic-lens or generic-optics with 'keyPairId' instead." #-}

-- | The tags applied to the imported key pair.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikprrsTags :: Lens.Lens' ImportKeyPairResponse (Core.Maybe [Types.Tag])
ikprrsTags = Lens.field @"tags"
{-# DEPRECATED ikprrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikprrsResponseStatus :: Lens.Lens' ImportKeyPairResponse Core.Int
ikprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ikprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
