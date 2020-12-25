{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateKeyPair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a 2048-bit RSA key pair with the specified name. Amazon EC2 stores the public key and displays the private key for you to save to a file. The private key is returned as an unencrypted PEM encoded PKCS#1 private key. If a key with the specified name already exists, Amazon EC2 returns an error.
--
-- You can have up to five thousand key pairs per Region.
-- The key pair returned to you is available only in the Region in which you create it. If you prefer, you can create your own key pair using a third-party tool and upload it to any Region using 'ImportKeyPair' .
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Key Pairs> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.CreateKeyPair
  ( -- * Creating a request
    CreateKeyPair (..),
    mkCreateKeyPair,

    -- ** Request lenses
    ckpKeyName,
    ckpDryRun,
    ckpTagSpecifications,

    -- * Destructuring the response
    CreateKeyPairResponse (..),
    mkCreateKeyPairResponse,

    -- ** Response lenses
    ckprrsKeyFingerprint,
    ckprrsKeyMaterial,
    ckprrsKeyName,
    ckprrsKeyPairId,
    ckprrsTags,
    ckprrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateKeyPair' smart constructor.
data CreateKeyPair = CreateKeyPair'
  { -- | A unique name for the key pair.
    --
    -- Constraints: Up to 255 ASCII characters
    keyName :: Types.String,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The tags to apply to the new key pair.
    tagSpecifications :: Core.Maybe [Types.TagSpecification]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateKeyPair' value with any optional fields omitted.
mkCreateKeyPair ::
  -- | 'keyName'
  Types.String ->
  CreateKeyPair
mkCreateKeyPair keyName =
  CreateKeyPair'
    { keyName,
      dryRun = Core.Nothing,
      tagSpecifications = Core.Nothing
    }

-- | A unique name for the key pair.
--
-- Constraints: Up to 255 ASCII characters
--
-- /Note:/ Consider using 'keyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckpKeyName :: Lens.Lens' CreateKeyPair Types.String
ckpKeyName = Lens.field @"keyName"
{-# DEPRECATED ckpKeyName "Use generic-lens or generic-optics with 'keyName' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckpDryRun :: Lens.Lens' CreateKeyPair (Core.Maybe Core.Bool)
ckpDryRun = Lens.field @"dryRun"
{-# DEPRECATED ckpDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The tags to apply to the new key pair.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckpTagSpecifications :: Lens.Lens' CreateKeyPair (Core.Maybe [Types.TagSpecification])
ckpTagSpecifications = Lens.field @"tagSpecifications"
{-# DEPRECATED ckpTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

instance Core.AWSRequest CreateKeyPair where
  type Rs CreateKeyPair = CreateKeyPairResponse
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
            ( Core.pure ("Action", "CreateKeyPair")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "KeyName" keyName)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "TagSpecification" Core.<$> tagSpecifications)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreateKeyPairResponse'
            Core.<$> (x Core..@ "keyFingerprint")
            Core.<*> (x Core..@ "keyMaterial")
            Core.<*> (x Core..@ "keyName")
            Core.<*> (x Core..@? "keyPairId")
            Core.<*> (x Core..@? "tagSet" Core..<@> Core.parseXMLList "item")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Describes a key pair.
--
-- /See:/ 'mkCreateKeyPairResponse' smart constructor.
data CreateKeyPairResponse = CreateKeyPairResponse'
  { -- | The SHA-1 digest of the DER encoded private key.
    keyFingerprint :: Types.String,
    -- | An unencrypted PEM encoded RSA private key.
    keyMaterial :: Types.SensitiveUserData,
    -- | The name of the key pair.
    keyName :: Types.String,
    -- | The ID of the key pair.
    keyPairId :: Core.Maybe Types.String,
    -- | Any tags applied to the key pair.
    tags :: Core.Maybe [Types.Tag],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateKeyPairResponse' value with any optional fields omitted.
mkCreateKeyPairResponse ::
  -- | 'keyFingerprint'
  Types.String ->
  -- | 'keyMaterial'
  Types.SensitiveUserData ->
  -- | 'keyName'
  Types.String ->
  -- | 'responseStatus'
  Core.Int ->
  CreateKeyPairResponse
mkCreateKeyPairResponse
  keyFingerprint
  keyMaterial
  keyName
  responseStatus =
    CreateKeyPairResponse'
      { keyFingerprint,
        keyMaterial,
        keyName,
        keyPairId = Core.Nothing,
        tags = Core.Nothing,
        responseStatus
      }

-- | The SHA-1 digest of the DER encoded private key.
--
-- /Note:/ Consider using 'keyFingerprint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckprrsKeyFingerprint :: Lens.Lens' CreateKeyPairResponse Types.String
ckprrsKeyFingerprint = Lens.field @"keyFingerprint"
{-# DEPRECATED ckprrsKeyFingerprint "Use generic-lens or generic-optics with 'keyFingerprint' instead." #-}

-- | An unencrypted PEM encoded RSA private key.
--
-- /Note:/ Consider using 'keyMaterial' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckprrsKeyMaterial :: Lens.Lens' CreateKeyPairResponse Types.SensitiveUserData
ckprrsKeyMaterial = Lens.field @"keyMaterial"
{-# DEPRECATED ckprrsKeyMaterial "Use generic-lens or generic-optics with 'keyMaterial' instead." #-}

-- | The name of the key pair.
--
-- /Note:/ Consider using 'keyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckprrsKeyName :: Lens.Lens' CreateKeyPairResponse Types.String
ckprrsKeyName = Lens.field @"keyName"
{-# DEPRECATED ckprrsKeyName "Use generic-lens or generic-optics with 'keyName' instead." #-}

-- | The ID of the key pair.
--
-- /Note:/ Consider using 'keyPairId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckprrsKeyPairId :: Lens.Lens' CreateKeyPairResponse (Core.Maybe Types.String)
ckprrsKeyPairId = Lens.field @"keyPairId"
{-# DEPRECATED ckprrsKeyPairId "Use generic-lens or generic-optics with 'keyPairId' instead." #-}

-- | Any tags applied to the key pair.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckprrsTags :: Lens.Lens' CreateKeyPairResponse (Core.Maybe [Types.Tag])
ckprrsTags = Lens.field @"tags"
{-# DEPRECATED ckprrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckprrsResponseStatus :: Lens.Lens' CreateKeyPairResponse Core.Int
ckprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ckprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
