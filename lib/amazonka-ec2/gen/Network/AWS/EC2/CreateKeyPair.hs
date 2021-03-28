{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateKeyPair (..)
    , mkCreateKeyPair
    -- ** Request lenses
    , ckpKeyName
    , ckpDryRun
    , ckpTagSpecifications

    -- * Destructuring the response
    , CreateKeyPairResponse (..)
    , mkCreateKeyPairResponse
    -- ** Response lenses
    , ckprrsKeyFingerprint
    , ckprrsKeyMaterial
    , ckprrsKeyName
    , ckprrsKeyPairId
    , ckprrsTags
    , ckprrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateKeyPair' smart constructor.
data CreateKeyPair = CreateKeyPair'
  { keyName :: Core.Text
    -- ^ A unique name for the key pair.
--
-- Constraints: Up to 255 ASCII characters
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , tagSpecifications :: Core.Maybe [Types.TagSpecification]
    -- ^ The tags to apply to the new key pair.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateKeyPair' value with any optional fields omitted.
mkCreateKeyPair
    :: Core.Text -- ^ 'keyName'
    -> CreateKeyPair
mkCreateKeyPair keyName
  = CreateKeyPair'{keyName, dryRun = Core.Nothing,
                   tagSpecifications = Core.Nothing}

-- | A unique name for the key pair.
--
-- Constraints: Up to 255 ASCII characters
--
-- /Note:/ Consider using 'keyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckpKeyName :: Lens.Lens' CreateKeyPair Core.Text
ckpKeyName = Lens.field @"keyName"
{-# INLINEABLE ckpKeyName #-}
{-# DEPRECATED keyName "Use generic-lens or generic-optics with 'keyName' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckpDryRun :: Lens.Lens' CreateKeyPair (Core.Maybe Core.Bool)
ckpDryRun = Lens.field @"dryRun"
{-# INLINEABLE ckpDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The tags to apply to the new key pair.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckpTagSpecifications :: Lens.Lens' CreateKeyPair (Core.Maybe [Types.TagSpecification])
ckpTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE ckpTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

instance Core.ToQuery CreateKeyPair where
        toQuery CreateKeyPair{..}
          = Core.toQueryPair "Action" ("CreateKeyPair" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "KeyName" keyName
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TagSpecification")
                tagSpecifications

instance Core.ToHeaders CreateKeyPair where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateKeyPair where
        type Rs CreateKeyPair = CreateKeyPairResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 CreateKeyPairResponse' Core.<$>
                   (x Core..@ "keyFingerprint") Core.<*> x Core..@ "keyMaterial"
                     Core.<*> x Core..@ "keyName"
                     Core.<*> x Core..@? "keyPairId"
                     Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Describes a key pair.
--
-- /See:/ 'mkCreateKeyPairResponse' smart constructor.
data CreateKeyPairResponse = CreateKeyPairResponse'
  { keyFingerprint :: Core.Text
    -- ^ The SHA-1 digest of the DER encoded private key.
  , keyMaterial :: Types.SensitiveUserData
    -- ^ An unencrypted PEM encoded RSA private key.
  , keyName :: Core.Text
    -- ^ The name of the key pair.
  , keyPairId :: Core.Maybe Core.Text
    -- ^ The ID of the key pair.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Any tags applied to the key pair.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateKeyPairResponse' value with any optional fields omitted.
mkCreateKeyPairResponse
    :: Core.Text -- ^ 'keyFingerprint'
    -> Types.SensitiveUserData -- ^ 'keyMaterial'
    -> Core.Text -- ^ 'keyName'
    -> Core.Int -- ^ 'responseStatus'
    -> CreateKeyPairResponse
mkCreateKeyPairResponse keyFingerprint keyMaterial keyName
  responseStatus
  = CreateKeyPairResponse'{keyFingerprint, keyMaterial, keyName,
                           keyPairId = Core.Nothing, tags = Core.Nothing, responseStatus}

-- | The SHA-1 digest of the DER encoded private key.
--
-- /Note:/ Consider using 'keyFingerprint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckprrsKeyFingerprint :: Lens.Lens' CreateKeyPairResponse Core.Text
ckprrsKeyFingerprint = Lens.field @"keyFingerprint"
{-# INLINEABLE ckprrsKeyFingerprint #-}
{-# DEPRECATED keyFingerprint "Use generic-lens or generic-optics with 'keyFingerprint' instead"  #-}

-- | An unencrypted PEM encoded RSA private key.
--
-- /Note:/ Consider using 'keyMaterial' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckprrsKeyMaterial :: Lens.Lens' CreateKeyPairResponse Types.SensitiveUserData
ckprrsKeyMaterial = Lens.field @"keyMaterial"
{-# INLINEABLE ckprrsKeyMaterial #-}
{-# DEPRECATED keyMaterial "Use generic-lens or generic-optics with 'keyMaterial' instead"  #-}

-- | The name of the key pair.
--
-- /Note:/ Consider using 'keyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckprrsKeyName :: Lens.Lens' CreateKeyPairResponse Core.Text
ckprrsKeyName = Lens.field @"keyName"
{-# INLINEABLE ckprrsKeyName #-}
{-# DEPRECATED keyName "Use generic-lens or generic-optics with 'keyName' instead"  #-}

-- | The ID of the key pair.
--
-- /Note:/ Consider using 'keyPairId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckprrsKeyPairId :: Lens.Lens' CreateKeyPairResponse (Core.Maybe Core.Text)
ckprrsKeyPairId = Lens.field @"keyPairId"
{-# INLINEABLE ckprrsKeyPairId #-}
{-# DEPRECATED keyPairId "Use generic-lens or generic-optics with 'keyPairId' instead"  #-}

-- | Any tags applied to the key pair.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckprrsTags :: Lens.Lens' CreateKeyPairResponse (Core.Maybe [Types.Tag])
ckprrsTags = Lens.field @"tags"
{-# INLINEABLE ckprrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckprrsResponseStatus :: Lens.Lens' CreateKeyPairResponse Core.Int
ckprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ckprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
