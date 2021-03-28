{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ImportKeyPair (..)
    , mkImportKeyPair
    -- ** Request lenses
    , ikpKeyName
    , ikpPublicKeyMaterial
    , ikpDryRun
    , ikpTagSpecifications

    -- * Destructuring the response
    , ImportKeyPairResponse (..)
    , mkImportKeyPairResponse
    -- ** Response lenses
    , ikprrsKeyFingerprint
    , ikprrsKeyName
    , ikprrsKeyPairId
    , ikprrsTags
    , ikprrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkImportKeyPair' smart constructor.
data ImportKeyPair = ImportKeyPair'
  { keyName :: Core.Text
    -- ^ A unique name for the key pair.
  , publicKeyMaterial :: Core.Base64
    -- ^ The public key. For API calls, the text must be base64-encoded. For command line tools, base64 encoding is performed for you.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , tagSpecifications :: Core.Maybe [Types.TagSpecification]
    -- ^ The tags to apply to the imported key pair.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportKeyPair' value with any optional fields omitted.
mkImportKeyPair
    :: Core.Text -- ^ 'keyName'
    -> Core.Base64 -- ^ 'publicKeyMaterial'
    -> ImportKeyPair
mkImportKeyPair keyName publicKeyMaterial
  = ImportKeyPair'{keyName, publicKeyMaterial, dryRun = Core.Nothing,
                   tagSpecifications = Core.Nothing}

-- | A unique name for the key pair.
--
-- /Note:/ Consider using 'keyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikpKeyName :: Lens.Lens' ImportKeyPair Core.Text
ikpKeyName = Lens.field @"keyName"
{-# INLINEABLE ikpKeyName #-}
{-# DEPRECATED keyName "Use generic-lens or generic-optics with 'keyName' instead"  #-}

-- | The public key. For API calls, the text must be base64-encoded. For command line tools, base64 encoding is performed for you.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'publicKeyMaterial' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikpPublicKeyMaterial :: Lens.Lens' ImportKeyPair Core.Base64
ikpPublicKeyMaterial = Lens.field @"publicKeyMaterial"
{-# INLINEABLE ikpPublicKeyMaterial #-}
{-# DEPRECATED publicKeyMaterial "Use generic-lens or generic-optics with 'publicKeyMaterial' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikpDryRun :: Lens.Lens' ImportKeyPair (Core.Maybe Core.Bool)
ikpDryRun = Lens.field @"dryRun"
{-# INLINEABLE ikpDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The tags to apply to the imported key pair.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikpTagSpecifications :: Lens.Lens' ImportKeyPair (Core.Maybe [Types.TagSpecification])
ikpTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE ikpTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

instance Core.ToQuery ImportKeyPair where
        toQuery ImportKeyPair{..}
          = Core.toQueryPair "Action" ("ImportKeyPair" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "KeyName" keyName
              Core.<> Core.toQueryPair "PublicKeyMaterial" publicKeyMaterial
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TagSpecification")
                tagSpecifications

instance Core.ToHeaders ImportKeyPair where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ImportKeyPair where
        type Rs ImportKeyPair = ImportKeyPairResponse
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
                 ImportKeyPairResponse' Core.<$>
                   (x Core..@? "keyFingerprint") Core.<*> x Core..@? "keyName"
                     Core.<*> x Core..@? "keyPairId"
                     Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkImportKeyPairResponse' smart constructor.
data ImportKeyPairResponse = ImportKeyPairResponse'
  { keyFingerprint :: Core.Maybe Core.Text
    -- ^ The MD5 public key fingerprint as specified in section 4 of RFC 4716.
  , keyName :: Core.Maybe Core.Text
    -- ^ The key pair name you provided.
  , keyPairId :: Core.Maybe Core.Text
    -- ^ The ID of the resulting key pair.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags applied to the imported key pair.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportKeyPairResponse' value with any optional fields omitted.
mkImportKeyPairResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ImportKeyPairResponse
mkImportKeyPairResponse responseStatus
  = ImportKeyPairResponse'{keyFingerprint = Core.Nothing,
                           keyName = Core.Nothing, keyPairId = Core.Nothing,
                           tags = Core.Nothing, responseStatus}

-- | The MD5 public key fingerprint as specified in section 4 of RFC 4716.
--
-- /Note:/ Consider using 'keyFingerprint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikprrsKeyFingerprint :: Lens.Lens' ImportKeyPairResponse (Core.Maybe Core.Text)
ikprrsKeyFingerprint = Lens.field @"keyFingerprint"
{-# INLINEABLE ikprrsKeyFingerprint #-}
{-# DEPRECATED keyFingerprint "Use generic-lens or generic-optics with 'keyFingerprint' instead"  #-}

-- | The key pair name you provided.
--
-- /Note:/ Consider using 'keyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikprrsKeyName :: Lens.Lens' ImportKeyPairResponse (Core.Maybe Core.Text)
ikprrsKeyName = Lens.field @"keyName"
{-# INLINEABLE ikprrsKeyName #-}
{-# DEPRECATED keyName "Use generic-lens or generic-optics with 'keyName' instead"  #-}

-- | The ID of the resulting key pair.
--
-- /Note:/ Consider using 'keyPairId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikprrsKeyPairId :: Lens.Lens' ImportKeyPairResponse (Core.Maybe Core.Text)
ikprrsKeyPairId = Lens.field @"keyPairId"
{-# INLINEABLE ikprrsKeyPairId #-}
{-# DEPRECATED keyPairId "Use generic-lens or generic-optics with 'keyPairId' instead"  #-}

-- | The tags applied to the imported key pair.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikprrsTags :: Lens.Lens' ImportKeyPairResponse (Core.Maybe [Types.Tag])
ikprrsTags = Lens.field @"tags"
{-# INLINEABLE ikprrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikprrsResponseStatus :: Lens.Lens' ImportKeyPairResponse Core.Int
ikprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ikprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
