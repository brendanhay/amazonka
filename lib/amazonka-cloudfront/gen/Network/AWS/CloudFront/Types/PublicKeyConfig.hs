{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.PublicKeyConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.PublicKeyConfig
  ( PublicKeyConfig (..)
  -- * Smart constructor
  , mkPublicKeyConfig
  -- * Lenses
  , pkcCallerReference
  , pkcName
  , pkcEncodedKey
  , pkcComment
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configuration information about a public key that you can use with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies> , or with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption> .
--
-- /See:/ 'mkPublicKeyConfig' smart constructor.
data PublicKeyConfig = PublicKeyConfig'
  { callerReference :: Core.Text
    -- ^ A string included in the request to help make sure that the request can’t be replayed.
  , name :: Core.Text
    -- ^ A name to help identify the public key.
  , encodedKey :: Core.Text
    -- ^ The public key that you can use with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies> , or with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption> .
  , comment :: Core.Maybe Core.Text
    -- ^ A comment to describe the public key.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PublicKeyConfig' value with any optional fields omitted.
mkPublicKeyConfig
    :: Core.Text -- ^ 'callerReference'
    -> Core.Text -- ^ 'name'
    -> Core.Text -- ^ 'encodedKey'
    -> PublicKeyConfig
mkPublicKeyConfig callerReference name encodedKey
  = PublicKeyConfig'{callerReference, name, encodedKey,
                     comment = Core.Nothing}

-- | A string included in the request to help make sure that the request can’t be replayed.
--
-- /Note:/ Consider using 'callerReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pkcCallerReference :: Lens.Lens' PublicKeyConfig Core.Text
pkcCallerReference = Lens.field @"callerReference"
{-# INLINEABLE pkcCallerReference #-}
{-# DEPRECATED callerReference "Use generic-lens or generic-optics with 'callerReference' instead"  #-}

-- | A name to help identify the public key.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pkcName :: Lens.Lens' PublicKeyConfig Core.Text
pkcName = Lens.field @"name"
{-# INLINEABLE pkcName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The public key that you can use with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies> , or with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption> .
--
-- /Note:/ Consider using 'encodedKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pkcEncodedKey :: Lens.Lens' PublicKeyConfig Core.Text
pkcEncodedKey = Lens.field @"encodedKey"
{-# INLINEABLE pkcEncodedKey #-}
{-# DEPRECATED encodedKey "Use generic-lens or generic-optics with 'encodedKey' instead"  #-}

-- | A comment to describe the public key.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pkcComment :: Lens.Lens' PublicKeyConfig (Core.Maybe Core.Text)
pkcComment = Lens.field @"comment"
{-# INLINEABLE pkcComment #-}
{-# DEPRECATED comment "Use generic-lens or generic-optics with 'comment' instead"  #-}

instance Core.ToXML PublicKeyConfig where
        toXML PublicKeyConfig{..}
          = Core.toXMLElement "CallerReference" callerReference Core.<>
              Core.toXMLElement "Name" name
              Core.<> Core.toXMLElement "EncodedKey" encodedKey
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "Comment") comment

instance Core.FromXML PublicKeyConfig where
        parseXML x
          = PublicKeyConfig' Core.<$>
              (x Core..@ "CallerReference") Core.<*> x Core..@ "Name" Core.<*>
                x Core..@ "EncodedKey"
                Core.<*> x Core..@? "Comment"
