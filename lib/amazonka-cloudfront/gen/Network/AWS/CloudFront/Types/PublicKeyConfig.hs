{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.PublicKeyConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.PublicKeyConfig
  ( PublicKeyConfig (..),

    -- * Smart constructor
    mkPublicKeyConfig,

    -- * Lenses
    pkcCallerReference,
    pkcName,
    pkcEncodedKey,
    pkcComment,
  )
where

import qualified Network.AWS.CloudFront.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configuration information about a public key that you can use with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies> , or with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption> .
--
-- /See:/ 'mkPublicKeyConfig' smart constructor.
data PublicKeyConfig = PublicKeyConfig'
  { -- | A string included in the request to help make sure that the request can’t be replayed.
    callerReference :: Types.String,
    -- | A name to help identify the public key.
    name :: Types.String,
    -- | The public key that you can use with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies> , or with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption> .
    encodedKey :: Types.String,
    -- | A comment to describe the public key.
    comment :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PublicKeyConfig' value with any optional fields omitted.
mkPublicKeyConfig ::
  -- | 'callerReference'
  Types.String ->
  -- | 'name'
  Types.String ->
  -- | 'encodedKey'
  Types.String ->
  PublicKeyConfig
mkPublicKeyConfig callerReference name encodedKey =
  PublicKeyConfig'
    { callerReference,
      name,
      encodedKey,
      comment = Core.Nothing
    }

-- | A string included in the request to help make sure that the request can’t be replayed.
--
-- /Note:/ Consider using 'callerReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pkcCallerReference :: Lens.Lens' PublicKeyConfig Types.String
pkcCallerReference = Lens.field @"callerReference"
{-# DEPRECATED pkcCallerReference "Use generic-lens or generic-optics with 'callerReference' instead." #-}

-- | A name to help identify the public key.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pkcName :: Lens.Lens' PublicKeyConfig Types.String
pkcName = Lens.field @"name"
{-# DEPRECATED pkcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The public key that you can use with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies> , or with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption> .
--
-- /Note:/ Consider using 'encodedKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pkcEncodedKey :: Lens.Lens' PublicKeyConfig Types.String
pkcEncodedKey = Lens.field @"encodedKey"
{-# DEPRECATED pkcEncodedKey "Use generic-lens or generic-optics with 'encodedKey' instead." #-}

-- | A comment to describe the public key.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pkcComment :: Lens.Lens' PublicKeyConfig (Core.Maybe Types.String)
pkcComment = Lens.field @"comment"
{-# DEPRECATED pkcComment "Use generic-lens or generic-optics with 'comment' instead." #-}

instance Core.ToXML PublicKeyConfig where
  toXML PublicKeyConfig {..} =
    Core.toXMLNode "CallerReference" callerReference
      Core.<> Core.toXMLNode "Name" name
      Core.<> Core.toXMLNode "EncodedKey" encodedKey
      Core.<> Core.toXMLNode "Comment" Core.<$> comment

instance Core.FromXML PublicKeyConfig where
  parseXML x =
    PublicKeyConfig'
      Core.<$> (x Core..@ "CallerReference")
      Core.<*> (x Core..@ "Name")
      Core.<*> (x Core..@ "EncodedKey")
      Core.<*> (x Core..@? "Comment")
