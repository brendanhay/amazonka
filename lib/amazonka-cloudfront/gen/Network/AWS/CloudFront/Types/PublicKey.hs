{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.PublicKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.PublicKey
  ( PublicKey (..),

    -- * Smart constructor
    mkPublicKey,

    -- * Lenses
    pkId,
    pkCreatedTime,
    pkPublicKeyConfig,
  )
where

import qualified Network.AWS.CloudFront.Types.PublicKeyConfig as Types
import qualified Network.AWS.CloudFront.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A public key that you can use with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies> , or with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption> .
--
-- /See:/ 'mkPublicKey' smart constructor.
data PublicKey = PublicKey'
  { -- | The identifier of the public key.
    id :: Types.String,
    -- | The date and time when the public key was uploaded.
    createdTime :: Core.UTCTime,
    -- | Configuration information about a public key that you can use with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies> , or with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption> .
    publicKeyConfig :: Types.PublicKeyConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'PublicKey' value with any optional fields omitted.
mkPublicKey ::
  -- | 'id'
  Types.String ->
  -- | 'createdTime'
  Core.UTCTime ->
  -- | 'publicKeyConfig'
  Types.PublicKeyConfig ->
  PublicKey
mkPublicKey id createdTime publicKeyConfig =
  PublicKey' {id, createdTime, publicKeyConfig}

-- | The identifier of the public key.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pkId :: Lens.Lens' PublicKey Types.String
pkId = Lens.field @"id"
{-# DEPRECATED pkId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The date and time when the public key was uploaded.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pkCreatedTime :: Lens.Lens' PublicKey Core.UTCTime
pkCreatedTime = Lens.field @"createdTime"
{-# DEPRECATED pkCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | Configuration information about a public key that you can use with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies> , or with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption> .
--
-- /Note:/ Consider using 'publicKeyConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pkPublicKeyConfig :: Lens.Lens' PublicKey Types.PublicKeyConfig
pkPublicKeyConfig = Lens.field @"publicKeyConfig"
{-# DEPRECATED pkPublicKeyConfig "Use generic-lens or generic-optics with 'publicKeyConfig' instead." #-}

instance Core.FromXML PublicKey where
  parseXML x =
    PublicKey'
      Core.<$> (x Core..@ "Id")
      Core.<*> (x Core..@ "CreatedTime")
      Core.<*> (x Core..@ "PublicKeyConfig")
