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
    pkCreatedTime,
    pkPublicKeyConfig,
    pkId,
  )
where

import Network.AWS.CloudFront.Types.PublicKeyConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A public key that you can use with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies> , or with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption> .
--
-- /See:/ 'mkPublicKey' smart constructor.
data PublicKey = PublicKey'
  { -- | The date and time when the public key was uploaded.
    createdTime :: Lude.DateTime,
    -- | Configuration information about a public key that you can use with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies> , or with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption> .
    publicKeyConfig :: PublicKeyConfig,
    -- | The identifier of the public key.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PublicKey' with the minimum fields required to make a request.
--
-- * 'createdTime' - The date and time when the public key was uploaded.
-- * 'publicKeyConfig' - Configuration information about a public key that you can use with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies> , or with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption> .
-- * 'id' - The identifier of the public key.
mkPublicKey ::
  -- | 'createdTime'
  Lude.DateTime ->
  -- | 'publicKeyConfig'
  PublicKeyConfig ->
  -- | 'id'
  Lude.Text ->
  PublicKey
mkPublicKey pCreatedTime_ pPublicKeyConfig_ pId_ =
  PublicKey'
    { createdTime = pCreatedTime_,
      publicKeyConfig = pPublicKeyConfig_,
      id = pId_
    }

-- | The date and time when the public key was uploaded.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pkCreatedTime :: Lens.Lens' PublicKey Lude.DateTime
pkCreatedTime = Lens.lens (createdTime :: PublicKey -> Lude.DateTime) (\s a -> s {createdTime = a} :: PublicKey)
{-# DEPRECATED pkCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | Configuration information about a public key that you can use with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies> , or with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption> .
--
-- /Note:/ Consider using 'publicKeyConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pkPublicKeyConfig :: Lens.Lens' PublicKey PublicKeyConfig
pkPublicKeyConfig = Lens.lens (publicKeyConfig :: PublicKey -> PublicKeyConfig) (\s a -> s {publicKeyConfig = a} :: PublicKey)
{-# DEPRECATED pkPublicKeyConfig "Use generic-lens or generic-optics with 'publicKeyConfig' instead." #-}

-- | The identifier of the public key.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pkId :: Lens.Lens' PublicKey Lude.Text
pkId = Lens.lens (id :: PublicKey -> Lude.Text) (\s a -> s {id = a} :: PublicKey)
{-# DEPRECATED pkId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromXML PublicKey where
  parseXML x =
    PublicKey'
      Lude.<$> (x Lude..@ "CreatedTime")
      Lude.<*> (x Lude..@ "PublicKeyConfig")
      Lude.<*> (x Lude..@ "Id")
