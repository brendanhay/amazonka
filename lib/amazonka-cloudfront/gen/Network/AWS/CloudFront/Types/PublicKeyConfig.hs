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
    pkcComment,
    pkcCallerReference,
    pkcName,
    pkcEncodedKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configuration information about a public key that you can use with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies> , or with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption> .
--
-- /See:/ 'mkPublicKeyConfig' smart constructor.
data PublicKeyConfig = PublicKeyConfig'
  { comment ::
      Lude.Maybe Lude.Text,
    callerReference :: Lude.Text,
    name :: Lude.Text,
    encodedKey :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PublicKeyConfig' with the minimum fields required to make a request.
--
-- * 'callerReference' - A string included in the request to help make sure that the request can’t be replayed.
-- * 'comment' - A comment to describe the public key.
-- * 'encodedKey' - The public key that you can use with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies> , or with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption> .
-- * 'name' - A name to help identify the public key.
mkPublicKeyConfig ::
  -- | 'callerReference'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'encodedKey'
  Lude.Text ->
  PublicKeyConfig
mkPublicKeyConfig pCallerReference_ pName_ pEncodedKey_ =
  PublicKeyConfig'
    { comment = Lude.Nothing,
      callerReference = pCallerReference_,
      name = pName_,
      encodedKey = pEncodedKey_
    }

-- | A comment to describe the public key.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pkcComment :: Lens.Lens' PublicKeyConfig (Lude.Maybe Lude.Text)
pkcComment = Lens.lens (comment :: PublicKeyConfig -> Lude.Maybe Lude.Text) (\s a -> s {comment = a} :: PublicKeyConfig)
{-# DEPRECATED pkcComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | A string included in the request to help make sure that the request can’t be replayed.
--
-- /Note:/ Consider using 'callerReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pkcCallerReference :: Lens.Lens' PublicKeyConfig Lude.Text
pkcCallerReference = Lens.lens (callerReference :: PublicKeyConfig -> Lude.Text) (\s a -> s {callerReference = a} :: PublicKeyConfig)
{-# DEPRECATED pkcCallerReference "Use generic-lens or generic-optics with 'callerReference' instead." #-}

-- | A name to help identify the public key.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pkcName :: Lens.Lens' PublicKeyConfig Lude.Text
pkcName = Lens.lens (name :: PublicKeyConfig -> Lude.Text) (\s a -> s {name = a} :: PublicKeyConfig)
{-# DEPRECATED pkcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The public key that you can use with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies> , or with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption> .
--
-- /Note:/ Consider using 'encodedKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pkcEncodedKey :: Lens.Lens' PublicKeyConfig Lude.Text
pkcEncodedKey = Lens.lens (encodedKey :: PublicKeyConfig -> Lude.Text) (\s a -> s {encodedKey = a} :: PublicKeyConfig)
{-# DEPRECATED pkcEncodedKey "Use generic-lens or generic-optics with 'encodedKey' instead." #-}

instance Lude.FromXML PublicKeyConfig where
  parseXML x =
    PublicKeyConfig'
      Lude.<$> (x Lude..@? "Comment")
      Lude.<*> (x Lude..@ "CallerReference")
      Lude.<*> (x Lude..@ "Name")
      Lude.<*> (x Lude..@ "EncodedKey")

instance Lude.ToXML PublicKeyConfig where
  toXML PublicKeyConfig' {..} =
    Lude.mconcat
      [ "Comment" Lude.@= comment,
        "CallerReference" Lude.@= callerReference,
        "Name" Lude.@= name,
        "EncodedKey" Lude.@= encodedKey
      ]
