-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.KeyPair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.KeyPair
  ( KeyPair (..),

    -- * Smart constructor
    mkKeyPair,

    -- * Lenses
    kpResourceType,
    kpArn,
    kpCreatedAt,
    kpLocation,
    kpFingerprint,
    kpName,
    kpSupportCode,
    kpTags,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.Tag
import qualified Network.AWS.Prelude as Lude

-- | Describes the SSH key pair.
--
-- /See:/ 'mkKeyPair' smart constructor.
data KeyPair = KeyPair'
  { resourceType :: Lude.Maybe ResourceType,
    arn :: Lude.Maybe Lude.Text,
    createdAt :: Lude.Maybe Lude.Timestamp,
    location :: Lude.Maybe ResourceLocation,
    fingerprint :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    supportCode :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KeyPair' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the key pair (e.g., @arn:aws:lightsail:us-east-2:123456789101:KeyPair/05859e3d-331d-48ba-9034-12345EXAMPLE@ ).
-- * 'createdAt' - The timestamp when the key pair was created (e.g., @1479816991.349@ ).
-- * 'fingerprint' - The RSA fingerprint of the key pair.
-- * 'location' - The region name and Availability Zone where the key pair was created.
-- * 'name' - The friendly name of the SSH key pair.
-- * 'resourceType' - The resource type (usually @KeyPair@ ).
-- * 'supportCode' - The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
-- * 'tags' - The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
mkKeyPair ::
  KeyPair
mkKeyPair =
  KeyPair'
    { resourceType = Lude.Nothing,
      arn = Lude.Nothing,
      createdAt = Lude.Nothing,
      location = Lude.Nothing,
      fingerprint = Lude.Nothing,
      name = Lude.Nothing,
      supportCode = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The resource type (usually @KeyPair@ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpResourceType :: Lens.Lens' KeyPair (Lude.Maybe ResourceType)
kpResourceType = Lens.lens (resourceType :: KeyPair -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: KeyPair)
{-# DEPRECATED kpResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The Amazon Resource Name (ARN) of the key pair (e.g., @arn:aws:lightsail:us-east-2:123456789101:KeyPair/05859e3d-331d-48ba-9034-12345EXAMPLE@ ).
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpArn :: Lens.Lens' KeyPair (Lude.Maybe Lude.Text)
kpArn = Lens.lens (arn :: KeyPair -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: KeyPair)
{-# DEPRECATED kpArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The timestamp when the key pair was created (e.g., @1479816991.349@ ).
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpCreatedAt :: Lens.Lens' KeyPair (Lude.Maybe Lude.Timestamp)
kpCreatedAt = Lens.lens (createdAt :: KeyPair -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: KeyPair)
{-# DEPRECATED kpCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The region name and Availability Zone where the key pair was created.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpLocation :: Lens.Lens' KeyPair (Lude.Maybe ResourceLocation)
kpLocation = Lens.lens (location :: KeyPair -> Lude.Maybe ResourceLocation) (\s a -> s {location = a} :: KeyPair)
{-# DEPRECATED kpLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The RSA fingerprint of the key pair.
--
-- /Note:/ Consider using 'fingerprint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpFingerprint :: Lens.Lens' KeyPair (Lude.Maybe Lude.Text)
kpFingerprint = Lens.lens (fingerprint :: KeyPair -> Lude.Maybe Lude.Text) (\s a -> s {fingerprint = a} :: KeyPair)
{-# DEPRECATED kpFingerprint "Use generic-lens or generic-optics with 'fingerprint' instead." #-}

-- | The friendly name of the SSH key pair.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpName :: Lens.Lens' KeyPair (Lude.Maybe Lude.Text)
kpName = Lens.lens (name :: KeyPair -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: KeyPair)
{-# DEPRECATED kpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
--
-- /Note:/ Consider using 'supportCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpSupportCode :: Lens.Lens' KeyPair (Lude.Maybe Lude.Text)
kpSupportCode = Lens.lens (supportCode :: KeyPair -> Lude.Maybe Lude.Text) (\s a -> s {supportCode = a} :: KeyPair)
{-# DEPRECATED kpSupportCode "Use generic-lens or generic-optics with 'supportCode' instead." #-}

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpTags :: Lens.Lens' KeyPair (Lude.Maybe [Tag])
kpTags = Lens.lens (tags :: KeyPair -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: KeyPair)
{-# DEPRECATED kpTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON KeyPair where
  parseJSON =
    Lude.withObject
      "KeyPair"
      ( \x ->
          KeyPair'
            Lude.<$> (x Lude..:? "resourceType")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "location")
            Lude.<*> (x Lude..:? "fingerprint")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "supportCode")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
