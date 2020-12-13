{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.StaticIP
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.StaticIP
  ( StaticIP (..),

    -- * Smart constructor
    mkStaticIP,

    -- * Lenses
    siIpAddress,
    siResourceType,
    siArn,
    siCreatedAt,
    siLocation,
    siIsAttached,
    siName,
    siSupportCode,
    siAttachedTo,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import qualified Network.AWS.Prelude as Lude

-- | Describes the static IP.
--
-- /See:/ 'mkStaticIP' smart constructor.
data StaticIP = StaticIP'
  { -- | The static IP address.
    ipAddress :: Lude.Maybe Lude.Text,
    -- | The resource type (usually @StaticIp@ ).
    resourceType :: Lude.Maybe ResourceType,
    -- | The Amazon Resource Name (ARN) of the static IP (e.g., @arn:aws:lightsail:us-east-2:123456789101:StaticIp/9cbb4a9e-f8e3-4dfe-b57e-12345EXAMPLE@ ).
    arn :: Lude.Maybe Lude.Text,
    -- | The timestamp when the static IP was created (e.g., @1479735304.222@ ).
    createdAt :: Lude.Maybe Lude.Timestamp,
    -- | The region and Availability Zone where the static IP was created.
    location :: Lude.Maybe ResourceLocation,
    -- | A Boolean value indicating whether the static IP is attached.
    isAttached :: Lude.Maybe Lude.Bool,
    -- | The name of the static IP (e.g., @StaticIP-Ohio-EXAMPLE@ ).
    name :: Lude.Maybe Lude.Text,
    -- | The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
    supportCode :: Lude.Maybe Lude.Text,
    -- | The instance where the static IP is attached (e.g., @Amazon_Linux-1GB-Ohio-1@ ).
    attachedTo :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StaticIP' with the minimum fields required to make a request.
--
-- * 'ipAddress' - The static IP address.
-- * 'resourceType' - The resource type (usually @StaticIp@ ).
-- * 'arn' - The Amazon Resource Name (ARN) of the static IP (e.g., @arn:aws:lightsail:us-east-2:123456789101:StaticIp/9cbb4a9e-f8e3-4dfe-b57e-12345EXAMPLE@ ).
-- * 'createdAt' - The timestamp when the static IP was created (e.g., @1479735304.222@ ).
-- * 'location' - The region and Availability Zone where the static IP was created.
-- * 'isAttached' - A Boolean value indicating whether the static IP is attached.
-- * 'name' - The name of the static IP (e.g., @StaticIP-Ohio-EXAMPLE@ ).
-- * 'supportCode' - The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
-- * 'attachedTo' - The instance where the static IP is attached (e.g., @Amazon_Linux-1GB-Ohio-1@ ).
mkStaticIP ::
  StaticIP
mkStaticIP =
  StaticIP'
    { ipAddress = Lude.Nothing,
      resourceType = Lude.Nothing,
      arn = Lude.Nothing,
      createdAt = Lude.Nothing,
      location = Lude.Nothing,
      isAttached = Lude.Nothing,
      name = Lude.Nothing,
      supportCode = Lude.Nothing,
      attachedTo = Lude.Nothing
    }

-- | The static IP address.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siIpAddress :: Lens.Lens' StaticIP (Lude.Maybe Lude.Text)
siIpAddress = Lens.lens (ipAddress :: StaticIP -> Lude.Maybe Lude.Text) (\s a -> s {ipAddress = a} :: StaticIP)
{-# DEPRECATED siIpAddress "Use generic-lens or generic-optics with 'ipAddress' instead." #-}

-- | The resource type (usually @StaticIp@ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siResourceType :: Lens.Lens' StaticIP (Lude.Maybe ResourceType)
siResourceType = Lens.lens (resourceType :: StaticIP -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: StaticIP)
{-# DEPRECATED siResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The Amazon Resource Name (ARN) of the static IP (e.g., @arn:aws:lightsail:us-east-2:123456789101:StaticIp/9cbb4a9e-f8e3-4dfe-b57e-12345EXAMPLE@ ).
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siArn :: Lens.Lens' StaticIP (Lude.Maybe Lude.Text)
siArn = Lens.lens (arn :: StaticIP -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: StaticIP)
{-# DEPRECATED siArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The timestamp when the static IP was created (e.g., @1479735304.222@ ).
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siCreatedAt :: Lens.Lens' StaticIP (Lude.Maybe Lude.Timestamp)
siCreatedAt = Lens.lens (createdAt :: StaticIP -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: StaticIP)
{-# DEPRECATED siCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The region and Availability Zone where the static IP was created.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siLocation :: Lens.Lens' StaticIP (Lude.Maybe ResourceLocation)
siLocation = Lens.lens (location :: StaticIP -> Lude.Maybe ResourceLocation) (\s a -> s {location = a} :: StaticIP)
{-# DEPRECATED siLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | A Boolean value indicating whether the static IP is attached.
--
-- /Note:/ Consider using 'isAttached' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siIsAttached :: Lens.Lens' StaticIP (Lude.Maybe Lude.Bool)
siIsAttached = Lens.lens (isAttached :: StaticIP -> Lude.Maybe Lude.Bool) (\s a -> s {isAttached = a} :: StaticIP)
{-# DEPRECATED siIsAttached "Use generic-lens or generic-optics with 'isAttached' instead." #-}

-- | The name of the static IP (e.g., @StaticIP-Ohio-EXAMPLE@ ).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siName :: Lens.Lens' StaticIP (Lude.Maybe Lude.Text)
siName = Lens.lens (name :: StaticIP -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: StaticIP)
{-# DEPRECATED siName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
--
-- /Note:/ Consider using 'supportCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siSupportCode :: Lens.Lens' StaticIP (Lude.Maybe Lude.Text)
siSupportCode = Lens.lens (supportCode :: StaticIP -> Lude.Maybe Lude.Text) (\s a -> s {supportCode = a} :: StaticIP)
{-# DEPRECATED siSupportCode "Use generic-lens or generic-optics with 'supportCode' instead." #-}

-- | The instance where the static IP is attached (e.g., @Amazon_Linux-1GB-Ohio-1@ ).
--
-- /Note:/ Consider using 'attachedTo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siAttachedTo :: Lens.Lens' StaticIP (Lude.Maybe Lude.Text)
siAttachedTo = Lens.lens (attachedTo :: StaticIP -> Lude.Maybe Lude.Text) (\s a -> s {attachedTo = a} :: StaticIP)
{-# DEPRECATED siAttachedTo "Use generic-lens or generic-optics with 'attachedTo' instead." #-}

instance Lude.FromJSON StaticIP where
  parseJSON =
    Lude.withObject
      "StaticIP"
      ( \x ->
          StaticIP'
            Lude.<$> (x Lude..:? "ipAddress")
            Lude.<*> (x Lude..:? "resourceType")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "location")
            Lude.<*> (x Lude..:? "isAttached")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "supportCode")
            Lude.<*> (x Lude..:? "attachedTo")
      )
