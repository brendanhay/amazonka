{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.Domain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.Domain
  ( Domain (..),

    -- * Smart constructor
    mkDomain,

    -- * Lenses
    dResourceType,
    dDomainEntries,
    dArn,
    dCreatedAt,
    dLocation,
    dName,
    dSupportCode,
    dTags,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.DomainEntry
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.Tag
import qualified Network.AWS.Prelude as Lude

-- | Describes a domain where you are storing recordsets in Lightsail.
--
-- /See:/ 'mkDomain' smart constructor.
data Domain = Domain'
  { -- | The resource type.
    resourceType :: Lude.Maybe ResourceType,
    -- | An array of key-value pairs containing information about the domain entries.
    domainEntries :: Lude.Maybe [DomainEntry],
    -- | The Amazon Resource Name (ARN) of the domain recordset (e.g., @arn:aws:lightsail:global:123456789101:Domain/824cede0-abc7-4f84-8dbc-12345EXAMPLE@ ).
    arn :: Lude.Maybe Lude.Text,
    -- | The date when the domain recordset was created.
    createdAt :: Lude.Maybe Lude.Timestamp,
    -- | The AWS Region and Availability Zones where the domain recordset was created.
    location :: Lude.Maybe ResourceLocation,
    -- | The name of the domain.
    name :: Lude.Maybe Lude.Text,
    -- | The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
    supportCode :: Lude.Maybe Lude.Text,
    -- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Domain' with the minimum fields required to make a request.
--
-- * 'resourceType' - The resource type.
-- * 'domainEntries' - An array of key-value pairs containing information about the domain entries.
-- * 'arn' - The Amazon Resource Name (ARN) of the domain recordset (e.g., @arn:aws:lightsail:global:123456789101:Domain/824cede0-abc7-4f84-8dbc-12345EXAMPLE@ ).
-- * 'createdAt' - The date when the domain recordset was created.
-- * 'location' - The AWS Region and Availability Zones where the domain recordset was created.
-- * 'name' - The name of the domain.
-- * 'supportCode' - The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
-- * 'tags' - The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
mkDomain ::
  Domain
mkDomain =
  Domain'
    { resourceType = Lude.Nothing,
      domainEntries = Lude.Nothing,
      arn = Lude.Nothing,
      createdAt = Lude.Nothing,
      location = Lude.Nothing,
      name = Lude.Nothing,
      supportCode = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The resource type.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dResourceType :: Lens.Lens' Domain (Lude.Maybe ResourceType)
dResourceType = Lens.lens (resourceType :: Domain -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: Domain)
{-# DEPRECATED dResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | An array of key-value pairs containing information about the domain entries.
--
-- /Note:/ Consider using 'domainEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDomainEntries :: Lens.Lens' Domain (Lude.Maybe [DomainEntry])
dDomainEntries = Lens.lens (domainEntries :: Domain -> Lude.Maybe [DomainEntry]) (\s a -> s {domainEntries = a} :: Domain)
{-# DEPRECATED dDomainEntries "Use generic-lens or generic-optics with 'domainEntries' instead." #-}

-- | The Amazon Resource Name (ARN) of the domain recordset (e.g., @arn:aws:lightsail:global:123456789101:Domain/824cede0-abc7-4f84-8dbc-12345EXAMPLE@ ).
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dArn :: Lens.Lens' Domain (Lude.Maybe Lude.Text)
dArn = Lens.lens (arn :: Domain -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Domain)
{-# DEPRECATED dArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The date when the domain recordset was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCreatedAt :: Lens.Lens' Domain (Lude.Maybe Lude.Timestamp)
dCreatedAt = Lens.lens (createdAt :: Domain -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: Domain)
{-# DEPRECATED dCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The AWS Region and Availability Zones where the domain recordset was created.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLocation :: Lens.Lens' Domain (Lude.Maybe ResourceLocation)
dLocation = Lens.lens (location :: Domain -> Lude.Maybe ResourceLocation) (\s a -> s {location = a} :: Domain)
{-# DEPRECATED dLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The name of the domain.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dName :: Lens.Lens' Domain (Lude.Maybe Lude.Text)
dName = Lens.lens (name :: Domain -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Domain)
{-# DEPRECATED dName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
--
-- /Note:/ Consider using 'supportCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSupportCode :: Lens.Lens' Domain (Lude.Maybe Lude.Text)
dSupportCode = Lens.lens (supportCode :: Domain -> Lude.Maybe Lude.Text) (\s a -> s {supportCode = a} :: Domain)
{-# DEPRECATED dSupportCode "Use generic-lens or generic-optics with 'supportCode' instead." #-}

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTags :: Lens.Lens' Domain (Lude.Maybe [Tag])
dTags = Lens.lens (tags :: Domain -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: Domain)
{-# DEPRECATED dTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON Domain where
  parseJSON =
    Lude.withObject
      "Domain"
      ( \x ->
          Domain'
            Lude.<$> (x Lude..:? "resourceType")
            Lude.<*> (x Lude..:? "domainEntries" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "location")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "supportCode")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
