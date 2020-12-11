-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.Namespace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.Namespace
  ( Namespace (..),

    -- * Smart constructor
    mkNamespace,

    -- * Lenses
    nARN,
    nCreatorRequestId,
    nCreateDate,
    nServiceCount,
    nName,
    nId,
    nType,
    nDescription,
    nProperties,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53AutoNaming.Types.NamespaceProperties
import Network.AWS.Route53AutoNaming.Types.NamespaceType

-- | A complex type that contains information about a specified namespace.
--
-- /See:/ 'mkNamespace' smart constructor.
data Namespace = Namespace'
  { arn :: Lude.Maybe Lude.Text,
    creatorRequestId :: Lude.Maybe Lude.Text,
    createDate :: Lude.Maybe Lude.Timestamp,
    serviceCount :: Lude.Maybe Lude.Int,
    name :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe NamespaceType,
    description :: Lude.Maybe Lude.Text,
    properties :: Lude.Maybe NamespaceProperties
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Namespace' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) that AWS Cloud Map assigns to the namespace when you create it.
-- * 'createDate' - The date that the namespace was created, in Unix date/time format and Coordinated Universal Time (UTC). The value of @CreateDate@ is accurate to milliseconds. For example, the value @1516925490.087@ represents Friday, January 26, 2018 12:11:30.087 AM.
-- * 'creatorRequestId' - A unique string that identifies the request and that allows failed requests to be retried without the risk of executing an operation twice.
-- * 'description' - The description that you specify for the namespace when you create it.
-- * 'id' - The ID of a namespace.
-- * 'name' - The name of the namespace, such as @example.com@ .
-- * 'properties' - A complex type that contains information that's specific to the type of the namespace.
-- * 'serviceCount' - The number of services that are associated with the namespace.
-- * 'type'' - The type of the namespace. The methods for discovering instances depends on the value that you specify:
--
--
--     * @HTTP@ : Instances can be discovered only programmatically, using the AWS Cloud Map @DiscoverInstances@ API.
--
--
--     * @DNS_PUBLIC@ : Instances can be discovered using public DNS queries and using the @DiscoverInstances@ API.
--
--
--     * @DNS_PRIVATE@ : Instances can be discovered using DNS queries in VPCs and using the @DiscoverInstances@ API.
mkNamespace ::
  Namespace
mkNamespace =
  Namespace'
    { arn = Lude.Nothing,
      creatorRequestId = Lude.Nothing,
      createDate = Lude.Nothing,
      serviceCount = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      type' = Lude.Nothing,
      description = Lude.Nothing,
      properties = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) that AWS Cloud Map assigns to the namespace when you create it.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nARN :: Lens.Lens' Namespace (Lude.Maybe Lude.Text)
nARN = Lens.lens (arn :: Namespace -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Namespace)
{-# DEPRECATED nARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | A unique string that identifies the request and that allows failed requests to be retried without the risk of executing an operation twice.
--
-- /Note:/ Consider using 'creatorRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nCreatorRequestId :: Lens.Lens' Namespace (Lude.Maybe Lude.Text)
nCreatorRequestId = Lens.lens (creatorRequestId :: Namespace -> Lude.Maybe Lude.Text) (\s a -> s {creatorRequestId = a} :: Namespace)
{-# DEPRECATED nCreatorRequestId "Use generic-lens or generic-optics with 'creatorRequestId' instead." #-}

-- | The date that the namespace was created, in Unix date/time format and Coordinated Universal Time (UTC). The value of @CreateDate@ is accurate to milliseconds. For example, the value @1516925490.087@ represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nCreateDate :: Lens.Lens' Namespace (Lude.Maybe Lude.Timestamp)
nCreateDate = Lens.lens (createDate :: Namespace -> Lude.Maybe Lude.Timestamp) (\s a -> s {createDate = a} :: Namespace)
{-# DEPRECATED nCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | The number of services that are associated with the namespace.
--
-- /Note:/ Consider using 'serviceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nServiceCount :: Lens.Lens' Namespace (Lude.Maybe Lude.Int)
nServiceCount = Lens.lens (serviceCount :: Namespace -> Lude.Maybe Lude.Int) (\s a -> s {serviceCount = a} :: Namespace)
{-# DEPRECATED nServiceCount "Use generic-lens or generic-optics with 'serviceCount' instead." #-}

-- | The name of the namespace, such as @example.com@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nName :: Lens.Lens' Namespace (Lude.Maybe Lude.Text)
nName = Lens.lens (name :: Namespace -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Namespace)
{-# DEPRECATED nName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of a namespace.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nId :: Lens.Lens' Namespace (Lude.Maybe Lude.Text)
nId = Lens.lens (id :: Namespace -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Namespace)
{-# DEPRECATED nId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The type of the namespace. The methods for discovering instances depends on the value that you specify:
--
--
--     * @HTTP@ : Instances can be discovered only programmatically, using the AWS Cloud Map @DiscoverInstances@ API.
--
--
--     * @DNS_PUBLIC@ : Instances can be discovered using public DNS queries and using the @DiscoverInstances@ API.
--
--
--     * @DNS_PRIVATE@ : Instances can be discovered using DNS queries in VPCs and using the @DiscoverInstances@ API.
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nType :: Lens.Lens' Namespace (Lude.Maybe NamespaceType)
nType = Lens.lens (type' :: Namespace -> Lude.Maybe NamespaceType) (\s a -> s {type' = a} :: Namespace)
{-# DEPRECATED nType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The description that you specify for the namespace when you create it.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nDescription :: Lens.Lens' Namespace (Lude.Maybe Lude.Text)
nDescription = Lens.lens (description :: Namespace -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Namespace)
{-# DEPRECATED nDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A complex type that contains information that's specific to the type of the namespace.
--
-- /Note:/ Consider using 'properties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nProperties :: Lens.Lens' Namespace (Lude.Maybe NamespaceProperties)
nProperties = Lens.lens (properties :: Namespace -> Lude.Maybe NamespaceProperties) (\s a -> s {properties = a} :: Namespace)
{-# DEPRECATED nProperties "Use generic-lens or generic-optics with 'properties' instead." #-}

instance Lude.FromJSON Namespace where
  parseJSON =
    Lude.withObject
      "Namespace"
      ( \x ->
          Namespace'
            Lude.<$> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "CreatorRequestId")
            Lude.<*> (x Lude..:? "CreateDate")
            Lude.<*> (x Lude..:? "ServiceCount")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "Properties")
      )
