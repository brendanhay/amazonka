-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.NamespaceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.NamespaceSummary
  ( NamespaceSummary (..),

    -- * Smart constructor
    mkNamespaceSummary,

    -- * Lenses
    nsARN,
    nsCreateDate,
    nsServiceCount,
    nsName,
    nsId,
    nsType,
    nsDescription,
    nsProperties,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53AutoNaming.Types.NamespaceProperties
import Network.AWS.Route53AutoNaming.Types.NamespaceType

-- | A complex type that contains information about a namespace.
--
-- /See:/ 'mkNamespaceSummary' smart constructor.
data NamespaceSummary = NamespaceSummary'
  { arn ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'NamespaceSummary' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) that AWS Cloud Map assigns to the namespace when you create it.
-- * 'createDate' - The date and time that the namespace was created.
-- * 'description' - A description for the namespace.
-- * 'id' - The ID of the namespace.
-- * 'name' - The name of the namespace. When you create a namespace, AWS Cloud Map automatically creates a Route 53 hosted zone that has the same name as the namespace.
-- * 'properties' - Undocumented field.
-- * 'serviceCount' - The number of services that were created using the namespace.
-- * 'type'' - The type of the namespace, either public or private.
mkNamespaceSummary ::
  NamespaceSummary
mkNamespaceSummary =
  NamespaceSummary'
    { arn = Lude.Nothing,
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
nsARN :: Lens.Lens' NamespaceSummary (Lude.Maybe Lude.Text)
nsARN = Lens.lens (arn :: NamespaceSummary -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: NamespaceSummary)
{-# DEPRECATED nsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The date and time that the namespace was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsCreateDate :: Lens.Lens' NamespaceSummary (Lude.Maybe Lude.Timestamp)
nsCreateDate = Lens.lens (createDate :: NamespaceSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {createDate = a} :: NamespaceSummary)
{-# DEPRECATED nsCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | The number of services that were created using the namespace.
--
-- /Note:/ Consider using 'serviceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsServiceCount :: Lens.Lens' NamespaceSummary (Lude.Maybe Lude.Int)
nsServiceCount = Lens.lens (serviceCount :: NamespaceSummary -> Lude.Maybe Lude.Int) (\s a -> s {serviceCount = a} :: NamespaceSummary)
{-# DEPRECATED nsServiceCount "Use generic-lens or generic-optics with 'serviceCount' instead." #-}

-- | The name of the namespace. When you create a namespace, AWS Cloud Map automatically creates a Route 53 hosted zone that has the same name as the namespace.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsName :: Lens.Lens' NamespaceSummary (Lude.Maybe Lude.Text)
nsName = Lens.lens (name :: NamespaceSummary -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: NamespaceSummary)
{-# DEPRECATED nsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the namespace.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsId :: Lens.Lens' NamespaceSummary (Lude.Maybe Lude.Text)
nsId = Lens.lens (id :: NamespaceSummary -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: NamespaceSummary)
{-# DEPRECATED nsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The type of the namespace, either public or private.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsType :: Lens.Lens' NamespaceSummary (Lude.Maybe NamespaceType)
nsType = Lens.lens (type' :: NamespaceSummary -> Lude.Maybe NamespaceType) (\s a -> s {type' = a} :: NamespaceSummary)
{-# DEPRECATED nsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | A description for the namespace.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsDescription :: Lens.Lens' NamespaceSummary (Lude.Maybe Lude.Text)
nsDescription = Lens.lens (description :: NamespaceSummary -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: NamespaceSummary)
{-# DEPRECATED nsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'properties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsProperties :: Lens.Lens' NamespaceSummary (Lude.Maybe NamespaceProperties)
nsProperties = Lens.lens (properties :: NamespaceSummary -> Lude.Maybe NamespaceProperties) (\s a -> s {properties = a} :: NamespaceSummary)
{-# DEPRECATED nsProperties "Use generic-lens or generic-optics with 'properties' instead." #-}

instance Lude.FromJSON NamespaceSummary where
  parseJSON =
    Lude.withObject
      "NamespaceSummary"
      ( \x ->
          NamespaceSummary'
            Lude.<$> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "CreateDate")
            Lude.<*> (x Lude..:? "ServiceCount")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "Properties")
      )
