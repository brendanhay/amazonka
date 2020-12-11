-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.TypedLinkFacet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.TypedLinkFacet
  ( TypedLinkFacet (..),

    -- * Smart constructor
    mkTypedLinkFacet,

    -- * Lenses
    tlfName,
    tlfAttributes,
    tlfIdentityAttributeOrder,
  )
where

import Network.AWS.CloudDirectory.Types.TypedLinkAttributeDefinition
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Defines the typed links structure and its attributes. To create a typed link facet, use the 'CreateTypedLinkFacet' API.
--
-- /See:/ 'mkTypedLinkFacet' smart constructor.
data TypedLinkFacet = TypedLinkFacet'
  { name :: Lude.Text,
    attributes :: [TypedLinkAttributeDefinition],
    identityAttributeOrder :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TypedLinkFacet' with the minimum fields required to make a request.
--
-- * 'attributes' - A set of key-value pairs associated with the typed link. Typed link attributes are used when you have data values that are related to the link itself, and not to one of the two objects being linked. Identity attributes also serve to distinguish the link from others of the same type between the same objects.
-- * 'identityAttributeOrder' - The set of attributes that distinguish links made from this facet from each other, in the order of significance. Listing typed links can filter on the values of these attributes. See 'ListOutgoingTypedLinks' and 'ListIncomingTypedLinks' for details.
-- * 'name' - The unique name of the typed link facet.
mkTypedLinkFacet ::
  -- | 'name'
  Lude.Text ->
  TypedLinkFacet
mkTypedLinkFacet pName_ =
  TypedLinkFacet'
    { name = pName_,
      attributes = Lude.mempty,
      identityAttributeOrder = Lude.mempty
    }

-- | The unique name of the typed link facet.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlfName :: Lens.Lens' TypedLinkFacet Lude.Text
tlfName = Lens.lens (name :: TypedLinkFacet -> Lude.Text) (\s a -> s {name = a} :: TypedLinkFacet)
{-# DEPRECATED tlfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A set of key-value pairs associated with the typed link. Typed link attributes are used when you have data values that are related to the link itself, and not to one of the two objects being linked. Identity attributes also serve to distinguish the link from others of the same type between the same objects.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlfAttributes :: Lens.Lens' TypedLinkFacet [TypedLinkAttributeDefinition]
tlfAttributes = Lens.lens (attributes :: TypedLinkFacet -> [TypedLinkAttributeDefinition]) (\s a -> s {attributes = a} :: TypedLinkFacet)
{-# DEPRECATED tlfAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The set of attributes that distinguish links made from this facet from each other, in the order of significance. Listing typed links can filter on the values of these attributes. See 'ListOutgoingTypedLinks' and 'ListIncomingTypedLinks' for details.
--
-- /Note:/ Consider using 'identityAttributeOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlfIdentityAttributeOrder :: Lens.Lens' TypedLinkFacet [Lude.Text]
tlfIdentityAttributeOrder = Lens.lens (identityAttributeOrder :: TypedLinkFacet -> [Lude.Text]) (\s a -> s {identityAttributeOrder = a} :: TypedLinkFacet)
{-# DEPRECATED tlfIdentityAttributeOrder "Use generic-lens or generic-optics with 'identityAttributeOrder' instead." #-}

instance Lude.ToJSON TypedLinkFacet where
  toJSON TypedLinkFacet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            Lude.Just ("Attributes" Lude..= attributes),
            Lude.Just
              ("IdentityAttributeOrder" Lude..= identityAttributeOrder)
          ]
      )
