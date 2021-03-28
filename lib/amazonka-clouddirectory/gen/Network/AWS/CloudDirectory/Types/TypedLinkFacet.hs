{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.TypedLinkFacet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.TypedLinkFacet
  ( TypedLinkFacet (..)
  -- * Smart constructor
  , mkTypedLinkFacet
  -- * Lenses
  , tlfName
  , tlfAttributes
  , tlfIdentityAttributeOrder
  ) where

import qualified Network.AWS.CloudDirectory.Types.AttributeName as Types
import qualified Network.AWS.CloudDirectory.Types.TypedLinkAttributeDefinition as Types
import qualified Network.AWS.CloudDirectory.Types.TypedLinkName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Defines the typed links structure and its attributes. To create a typed link facet, use the 'CreateTypedLinkFacet' API.
--
-- /See:/ 'mkTypedLinkFacet' smart constructor.
data TypedLinkFacet = TypedLinkFacet'
  { name :: Types.TypedLinkName
    -- ^ The unique name of the typed link facet.
  , attributes :: [Types.TypedLinkAttributeDefinition]
    -- ^ A set of key-value pairs associated with the typed link. Typed link attributes are used when you have data values that are related to the link itself, and not to one of the two objects being linked. Identity attributes also serve to distinguish the link from others of the same type between the same objects.
  , identityAttributeOrder :: [Types.AttributeName]
    -- ^ The set of attributes that distinguish links made from this facet from each other, in the order of significance. Listing typed links can filter on the values of these attributes. See 'ListOutgoingTypedLinks' and 'ListIncomingTypedLinks' for details.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'TypedLinkFacet' value with any optional fields omitted.
mkTypedLinkFacet
    :: Types.TypedLinkName -- ^ 'name'
    -> TypedLinkFacet
mkTypedLinkFacet name
  = TypedLinkFacet'{name, attributes = Core.mempty,
                    identityAttributeOrder = Core.mempty}

-- | The unique name of the typed link facet.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlfName :: Lens.Lens' TypedLinkFacet Types.TypedLinkName
tlfName = Lens.field @"name"
{-# INLINEABLE tlfName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A set of key-value pairs associated with the typed link. Typed link attributes are used when you have data values that are related to the link itself, and not to one of the two objects being linked. Identity attributes also serve to distinguish the link from others of the same type between the same objects.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlfAttributes :: Lens.Lens' TypedLinkFacet [Types.TypedLinkAttributeDefinition]
tlfAttributes = Lens.field @"attributes"
{-# INLINEABLE tlfAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | The set of attributes that distinguish links made from this facet from each other, in the order of significance. Listing typed links can filter on the values of these attributes. See 'ListOutgoingTypedLinks' and 'ListIncomingTypedLinks' for details.
--
-- /Note:/ Consider using 'identityAttributeOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlfIdentityAttributeOrder :: Lens.Lens' TypedLinkFacet [Types.AttributeName]
tlfIdentityAttributeOrder = Lens.field @"identityAttributeOrder"
{-# INLINEABLE tlfIdentityAttributeOrder #-}
{-# DEPRECATED identityAttributeOrder "Use generic-lens or generic-optics with 'identityAttributeOrder' instead"  #-}

instance Core.FromJSON TypedLinkFacet where
        toJSON TypedLinkFacet{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("Attributes" Core..= attributes),
                  Core.Just
                    ("IdentityAttributeOrder" Core..= identityAttributeOrder)])
