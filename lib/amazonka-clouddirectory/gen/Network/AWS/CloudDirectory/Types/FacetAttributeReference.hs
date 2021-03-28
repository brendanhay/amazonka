{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.FacetAttributeReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.FacetAttributeReference
  ( FacetAttributeReference (..)
  -- * Smart constructor
  , mkFacetAttributeReference
  -- * Lenses
  , farTargetFacetName
  , farTargetAttributeName
  ) where

import qualified Network.AWS.CloudDirectory.Types.FacetName as Types
import qualified Network.AWS.CloudDirectory.Types.TargetAttributeName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The facet attribute reference that specifies the attribute definition that contains the attribute facet name and attribute name.
--
-- /See:/ 'mkFacetAttributeReference' smart constructor.
data FacetAttributeReference = FacetAttributeReference'
  { targetFacetName :: Types.FacetName
    -- ^ The target facet name that is associated with the facet reference. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References> for more information.
  , targetAttributeName :: Types.TargetAttributeName
    -- ^ The target attribute name that is associated with the facet reference. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References> for more information.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FacetAttributeReference' value with any optional fields omitted.
mkFacetAttributeReference
    :: Types.FacetName -- ^ 'targetFacetName'
    -> Types.TargetAttributeName -- ^ 'targetAttributeName'
    -> FacetAttributeReference
mkFacetAttributeReference targetFacetName targetAttributeName
  = FacetAttributeReference'{targetFacetName, targetAttributeName}

-- | The target facet name that is associated with the facet reference. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References> for more information.
--
-- /Note:/ Consider using 'targetFacetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
farTargetFacetName :: Lens.Lens' FacetAttributeReference Types.FacetName
farTargetFacetName = Lens.field @"targetFacetName"
{-# INLINEABLE farTargetFacetName #-}
{-# DEPRECATED targetFacetName "Use generic-lens or generic-optics with 'targetFacetName' instead"  #-}

-- | The target attribute name that is associated with the facet reference. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References> for more information.
--
-- /Note:/ Consider using 'targetAttributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
farTargetAttributeName :: Lens.Lens' FacetAttributeReference Types.TargetAttributeName
farTargetAttributeName = Lens.field @"targetAttributeName"
{-# INLINEABLE farTargetAttributeName #-}
{-# DEPRECATED targetAttributeName "Use generic-lens or generic-optics with 'targetAttributeName' instead"  #-}

instance Core.FromJSON FacetAttributeReference where
        toJSON FacetAttributeReference{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TargetFacetName" Core..= targetFacetName),
                  Core.Just ("TargetAttributeName" Core..= targetAttributeName)])

instance Core.FromJSON FacetAttributeReference where
        parseJSON
          = Core.withObject "FacetAttributeReference" Core.$
              \ x ->
                FacetAttributeReference' Core.<$>
                  (x Core..: "TargetFacetName") Core.<*>
                    x Core..: "TargetAttributeName"
