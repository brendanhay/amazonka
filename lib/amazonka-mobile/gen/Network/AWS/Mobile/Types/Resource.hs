{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.Types.Resource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Mobile.Types.Resource
  ( Resource (..)
  -- * Smart constructor
  , mkResource
  -- * Lenses
  , rArn
  , rAttributes
  , rFeature
  , rName
  , rType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Mobile.Types.AttributeKey as Types
import qualified Network.AWS.Mobile.Types.AttributeValue as Types
import qualified Network.AWS.Mobile.Types.Feature as Types
import qualified Network.AWS.Mobile.Types.ResourceArn as Types
import qualified Network.AWS.Mobile.Types.ResourceName as Types
import qualified Network.AWS.Mobile.Types.ResourceType as Types
import qualified Network.AWS.Prelude as Core

-- | Information about an instance of an AWS resource associated with a project. 
--
-- /See:/ 'mkResource' smart constructor.
data Resource = Resource'
  { arn :: Core.Maybe Types.ResourceArn
  , attributes :: Core.Maybe (Core.HashMap Types.AttributeKey Types.AttributeValue)
  , feature :: Core.Maybe Types.Feature
  , name :: Core.Maybe Types.ResourceName
  , type' :: Core.Maybe Types.ResourceType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Resource' value with any optional fields omitted.
mkResource
    :: Resource
mkResource
  = Resource'{arn = Core.Nothing, attributes = Core.Nothing,
              feature = Core.Nothing, name = Core.Nothing, type' = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rArn :: Lens.Lens' Resource (Core.Maybe Types.ResourceArn)
rArn = Lens.field @"arn"
{-# INLINEABLE rArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rAttributes :: Lens.Lens' Resource (Core.Maybe (Core.HashMap Types.AttributeKey Types.AttributeValue))
rAttributes = Lens.field @"attributes"
{-# INLINEABLE rAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'feature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rFeature :: Lens.Lens' Resource (Core.Maybe Types.Feature)
rFeature = Lens.field @"feature"
{-# INLINEABLE rFeature #-}
{-# DEPRECATED feature "Use generic-lens or generic-optics with 'feature' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rName :: Lens.Lens' Resource (Core.Maybe Types.ResourceName)
rName = Lens.field @"name"
{-# INLINEABLE rName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rType :: Lens.Lens' Resource (Core.Maybe Types.ResourceType)
rType = Lens.field @"type'"
{-# INLINEABLE rType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON Resource where
        parseJSON
          = Core.withObject "Resource" Core.$
              \ x ->
                Resource' Core.<$>
                  (x Core..:? "arn") Core.<*> x Core..:? "attributes" Core.<*>
                    x Core..:? "feature"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "type"
