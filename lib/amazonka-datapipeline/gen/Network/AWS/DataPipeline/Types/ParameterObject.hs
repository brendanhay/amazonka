{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.ParameterObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DataPipeline.Types.ParameterObject
  ( ParameterObject (..)
  -- * Smart constructor
  , mkParameterObject
  -- * Lenses
  , poId
  , poAttributes
  ) where

import qualified Network.AWS.DataPipeline.Types.Id as Types
import qualified Network.AWS.DataPipeline.Types.ParameterAttribute as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a parameter object.
--
-- /See:/ 'mkParameterObject' smart constructor.
data ParameterObject = ParameterObject'
  { id :: Types.Id
    -- ^ The ID of the parameter object. 
  , attributes :: [Types.ParameterAttribute]
    -- ^ The attributes of the parameter object.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ParameterObject' value with any optional fields omitted.
mkParameterObject
    :: Types.Id -- ^ 'id'
    -> ParameterObject
mkParameterObject id
  = ParameterObject'{id, attributes = Core.mempty}

-- | The ID of the parameter object. 
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poId :: Lens.Lens' ParameterObject Types.Id
poId = Lens.field @"id"
{-# INLINEABLE poId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The attributes of the parameter object.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poAttributes :: Lens.Lens' ParameterObject [Types.ParameterAttribute]
poAttributes = Lens.field @"attributes"
{-# INLINEABLE poAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

instance Core.FromJSON ParameterObject where
        toJSON ParameterObject{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("id" Core..= id),
                  Core.Just ("attributes" Core..= attributes)])

instance Core.FromJSON ParameterObject where
        parseJSON
          = Core.withObject "ParameterObject" Core.$
              \ x ->
                ParameterObject' Core.<$>
                  (x Core..: "id") Core.<*>
                    x Core..:? "attributes" Core..!= Core.mempty
