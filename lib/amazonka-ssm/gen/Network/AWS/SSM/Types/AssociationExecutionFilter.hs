{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationExecutionFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.AssociationExecutionFilter
  ( AssociationExecutionFilter (..)
  -- * Smart constructor
  , mkAssociationExecutionFilter
  -- * Lenses
  , aefKey
  , aefValue
  , aefType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.AssociationExecutionFilterKey as Types
import qualified Network.AWS.SSM.Types.AssociationFilterOperatorType as Types
import qualified Network.AWS.SSM.Types.Value as Types

-- | Filters used in the request.
--
-- /See:/ 'mkAssociationExecutionFilter' smart constructor.
data AssociationExecutionFilter = AssociationExecutionFilter'
  { key :: Types.AssociationExecutionFilterKey
    -- ^ The key value used in the request.
  , value :: Types.Value
    -- ^ The value specified for the key.
  , type' :: Types.AssociationFilterOperatorType
    -- ^ The filter type specified in the request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociationExecutionFilter' value with any optional fields omitted.
mkAssociationExecutionFilter
    :: Types.AssociationExecutionFilterKey -- ^ 'key'
    -> Types.Value -- ^ 'value'
    -> Types.AssociationFilterOperatorType -- ^ 'type\''
    -> AssociationExecutionFilter
mkAssociationExecutionFilter key value type'
  = AssociationExecutionFilter'{key, value, type'}

-- | The key value used in the request.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aefKey :: Lens.Lens' AssociationExecutionFilter Types.AssociationExecutionFilterKey
aefKey = Lens.field @"key"
{-# INLINEABLE aefKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The value specified for the key.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aefValue :: Lens.Lens' AssociationExecutionFilter Types.Value
aefValue = Lens.field @"value"
{-# INLINEABLE aefValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

-- | The filter type specified in the request.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aefType :: Lens.Lens' AssociationExecutionFilter Types.AssociationFilterOperatorType
aefType = Lens.field @"type'"
{-# INLINEABLE aefType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON AssociationExecutionFilter where
        toJSON AssociationExecutionFilter{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Key" Core..= key), Core.Just ("Value" Core..= value),
                  Core.Just ("Type" Core..= type')])
