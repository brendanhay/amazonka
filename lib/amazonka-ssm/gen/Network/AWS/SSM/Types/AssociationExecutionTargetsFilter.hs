{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationExecutionTargetsFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.AssociationExecutionTargetsFilter
  ( AssociationExecutionTargetsFilter (..)
  -- * Smart constructor
  , mkAssociationExecutionTargetsFilter
  -- * Lenses
  , aetfKey
  , aetfValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.AssociationExecutionTargetsFilterKey as Types
import qualified Network.AWS.SSM.Types.Value as Types

-- | Filters for the association execution.
--
-- /See:/ 'mkAssociationExecutionTargetsFilter' smart constructor.
data AssociationExecutionTargetsFilter = AssociationExecutionTargetsFilter'
  { key :: Types.AssociationExecutionTargetsFilterKey
    -- ^ The key value used in the request.
  , value :: Types.Value
    -- ^ The value specified for the key.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociationExecutionTargetsFilter' value with any optional fields omitted.
mkAssociationExecutionTargetsFilter
    :: Types.AssociationExecutionTargetsFilterKey -- ^ 'key'
    -> Types.Value -- ^ 'value'
    -> AssociationExecutionTargetsFilter
mkAssociationExecutionTargetsFilter key value
  = AssociationExecutionTargetsFilter'{key, value}

-- | The key value used in the request.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetfKey :: Lens.Lens' AssociationExecutionTargetsFilter Types.AssociationExecutionTargetsFilterKey
aetfKey = Lens.field @"key"
{-# INLINEABLE aetfKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The value specified for the key.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetfValue :: Lens.Lens' AssociationExecutionTargetsFilter Types.Value
aetfValue = Lens.field @"value"
{-# INLINEABLE aetfValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON AssociationExecutionTargetsFilter where
        toJSON AssociationExecutionTargetsFilter{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Key" Core..= key), Core.Just ("Value" Core..= value)])
