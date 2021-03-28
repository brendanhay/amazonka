{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.Types.UpdateCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SDB.Types.UpdateCondition
  ( UpdateCondition (..)
  -- * Smart constructor
  , mkUpdateCondition
  -- * Lenses
  , ucExists
  , ucName
  , ucValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the conditions under which data should be updated. If an update condition is specified for a request, the data will only be updated if the condition is satisfied. For example, if an attribute with a specific name and value exists, or if a specific attribute doesn't exist. 
--
-- /See:/ 'mkUpdateCondition' smart constructor.
data UpdateCondition = UpdateCondition'
  { exists :: Core.Maybe Core.Bool
    -- ^ A value specifying whether or not the specified attribute must exist with the specified value in order for the update condition to be satisfied. Specify @true@ if the attribute must exist for the update condition to be satisfied. Specify @false@ if the attribute should not exist in order for the update condition to be satisfied.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the attribute involved in the condition.
  , value :: Core.Maybe Core.Text
    -- ^ The value of an attribute. This value can only be specified when the @Exists@ parameter is equal to @true@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCondition' value with any optional fields omitted.
mkUpdateCondition
    :: UpdateCondition
mkUpdateCondition
  = UpdateCondition'{exists = Core.Nothing, name = Core.Nothing,
                     value = Core.Nothing}

-- | A value specifying whether or not the specified attribute must exist with the specified value in order for the update condition to be satisfied. Specify @true@ if the attribute must exist for the update condition to be satisfied. Specify @false@ if the attribute should not exist in order for the update condition to be satisfied.
--
-- /Note:/ Consider using 'exists' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucExists :: Lens.Lens' UpdateCondition (Core.Maybe Core.Bool)
ucExists = Lens.field @"exists"
{-# INLINEABLE ucExists #-}
{-# DEPRECATED exists "Use generic-lens or generic-optics with 'exists' instead"  #-}

-- | The name of the attribute involved in the condition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucName :: Lens.Lens' UpdateCondition (Core.Maybe Core.Text)
ucName = Lens.field @"name"
{-# INLINEABLE ucName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The value of an attribute. This value can only be specified when the @Exists@ parameter is equal to @true@ .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucValue :: Lens.Lens' UpdateCondition (Core.Maybe Core.Text)
ucValue = Lens.field @"value"
{-# INLINEABLE ucValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.ToQuery UpdateCondition where
        toQuery UpdateCondition{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Exists") exists Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Name") name
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Value") value
