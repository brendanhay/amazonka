{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.Types.UpdateCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SDB.Types.UpdateCondition
  ( UpdateCondition (..),

    -- * Smart constructor
    mkUpdateCondition,

    -- * Lenses
    ucExists,
    ucName,
    ucValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SDB.Types.String as Types

-- | Specifies the conditions under which data should be updated. If an update condition is specified for a request, the data will only be updated if the condition is satisfied. For example, if an attribute with a specific name and value exists, or if a specific attribute doesn't exist.
--
-- /See:/ 'mkUpdateCondition' smart constructor.
data UpdateCondition = UpdateCondition'
  { -- | A value specifying whether or not the specified attribute must exist with the specified value in order for the update condition to be satisfied. Specify @true@ if the attribute must exist for the update condition to be satisfied. Specify @false@ if the attribute should not exist in order for the update condition to be satisfied.
    exists :: Core.Maybe Core.Bool,
    -- | The name of the attribute involved in the condition.
    name :: Core.Maybe Types.String,
    -- | The value of an attribute. This value can only be specified when the @Exists@ parameter is equal to @true@ .
    value :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCondition' value with any optional fields omitted.
mkUpdateCondition ::
  UpdateCondition
mkUpdateCondition =
  UpdateCondition'
    { exists = Core.Nothing,
      name = Core.Nothing,
      value = Core.Nothing
    }

-- | A value specifying whether or not the specified attribute must exist with the specified value in order for the update condition to be satisfied. Specify @true@ if the attribute must exist for the update condition to be satisfied. Specify @false@ if the attribute should not exist in order for the update condition to be satisfied.
--
-- /Note:/ Consider using 'exists' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucExists :: Lens.Lens' UpdateCondition (Core.Maybe Core.Bool)
ucExists = Lens.field @"exists"
{-# DEPRECATED ucExists "Use generic-lens or generic-optics with 'exists' instead." #-}

-- | The name of the attribute involved in the condition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucName :: Lens.Lens' UpdateCondition (Core.Maybe Types.String)
ucName = Lens.field @"name"
{-# DEPRECATED ucName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The value of an attribute. This value can only be specified when the @Exists@ parameter is equal to @true@ .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucValue :: Lens.Lens' UpdateCondition (Core.Maybe Types.String)
ucValue = Lens.field @"value"
{-# DEPRECATED ucValue "Use generic-lens or generic-optics with 'value' instead." #-}
