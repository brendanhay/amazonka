{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.AccessLevelFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.AccessLevelFilter
  ( AccessLevelFilter (..),

    -- * Smart constructor
    mkAccessLevelFilter,

    -- * Lenses
    alfKey,
    alfValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.AccessLevelFilterKey as Types
import qualified Network.AWS.ServiceCatalog.Types.Value as Types

-- | The access level to use to filter results.
--
-- /See:/ 'mkAccessLevelFilter' smart constructor.
data AccessLevelFilter = AccessLevelFilter'
  { -- | The access level.
    --
    --
    --     * @Account@ - Filter results based on the account.
    --
    --
    --     * @Role@ - Filter results based on the federated role of the specified user.
    --
    --
    --     * @User@ - Filter results based on the specified user.
    key :: Core.Maybe Types.AccessLevelFilterKey,
    -- | The user to which the access level applies. The only supported value is @Self@ .
    value :: Core.Maybe Types.Value
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AccessLevelFilter' value with any optional fields omitted.
mkAccessLevelFilter ::
  AccessLevelFilter
mkAccessLevelFilter =
  AccessLevelFilter' {key = Core.Nothing, value = Core.Nothing}

-- | The access level.
--
--
--     * @Account@ - Filter results based on the account.
--
--
--     * @Role@ - Filter results based on the federated role of the specified user.
--
--
--     * @User@ - Filter results based on the specified user.
--
--
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alfKey :: Lens.Lens' AccessLevelFilter (Core.Maybe Types.AccessLevelFilterKey)
alfKey = Lens.field @"key"
{-# DEPRECATED alfKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The user to which the access level applies. The only supported value is @Self@ .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alfValue :: Lens.Lens' AccessLevelFilter (Core.Maybe Types.Value)
alfValue = Lens.field @"value"
{-# DEPRECATED alfValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON AccessLevelFilter where
  toJSON AccessLevelFilter {..} =
    Core.object
      ( Core.catMaybes
          [("Key" Core..=) Core.<$> key, ("Value" Core..=) Core.<$> value]
      )
