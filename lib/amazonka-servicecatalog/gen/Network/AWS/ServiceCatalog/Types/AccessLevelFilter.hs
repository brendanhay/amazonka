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
    alfValue,
    alfKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.ServiceCatalog.Types.AccessLevelFilterKey

-- | The access level to use to filter results.
--
-- /See:/ 'mkAccessLevelFilter' smart constructor.
data AccessLevelFilter = AccessLevelFilter'
  { -- | The user to which the access level applies. The only supported value is @Self@ .
    value :: Lude.Maybe Lude.Text,
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
    key :: Lude.Maybe AccessLevelFilterKey
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AccessLevelFilter' with the minimum fields required to make a request.
--
-- * 'value' - The user to which the access level applies. The only supported value is @Self@ .
-- * 'key' - The access level.
--
--
--     * @Account@ - Filter results based on the account.
--
--
--     * @Role@ - Filter results based on the federated role of the specified user.
--
--
--     * @User@ - Filter results based on the specified user.
mkAccessLevelFilter ::
  AccessLevelFilter
mkAccessLevelFilter =
  AccessLevelFilter' {value = Lude.Nothing, key = Lude.Nothing}

-- | The user to which the access level applies. The only supported value is @Self@ .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alfValue :: Lens.Lens' AccessLevelFilter (Lude.Maybe Lude.Text)
alfValue = Lens.lens (value :: AccessLevelFilter -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: AccessLevelFilter)
{-# DEPRECATED alfValue "Use generic-lens or generic-optics with 'value' instead." #-}

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
alfKey :: Lens.Lens' AccessLevelFilter (Lude.Maybe AccessLevelFilterKey)
alfKey = Lens.lens (key :: AccessLevelFilter -> Lude.Maybe AccessLevelFilterKey) (\s a -> s {key = a} :: AccessLevelFilter)
{-# DEPRECATED alfKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.ToJSON AccessLevelFilter where
  toJSON AccessLevelFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [("Value" Lude..=) Lude.<$> value, ("Key" Lude..=) Lude.<$> key]
      )
