{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.AccessControlList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.AccessControlList
  ( AccessControlList (..)
  -- * Smart constructor
  , mkAccessControlList
  -- * Lenses
  , aclAllowsPublicReadAccess
  , aclAllowsPublicWriteAccess
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information on the current access control policies for the bucket.
--
-- /See:/ 'mkAccessControlList' smart constructor.
data AccessControlList = AccessControlList'
  { allowsPublicReadAccess :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether public read access for the bucket is enabled through an Access Control List (ACL).
  , allowsPublicWriteAccess :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether public write access for the bucket is enabled through an Access Control List (ACL).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AccessControlList' value with any optional fields omitted.
mkAccessControlList
    :: AccessControlList
mkAccessControlList
  = AccessControlList'{allowsPublicReadAccess = Core.Nothing,
                       allowsPublicWriteAccess = Core.Nothing}

-- | A value that indicates whether public read access for the bucket is enabled through an Access Control List (ACL).
--
-- /Note:/ Consider using 'allowsPublicReadAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aclAllowsPublicReadAccess :: Lens.Lens' AccessControlList (Core.Maybe Core.Bool)
aclAllowsPublicReadAccess = Lens.field @"allowsPublicReadAccess"
{-# INLINEABLE aclAllowsPublicReadAccess #-}
{-# DEPRECATED allowsPublicReadAccess "Use generic-lens or generic-optics with 'allowsPublicReadAccess' instead"  #-}

-- | A value that indicates whether public write access for the bucket is enabled through an Access Control List (ACL).
--
-- /Note:/ Consider using 'allowsPublicWriteAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aclAllowsPublicWriteAccess :: Lens.Lens' AccessControlList (Core.Maybe Core.Bool)
aclAllowsPublicWriteAccess = Lens.field @"allowsPublicWriteAccess"
{-# INLINEABLE aclAllowsPublicWriteAccess #-}
{-# DEPRECATED allowsPublicWriteAccess "Use generic-lens or generic-optics with 'allowsPublicWriteAccess' instead"  #-}

instance Core.FromJSON AccessControlList where
        parseJSON
          = Core.withObject "AccessControlList" Core.$
              \ x ->
                AccessControlList' Core.<$>
                  (x Core..:? "allowsPublicReadAccess") Core.<*>
                    x Core..:? "allowsPublicWriteAccess"
