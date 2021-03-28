{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ObjectLockConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.ObjectLockConfiguration
  ( ObjectLockConfiguration (..)
  -- * Smart constructor
  , mkObjectLockConfiguration
  -- * Lenses
  , olcObjectLockEnabled
  , olcRule
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.ObjectLockEnabled as Types
import qualified Network.AWS.S3.Types.ObjectLockRule as Types

-- | The container element for Object Lock configuration parameters.
--
-- /See:/ 'mkObjectLockConfiguration' smart constructor.
data ObjectLockConfiguration = ObjectLockConfiguration'
  { objectLockEnabled :: Core.Maybe Types.ObjectLockEnabled
    -- ^ Indicates whether this bucket has an Object Lock configuration enabled.
  , rule :: Core.Maybe Types.ObjectLockRule
    -- ^ The Object Lock rule in place for the specified object.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ObjectLockConfiguration' value with any optional fields omitted.
mkObjectLockConfiguration
    :: ObjectLockConfiguration
mkObjectLockConfiguration
  = ObjectLockConfiguration'{objectLockEnabled = Core.Nothing,
                             rule = Core.Nothing}

-- | Indicates whether this bucket has an Object Lock configuration enabled.
--
-- /Note:/ Consider using 'objectLockEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
olcObjectLockEnabled :: Lens.Lens' ObjectLockConfiguration (Core.Maybe Types.ObjectLockEnabled)
olcObjectLockEnabled = Lens.field @"objectLockEnabled"
{-# INLINEABLE olcObjectLockEnabled #-}
{-# DEPRECATED objectLockEnabled "Use generic-lens or generic-optics with 'objectLockEnabled' instead"  #-}

-- | The Object Lock rule in place for the specified object.
--
-- /Note:/ Consider using 'rule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
olcRule :: Lens.Lens' ObjectLockConfiguration (Core.Maybe Types.ObjectLockRule)
olcRule = Lens.field @"rule"
{-# INLINEABLE olcRule #-}
{-# DEPRECATED rule "Use generic-lens or generic-optics with 'rule' instead"  #-}

instance Core.ToXML ObjectLockConfiguration where
        toXML ObjectLockConfiguration{..}
          = Core.maybe Core.mempty (Core.toXMLElement "ObjectLockEnabled")
              objectLockEnabled
              Core.<> Core.maybe Core.mempty (Core.toXMLElement "Rule") rule

instance Core.FromXML ObjectLockConfiguration where
        parseXML x
          = ObjectLockConfiguration' Core.<$>
              (x Core..@? "ObjectLockEnabled") Core.<*> x Core..@? "Rule"
