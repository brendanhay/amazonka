{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.FilterRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.FilterRule
  ( FilterRule (..)
  -- * Smart constructor
  , mkFilterRule
  -- * Lenses
  , frName
  , frValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.FilterRuleName as Types
import qualified Network.AWS.S3.Types.FilterRuleValue as Types

-- | Specifies the Amazon S3 object key name to filter on and whether to filter on the suffix or prefix of the key name.
--
-- /See:/ 'mkFilterRule' smart constructor.
data FilterRule = FilterRule'
  { name :: Core.Maybe Types.FilterRuleName
    -- ^ The object key name prefix or suffix identifying one or more objects to which the filtering rule applies. The maximum length is 1,024 characters. Overlapping prefixes and suffixes are not supported. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Configuring Event Notifications> in the /Amazon Simple Storage Service Developer Guide/ .
  , value :: Core.Maybe Types.FilterRuleValue
    -- ^ The value that the filter searches for in object key names.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FilterRule' value with any optional fields omitted.
mkFilterRule
    :: FilterRule
mkFilterRule
  = FilterRule'{name = Core.Nothing, value = Core.Nothing}

-- | The object key name prefix or suffix identifying one or more objects to which the filtering rule applies. The maximum length is 1,024 characters. Overlapping prefixes and suffixes are not supported. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Configuring Event Notifications> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frName :: Lens.Lens' FilterRule (Core.Maybe Types.FilterRuleName)
frName = Lens.field @"name"
{-# INLINEABLE frName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The value that the filter searches for in object key names.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frValue :: Lens.Lens' FilterRule (Core.Maybe Types.FilterRuleValue)
frValue = Lens.field @"value"
{-# INLINEABLE frValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.ToXML FilterRule where
        toXML FilterRule{..}
          = Core.maybe Core.mempty (Core.toXMLElement "Name") name Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "Value") value

instance Core.FromXML FilterRule where
        parseXML x
          = FilterRule' Core.<$>
              (x Core..@? "Name") Core.<*> x Core..@? "Value"
