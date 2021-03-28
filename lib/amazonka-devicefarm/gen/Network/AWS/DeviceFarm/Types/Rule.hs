{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Rule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.Rule
  ( Rule (..)
  -- * Smart constructor
  , mkRule
  -- * Lenses
  , rAttribute
  , rOperator
  , rValue
  ) where

import qualified Network.AWS.DeviceFarm.Types.DeviceAttribute as Types
import qualified Network.AWS.DeviceFarm.Types.RuleOperator as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a condition for a device pool.
--
-- /See:/ 'mkRule' smart constructor.
data Rule = Rule'
  { attribute :: Core.Maybe Types.DeviceAttribute
    -- ^ The rule's stringified attribute. For example, specify the value as @"\"abc\""@ .
--
-- The supported operators for each attribute are provided in the following list.
--
--     * APPIUM_VERSION
--
--     * The Appium version for the test.
-- Supported operators: @CONTAINS@ 
--
--
--     * ARN
--
--     * The Amazon Resource Name (ARN) of the device (for example, @arn:aws:devicefarm:us-west-2::device:12345Example@ .
-- Supported operators: @EQUALS@ , @IN@ , @NOT_IN@ 
--
--
--     * AVAILABILITY
--
--     * The current availability of the device. Valid values are AVAILABLE, HIGHLY_AVAILABLE, BUSY, or TEMPORARY_NOT_AVAILABLE.
-- Supported operators: @EQUALS@ 
--
--
--     * FLEET_TYPE
--
--     * The fleet type. Valid values are PUBLIC or PRIVATE.
-- Supported operators: @EQUALS@ 
--
--
--     * FORM_FACTOR
--
--     * The device form factor. Valid values are PHONE or TABLET.
-- Supported operators: @EQUALS@ , @IN@ , @NOT_IN@ 
--
--
--     * INSTANCE_ARN
--
--     * The Amazon Resource Name (ARN) of the device instance.
-- Supported operators: @IN@ , @NOT_IN@ 
--
--
--     * INSTANCE_LABELS
--
--     * The label of the device instance.
-- Supported operators: @CONTAINS@ 
--
--
--     * MANUFACTURER
--
--     * The device manufacturer (for example, Apple).
-- Supported operators: @EQUALS@ , @IN@ , @NOT_IN@ 
--
--
--     * MODEL
--
--     * The device model, such as Apple iPad Air 2 or Google Pixel.
-- Supported operators: @CONTAINS@ , @EQUALS@ , @IN@ , @NOT_IN@ 
--
--
--     * OS_VERSION
--
--     * The operating system version (for example, 10.3.2).
-- Supported operators: @EQUALS@ , @GREATER_THAN@ , @GREATER_THAN_OR_EQUALS@ , @IN@ , @LESS_THAN@ , @LESS_THAN_OR_EQUALS@ , @NOT_IN@ 
--
--
--     * PLATFORM
--
--     * The device platform. Valid values are ANDROID or IOS.
-- Supported operators: @EQUALS@ , @IN@ , @NOT_IN@ 
--
--
--     * REMOTE_ACCESS_ENABLED
--
--     * Whether the device is enabled for remote access. Valid values are TRUE or FALSE.
-- Supported operators: @EQUALS@ 
--
--
--     * REMOTE_DEBUG_ENABLED
--
--     * Whether the device is enabled for remote debugging. Valid values are TRUE or FALSE.
-- Supported operators: @EQUALS@ 
-- Because remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> , this filter is ignored.
--
--
  , operator :: Core.Maybe Types.RuleOperator
    -- ^ Specifies how Device Farm compares the rule's attribute to the value. For the operators that are supported by each attribute, see the attribute descriptions.
  , value :: Core.Maybe Core.Text
    -- ^ The rule's value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Rule' value with any optional fields omitted.
mkRule
    :: Rule
mkRule
  = Rule'{attribute = Core.Nothing, operator = Core.Nothing,
          value = Core.Nothing}

-- | The rule's stringified attribute. For example, specify the value as @"\"abc\""@ .
--
-- The supported operators for each attribute are provided in the following list.
--
--     * APPIUM_VERSION
--
--     * The Appium version for the test.
-- Supported operators: @CONTAINS@ 
--
--
--     * ARN
--
--     * The Amazon Resource Name (ARN) of the device (for example, @arn:aws:devicefarm:us-west-2::device:12345Example@ .
-- Supported operators: @EQUALS@ , @IN@ , @NOT_IN@ 
--
--
--     * AVAILABILITY
--
--     * The current availability of the device. Valid values are AVAILABLE, HIGHLY_AVAILABLE, BUSY, or TEMPORARY_NOT_AVAILABLE.
-- Supported operators: @EQUALS@ 
--
--
--     * FLEET_TYPE
--
--     * The fleet type. Valid values are PUBLIC or PRIVATE.
-- Supported operators: @EQUALS@ 
--
--
--     * FORM_FACTOR
--
--     * The device form factor. Valid values are PHONE or TABLET.
-- Supported operators: @EQUALS@ , @IN@ , @NOT_IN@ 
--
--
--     * INSTANCE_ARN
--
--     * The Amazon Resource Name (ARN) of the device instance.
-- Supported operators: @IN@ , @NOT_IN@ 
--
--
--     * INSTANCE_LABELS
--
--     * The label of the device instance.
-- Supported operators: @CONTAINS@ 
--
--
--     * MANUFACTURER
--
--     * The device manufacturer (for example, Apple).
-- Supported operators: @EQUALS@ , @IN@ , @NOT_IN@ 
--
--
--     * MODEL
--
--     * The device model, such as Apple iPad Air 2 or Google Pixel.
-- Supported operators: @CONTAINS@ , @EQUALS@ , @IN@ , @NOT_IN@ 
--
--
--     * OS_VERSION
--
--     * The operating system version (for example, 10.3.2).
-- Supported operators: @EQUALS@ , @GREATER_THAN@ , @GREATER_THAN_OR_EQUALS@ , @IN@ , @LESS_THAN@ , @LESS_THAN_OR_EQUALS@ , @NOT_IN@ 
--
--
--     * PLATFORM
--
--     * The device platform. Valid values are ANDROID or IOS.
-- Supported operators: @EQUALS@ , @IN@ , @NOT_IN@ 
--
--
--     * REMOTE_ACCESS_ENABLED
--
--     * Whether the device is enabled for remote access. Valid values are TRUE or FALSE.
-- Supported operators: @EQUALS@ 
--
--
--     * REMOTE_DEBUG_ENABLED
--
--     * Whether the device is enabled for remote debugging. Valid values are TRUE or FALSE.
-- Supported operators: @EQUALS@ 
-- Because remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> , this filter is ignored.
--
--
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rAttribute :: Lens.Lens' Rule (Core.Maybe Types.DeviceAttribute)
rAttribute = Lens.field @"attribute"
{-# INLINEABLE rAttribute #-}
{-# DEPRECATED attribute "Use generic-lens or generic-optics with 'attribute' instead"  #-}

-- | Specifies how Device Farm compares the rule's attribute to the value. For the operators that are supported by each attribute, see the attribute descriptions.
--
-- /Note:/ Consider using 'operator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rOperator :: Lens.Lens' Rule (Core.Maybe Types.RuleOperator)
rOperator = Lens.field @"operator"
{-# INLINEABLE rOperator #-}
{-# DEPRECATED operator "Use generic-lens or generic-optics with 'operator' instead"  #-}

-- | The rule's value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rValue :: Lens.Lens' Rule (Core.Maybe Core.Text)
rValue = Lens.field @"value"
{-# INLINEABLE rValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON Rule where
        toJSON Rule{..}
          = Core.object
              (Core.catMaybes
                 [("attribute" Core..=) Core.<$> attribute,
                  ("operator" Core..=) Core.<$> operator,
                  ("value" Core..=) Core.<$> value])

instance Core.FromJSON Rule where
        parseJSON
          = Core.withObject "Rule" Core.$
              \ x ->
                Rule' Core.<$>
                  (x Core..:? "attribute") Core.<*> x Core..:? "operator" Core.<*>
                    x Core..:? "value"
