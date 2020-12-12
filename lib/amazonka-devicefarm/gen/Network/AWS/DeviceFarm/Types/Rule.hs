{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Rule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Rule
  ( Rule (..),

    -- * Smart constructor
    mkRule,

    -- * Lenses
    rAttribute,
    rOperator,
    rValue,
  )
where

import Network.AWS.DeviceFarm.Types.DeviceAttribute
import Network.AWS.DeviceFarm.Types.RuleOperator
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a condition for a device pool.
--
-- /See:/ 'mkRule' smart constructor.
data Rule = Rule'
  { attribute :: Lude.Maybe DeviceAttribute,
    operator :: Lude.Maybe RuleOperator,
    value :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Rule' with the minimum fields required to make a request.
--
-- * 'attribute' - The rule's stringified attribute. For example, specify the value as @"\"abc\""@ .
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
-- * 'operator' - Specifies how Device Farm compares the rule's attribute to the value. For the operators that are supported by each attribute, see the attribute descriptions.
-- * 'value' - The rule's value.
mkRule ::
  Rule
mkRule =
  Rule'
    { attribute = Lude.Nothing,
      operator = Lude.Nothing,
      value = Lude.Nothing
    }

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
rAttribute :: Lens.Lens' Rule (Lude.Maybe DeviceAttribute)
rAttribute = Lens.lens (attribute :: Rule -> Lude.Maybe DeviceAttribute) (\s a -> s {attribute = a} :: Rule)
{-# DEPRECATED rAttribute "Use generic-lens or generic-optics with 'attribute' instead." #-}

-- | Specifies how Device Farm compares the rule's attribute to the value. For the operators that are supported by each attribute, see the attribute descriptions.
--
-- /Note:/ Consider using 'operator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rOperator :: Lens.Lens' Rule (Lude.Maybe RuleOperator)
rOperator = Lens.lens (operator :: Rule -> Lude.Maybe RuleOperator) (\s a -> s {operator = a} :: Rule)
{-# DEPRECATED rOperator "Use generic-lens or generic-optics with 'operator' instead." #-}

-- | The rule's value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rValue :: Lens.Lens' Rule (Lude.Maybe Lude.Text)
rValue = Lens.lens (value :: Rule -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: Rule)
{-# DEPRECATED rValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.FromJSON Rule where
  parseJSON =
    Lude.withObject
      "Rule"
      ( \x ->
          Rule'
            Lude.<$> (x Lude..:? "attribute")
            Lude.<*> (x Lude..:? "operator")
            Lude.<*> (x Lude..:? "value")
      )

instance Lude.ToJSON Rule where
  toJSON Rule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("attribute" Lude..=) Lude.<$> attribute,
            ("operator" Lude..=) Lude.<$> operator,
            ("value" Lude..=) Lude.<$> value
          ]
      )
