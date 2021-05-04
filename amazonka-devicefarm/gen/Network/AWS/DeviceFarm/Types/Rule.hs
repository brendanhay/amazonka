{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Rule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Rule where

import Network.AWS.DeviceFarm.Types.DeviceAttribute
import Network.AWS.DeviceFarm.Types.RuleOperator
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a condition for a device pool.
--
-- /See:/ 'newRule' smart constructor.
data Rule = Rule'
  { -- | Specifies how Device Farm compares the rule\'s attribute to the value.
    -- For the operators that are supported by each attribute, see the
    -- attribute descriptions.
    operator :: Prelude.Maybe RuleOperator,
    -- | The rule\'s stringified attribute. For example, specify the value as
    -- @\"\\\"abc\\\"\"@.
    --
    -- The supported operators for each attribute are provided in the following
    -- list.
    --
    -- [APPIUM_VERSION]
    --     The Appium version for the test.
    --
    --     Supported operators: @CONTAINS@
    --
    -- [ARN]
    --     The Amazon Resource Name (ARN) of the device (for example,
    --     @arn:aws:devicefarm:us-west-2::device:12345Example@.
    --
    --     Supported operators: @EQUALS@, @IN@, @NOT_IN@
    --
    -- [AVAILABILITY]
    --     The current availability of the device. Valid values are AVAILABLE,
    --     HIGHLY_AVAILABLE, BUSY, or TEMPORARY_NOT_AVAILABLE.
    --
    --     Supported operators: @EQUALS@
    --
    -- [FLEET_TYPE]
    --     The fleet type. Valid values are PUBLIC or PRIVATE.
    --
    --     Supported operators: @EQUALS@
    --
    -- [FORM_FACTOR]
    --     The device form factor. Valid values are PHONE or TABLET.
    --
    --     Supported operators: @EQUALS@, @IN@, @NOT_IN@
    --
    -- [INSTANCE_ARN]
    --     The Amazon Resource Name (ARN) of the device instance.
    --
    --     Supported operators: @IN@, @NOT_IN@
    --
    -- [INSTANCE_LABELS]
    --     The label of the device instance.
    --
    --     Supported operators: @CONTAINS@
    --
    -- [MANUFACTURER]
    --     The device manufacturer (for example, Apple).
    --
    --     Supported operators: @EQUALS@, @IN@, @NOT_IN@
    --
    -- [MODEL]
    --     The device model, such as Apple iPad Air 2 or Google Pixel.
    --
    --     Supported operators: @CONTAINS@, @EQUALS@, @IN@, @NOT_IN@
    --
    -- [OS_VERSION]
    --     The operating system version (for example, 10.3.2).
    --
    --     Supported operators: @EQUALS@, @GREATER_THAN@,
    --     @GREATER_THAN_OR_EQUALS@, @IN@, @LESS_THAN@, @LESS_THAN_OR_EQUALS@,
    --     @NOT_IN@
    --
    -- [PLATFORM]
    --     The device platform. Valid values are ANDROID or IOS.
    --
    --     Supported operators: @EQUALS@, @IN@, @NOT_IN@
    --
    -- [REMOTE_ACCESS_ENABLED]
    --     Whether the device is enabled for remote access. Valid values are
    --     TRUE or FALSE.
    --
    --     Supported operators: @EQUALS@
    --
    -- [REMOTE_DEBUG_ENABLED]
    --     Whether the device is enabled for remote debugging. Valid values are
    --     TRUE or FALSE.
    --
    --     Supported operators: @EQUALS@
    --
    --     Because remote debugging is
    --     <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>,
    --     this filter is ignored.
    attribute :: Prelude.Maybe DeviceAttribute,
    -- | The rule\'s value.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Rule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operator', 'rule_operator' - Specifies how Device Farm compares the rule\'s attribute to the value.
-- For the operators that are supported by each attribute, see the
-- attribute descriptions.
--
-- 'attribute', 'rule_attribute' - The rule\'s stringified attribute. For example, specify the value as
-- @\"\\\"abc\\\"\"@.
--
-- The supported operators for each attribute are provided in the following
-- list.
--
-- [APPIUM_VERSION]
--     The Appium version for the test.
--
--     Supported operators: @CONTAINS@
--
-- [ARN]
--     The Amazon Resource Name (ARN) of the device (for example,
--     @arn:aws:devicefarm:us-west-2::device:12345Example@.
--
--     Supported operators: @EQUALS@, @IN@, @NOT_IN@
--
-- [AVAILABILITY]
--     The current availability of the device. Valid values are AVAILABLE,
--     HIGHLY_AVAILABLE, BUSY, or TEMPORARY_NOT_AVAILABLE.
--
--     Supported operators: @EQUALS@
--
-- [FLEET_TYPE]
--     The fleet type. Valid values are PUBLIC or PRIVATE.
--
--     Supported operators: @EQUALS@
--
-- [FORM_FACTOR]
--     The device form factor. Valid values are PHONE or TABLET.
--
--     Supported operators: @EQUALS@, @IN@, @NOT_IN@
--
-- [INSTANCE_ARN]
--     The Amazon Resource Name (ARN) of the device instance.
--
--     Supported operators: @IN@, @NOT_IN@
--
-- [INSTANCE_LABELS]
--     The label of the device instance.
--
--     Supported operators: @CONTAINS@
--
-- [MANUFACTURER]
--     The device manufacturer (for example, Apple).
--
--     Supported operators: @EQUALS@, @IN@, @NOT_IN@
--
-- [MODEL]
--     The device model, such as Apple iPad Air 2 or Google Pixel.
--
--     Supported operators: @CONTAINS@, @EQUALS@, @IN@, @NOT_IN@
--
-- [OS_VERSION]
--     The operating system version (for example, 10.3.2).
--
--     Supported operators: @EQUALS@, @GREATER_THAN@,
--     @GREATER_THAN_OR_EQUALS@, @IN@, @LESS_THAN@, @LESS_THAN_OR_EQUALS@,
--     @NOT_IN@
--
-- [PLATFORM]
--     The device platform. Valid values are ANDROID or IOS.
--
--     Supported operators: @EQUALS@, @IN@, @NOT_IN@
--
-- [REMOTE_ACCESS_ENABLED]
--     Whether the device is enabled for remote access. Valid values are
--     TRUE or FALSE.
--
--     Supported operators: @EQUALS@
--
-- [REMOTE_DEBUG_ENABLED]
--     Whether the device is enabled for remote debugging. Valid values are
--     TRUE or FALSE.
--
--     Supported operators: @EQUALS@
--
--     Because remote debugging is
--     <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>,
--     this filter is ignored.
--
-- 'value', 'rule_value' - The rule\'s value.
newRule ::
  Rule
newRule =
  Rule'
    { operator = Prelude.Nothing,
      attribute = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | Specifies how Device Farm compares the rule\'s attribute to the value.
-- For the operators that are supported by each attribute, see the
-- attribute descriptions.
rule_operator :: Lens.Lens' Rule (Prelude.Maybe RuleOperator)
rule_operator = Lens.lens (\Rule' {operator} -> operator) (\s@Rule' {} a -> s {operator = a} :: Rule)

-- | The rule\'s stringified attribute. For example, specify the value as
-- @\"\\\"abc\\\"\"@.
--
-- The supported operators for each attribute are provided in the following
-- list.
--
-- [APPIUM_VERSION]
--     The Appium version for the test.
--
--     Supported operators: @CONTAINS@
--
-- [ARN]
--     The Amazon Resource Name (ARN) of the device (for example,
--     @arn:aws:devicefarm:us-west-2::device:12345Example@.
--
--     Supported operators: @EQUALS@, @IN@, @NOT_IN@
--
-- [AVAILABILITY]
--     The current availability of the device. Valid values are AVAILABLE,
--     HIGHLY_AVAILABLE, BUSY, or TEMPORARY_NOT_AVAILABLE.
--
--     Supported operators: @EQUALS@
--
-- [FLEET_TYPE]
--     The fleet type. Valid values are PUBLIC or PRIVATE.
--
--     Supported operators: @EQUALS@
--
-- [FORM_FACTOR]
--     The device form factor. Valid values are PHONE or TABLET.
--
--     Supported operators: @EQUALS@, @IN@, @NOT_IN@
--
-- [INSTANCE_ARN]
--     The Amazon Resource Name (ARN) of the device instance.
--
--     Supported operators: @IN@, @NOT_IN@
--
-- [INSTANCE_LABELS]
--     The label of the device instance.
--
--     Supported operators: @CONTAINS@
--
-- [MANUFACTURER]
--     The device manufacturer (for example, Apple).
--
--     Supported operators: @EQUALS@, @IN@, @NOT_IN@
--
-- [MODEL]
--     The device model, such as Apple iPad Air 2 or Google Pixel.
--
--     Supported operators: @CONTAINS@, @EQUALS@, @IN@, @NOT_IN@
--
-- [OS_VERSION]
--     The operating system version (for example, 10.3.2).
--
--     Supported operators: @EQUALS@, @GREATER_THAN@,
--     @GREATER_THAN_OR_EQUALS@, @IN@, @LESS_THAN@, @LESS_THAN_OR_EQUALS@,
--     @NOT_IN@
--
-- [PLATFORM]
--     The device platform. Valid values are ANDROID or IOS.
--
--     Supported operators: @EQUALS@, @IN@, @NOT_IN@
--
-- [REMOTE_ACCESS_ENABLED]
--     Whether the device is enabled for remote access. Valid values are
--     TRUE or FALSE.
--
--     Supported operators: @EQUALS@
--
-- [REMOTE_DEBUG_ENABLED]
--     Whether the device is enabled for remote debugging. Valid values are
--     TRUE or FALSE.
--
--     Supported operators: @EQUALS@
--
--     Because remote debugging is
--     <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>,
--     this filter is ignored.
rule_attribute :: Lens.Lens' Rule (Prelude.Maybe DeviceAttribute)
rule_attribute = Lens.lens (\Rule' {attribute} -> attribute) (\s@Rule' {} a -> s {attribute = a} :: Rule)

-- | The rule\'s value.
rule_value :: Lens.Lens' Rule (Prelude.Maybe Prelude.Text)
rule_value = Lens.lens (\Rule' {value} -> value) (\s@Rule' {} a -> s {value = a} :: Rule)

instance Prelude.FromJSON Rule where
  parseJSON =
    Prelude.withObject
      "Rule"
      ( \x ->
          Rule'
            Prelude.<$> (x Prelude..:? "operator")
            Prelude.<*> (x Prelude..:? "attribute")
            Prelude.<*> (x Prelude..:? "value")
      )

instance Prelude.Hashable Rule

instance Prelude.NFData Rule

instance Prelude.ToJSON Rule where
  toJSON Rule' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("operator" Prelude..=) Prelude.<$> operator,
            ("attribute" Prelude..=) Prelude.<$> attribute,
            ("value" Prelude..=) Prelude.<$> value
          ]
      )
