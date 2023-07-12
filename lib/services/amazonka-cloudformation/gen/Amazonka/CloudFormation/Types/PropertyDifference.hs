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
-- Module      : Amazonka.CloudFormation.Types.PropertyDifference
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.PropertyDifference where

import Amazonka.CloudFormation.Types.DifferenceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a resource property whose actual value differs from
-- its expected value, as defined in the stack template and any values
-- specified as template parameters. These will be present only for
-- resources whose @StackResourceDriftStatus@ is @MODIFIED@. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources>.
--
-- /See:/ 'newPropertyDifference' smart constructor.
data PropertyDifference = PropertyDifference'
  { -- | The fully-qualified path to the resource property.
    propertyPath :: Prelude.Text,
    -- | The expected property value of the resource property, as defined in the
    -- stack template and any values specified as template parameters.
    expectedValue :: Prelude.Text,
    -- | The actual property value of the resource property.
    actualValue :: Prelude.Text,
    -- | The type of property difference.
    --
    -- -   @ADD@: A value has been added to a resource property that\'s an
    --     array or list data type.
    --
    -- -   @REMOVE@: The property has been removed from the current resource
    --     configuration.
    --
    -- -   @NOT_EQUAL@: The current property value differs from its expected
    --     value (as defined in the stack template and any values specified as
    --     template parameters).
    differenceType :: DifferenceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PropertyDifference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'propertyPath', 'propertyDifference_propertyPath' - The fully-qualified path to the resource property.
--
-- 'expectedValue', 'propertyDifference_expectedValue' - The expected property value of the resource property, as defined in the
-- stack template and any values specified as template parameters.
--
-- 'actualValue', 'propertyDifference_actualValue' - The actual property value of the resource property.
--
-- 'differenceType', 'propertyDifference_differenceType' - The type of property difference.
--
-- -   @ADD@: A value has been added to a resource property that\'s an
--     array or list data type.
--
-- -   @REMOVE@: The property has been removed from the current resource
--     configuration.
--
-- -   @NOT_EQUAL@: The current property value differs from its expected
--     value (as defined in the stack template and any values specified as
--     template parameters).
newPropertyDifference ::
  -- | 'propertyPath'
  Prelude.Text ->
  -- | 'expectedValue'
  Prelude.Text ->
  -- | 'actualValue'
  Prelude.Text ->
  -- | 'differenceType'
  DifferenceType ->
  PropertyDifference
newPropertyDifference
  pPropertyPath_
  pExpectedValue_
  pActualValue_
  pDifferenceType_ =
    PropertyDifference'
      { propertyPath = pPropertyPath_,
        expectedValue = pExpectedValue_,
        actualValue = pActualValue_,
        differenceType = pDifferenceType_
      }

-- | The fully-qualified path to the resource property.
propertyDifference_propertyPath :: Lens.Lens' PropertyDifference Prelude.Text
propertyDifference_propertyPath = Lens.lens (\PropertyDifference' {propertyPath} -> propertyPath) (\s@PropertyDifference' {} a -> s {propertyPath = a} :: PropertyDifference)

-- | The expected property value of the resource property, as defined in the
-- stack template and any values specified as template parameters.
propertyDifference_expectedValue :: Lens.Lens' PropertyDifference Prelude.Text
propertyDifference_expectedValue = Lens.lens (\PropertyDifference' {expectedValue} -> expectedValue) (\s@PropertyDifference' {} a -> s {expectedValue = a} :: PropertyDifference)

-- | The actual property value of the resource property.
propertyDifference_actualValue :: Lens.Lens' PropertyDifference Prelude.Text
propertyDifference_actualValue = Lens.lens (\PropertyDifference' {actualValue} -> actualValue) (\s@PropertyDifference' {} a -> s {actualValue = a} :: PropertyDifference)

-- | The type of property difference.
--
-- -   @ADD@: A value has been added to a resource property that\'s an
--     array or list data type.
--
-- -   @REMOVE@: The property has been removed from the current resource
--     configuration.
--
-- -   @NOT_EQUAL@: The current property value differs from its expected
--     value (as defined in the stack template and any values specified as
--     template parameters).
propertyDifference_differenceType :: Lens.Lens' PropertyDifference DifferenceType
propertyDifference_differenceType = Lens.lens (\PropertyDifference' {differenceType} -> differenceType) (\s@PropertyDifference' {} a -> s {differenceType = a} :: PropertyDifference)

instance Data.FromXML PropertyDifference where
  parseXML x =
    PropertyDifference'
      Prelude.<$> (x Data..@ "PropertyPath")
      Prelude.<*> (x Data..@ "ExpectedValue")
      Prelude.<*> (x Data..@ "ActualValue")
      Prelude.<*> (x Data..@ "DifferenceType")

instance Prelude.Hashable PropertyDifference where
  hashWithSalt _salt PropertyDifference' {..} =
    _salt
      `Prelude.hashWithSalt` propertyPath
      `Prelude.hashWithSalt` expectedValue
      `Prelude.hashWithSalt` actualValue
      `Prelude.hashWithSalt` differenceType

instance Prelude.NFData PropertyDifference where
  rnf PropertyDifference' {..} =
    Prelude.rnf propertyPath
      `Prelude.seq` Prelude.rnf expectedValue
      `Prelude.seq` Prelude.rnf actualValue
      `Prelude.seq` Prelude.rnf differenceType
