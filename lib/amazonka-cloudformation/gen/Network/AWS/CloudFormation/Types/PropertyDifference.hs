{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.PropertyDifference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.PropertyDifference
  ( PropertyDifference (..),

    -- * Smart constructor
    mkPropertyDifference,

    -- * Lenses
    pdPropertyPath,
    pdExpectedValue,
    pdActualValue,
    pdDifferenceType,
  )
where

import Network.AWS.CloudFormation.Types.DifferenceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a resource property whose actual value differs from its expected value, as defined in the stack template and any values specified as template parameters. These will be present only for resources whose @StackResourceDriftStatus@ is @MODIFIED@ . For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
--
-- /See:/ 'mkPropertyDifference' smart constructor.
data PropertyDifference = PropertyDifference'
  { propertyPath ::
      Lude.Text,
    expectedValue :: Lude.Text,
    actualValue :: Lude.Text,
    differenceType :: DifferenceType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PropertyDifference' with the minimum fields required to make a request.
--
-- * 'actualValue' - The actual property value of the resource property.
-- * 'differenceType' - The type of property difference.
--
--
--     * @ADD@ : A value has been added to a resource property that is an array or list data type.
--
--
--     * @REMOVE@ : The property has been removed from the current resource configuration.
--
--
--     * @NOT_EQUAL@ : The current property value differs from its expected value (as defined in the stack template and any values specified as template parameters).
--
--
-- * 'expectedValue' - The expected property value of the resource property, as defined in the stack template and any values specified as template parameters.
-- * 'propertyPath' - The fully-qualified path to the resource property.
mkPropertyDifference ::
  -- | 'propertyPath'
  Lude.Text ->
  -- | 'expectedValue'
  Lude.Text ->
  -- | 'actualValue'
  Lude.Text ->
  -- | 'differenceType'
  DifferenceType ->
  PropertyDifference
mkPropertyDifference
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
--
-- /Note:/ Consider using 'propertyPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPropertyPath :: Lens.Lens' PropertyDifference Lude.Text
pdPropertyPath = Lens.lens (propertyPath :: PropertyDifference -> Lude.Text) (\s a -> s {propertyPath = a} :: PropertyDifference)
{-# DEPRECATED pdPropertyPath "Use generic-lens or generic-optics with 'propertyPath' instead." #-}

-- | The expected property value of the resource property, as defined in the stack template and any values specified as template parameters.
--
-- /Note:/ Consider using 'expectedValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdExpectedValue :: Lens.Lens' PropertyDifference Lude.Text
pdExpectedValue = Lens.lens (expectedValue :: PropertyDifference -> Lude.Text) (\s a -> s {expectedValue = a} :: PropertyDifference)
{-# DEPRECATED pdExpectedValue "Use generic-lens or generic-optics with 'expectedValue' instead." #-}

-- | The actual property value of the resource property.
--
-- /Note:/ Consider using 'actualValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdActualValue :: Lens.Lens' PropertyDifference Lude.Text
pdActualValue = Lens.lens (actualValue :: PropertyDifference -> Lude.Text) (\s a -> s {actualValue = a} :: PropertyDifference)
{-# DEPRECATED pdActualValue "Use generic-lens or generic-optics with 'actualValue' instead." #-}

-- | The type of property difference.
--
--
--     * @ADD@ : A value has been added to a resource property that is an array or list data type.
--
--
--     * @REMOVE@ : The property has been removed from the current resource configuration.
--
--
--     * @NOT_EQUAL@ : The current property value differs from its expected value (as defined in the stack template and any values specified as template parameters).
--
--
--
-- /Note:/ Consider using 'differenceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdDifferenceType :: Lens.Lens' PropertyDifference DifferenceType
pdDifferenceType = Lens.lens (differenceType :: PropertyDifference -> DifferenceType) (\s a -> s {differenceType = a} :: PropertyDifference)
{-# DEPRECATED pdDifferenceType "Use generic-lens or generic-optics with 'differenceType' instead." #-}

instance Lude.FromXML PropertyDifference where
  parseXML x =
    PropertyDifference'
      Lude.<$> (x Lude..@ "PropertyPath")
      Lude.<*> (x Lude..@ "ExpectedValue")
      Lude.<*> (x Lude..@ "ActualValue")
      Lude.<*> (x Lude..@ "DifferenceType")
