{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.PropertyDifference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.PropertyDifference
  ( PropertyDifference (..)
  -- * Smart constructor
  , mkPropertyDifference
  -- * Lenses
  , pdPropertyPath
  , pdExpectedValue
  , pdActualValue
  , pdDifferenceType
  ) where

import qualified Network.AWS.CloudFormation.Types.DifferenceType as Types
import qualified Network.AWS.CloudFormation.Types.PropertyPath as Types
import qualified Network.AWS.CloudFormation.Types.PropertyValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a resource property whose actual value differs from its expected value, as defined in the stack template and any values specified as template parameters. These will be present only for resources whose @StackResourceDriftStatus@ is @MODIFIED@ . For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
--
-- /See:/ 'mkPropertyDifference' smart constructor.
data PropertyDifference = PropertyDifference'
  { propertyPath :: Types.PropertyPath
    -- ^ The fully-qualified path to the resource property.
  , expectedValue :: Types.PropertyValue
    -- ^ The expected property value of the resource property, as defined in the stack template and any values specified as template parameters.
  , actualValue :: Types.PropertyValue
    -- ^ The actual property value of the resource property.
  , differenceType :: Types.DifferenceType
    -- ^ The type of property difference.
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PropertyDifference' value with any optional fields omitted.
mkPropertyDifference
    :: Types.PropertyPath -- ^ 'propertyPath'
    -> Types.PropertyValue -- ^ 'expectedValue'
    -> Types.PropertyValue -- ^ 'actualValue'
    -> Types.DifferenceType -- ^ 'differenceType'
    -> PropertyDifference
mkPropertyDifference propertyPath expectedValue actualValue
  differenceType
  = PropertyDifference'{propertyPath, expectedValue, actualValue,
                        differenceType}

-- | The fully-qualified path to the resource property.
--
-- /Note:/ Consider using 'propertyPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPropertyPath :: Lens.Lens' PropertyDifference Types.PropertyPath
pdPropertyPath = Lens.field @"propertyPath"
{-# INLINEABLE pdPropertyPath #-}
{-# DEPRECATED propertyPath "Use generic-lens or generic-optics with 'propertyPath' instead"  #-}

-- | The expected property value of the resource property, as defined in the stack template and any values specified as template parameters.
--
-- /Note:/ Consider using 'expectedValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdExpectedValue :: Lens.Lens' PropertyDifference Types.PropertyValue
pdExpectedValue = Lens.field @"expectedValue"
{-# INLINEABLE pdExpectedValue #-}
{-# DEPRECATED expectedValue "Use generic-lens or generic-optics with 'expectedValue' instead"  #-}

-- | The actual property value of the resource property.
--
-- /Note:/ Consider using 'actualValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdActualValue :: Lens.Lens' PropertyDifference Types.PropertyValue
pdActualValue = Lens.field @"actualValue"
{-# INLINEABLE pdActualValue #-}
{-# DEPRECATED actualValue "Use generic-lens or generic-optics with 'actualValue' instead"  #-}

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
pdDifferenceType :: Lens.Lens' PropertyDifference Types.DifferenceType
pdDifferenceType = Lens.field @"differenceType"
{-# INLINEABLE pdDifferenceType #-}
{-# DEPRECATED differenceType "Use generic-lens or generic-optics with 'differenceType' instead"  #-}

instance Core.FromXML PropertyDifference where
        parseXML x
          = PropertyDifference' Core.<$>
              (x Core..@ "PropertyPath") Core.<*> x Core..@ "ExpectedValue"
                Core.<*> x Core..@ "ActualValue"
                Core.<*> x Core..@ "DifferenceType"
