{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.PropertyDifference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.PropertyDifference where

import Network.AWS.CloudFormation.Types.DifferenceType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a resource property whose actual value differs from its expected value, as defined in the stack template and any values specified as template parameters. These will be present only for resources whose @StackResourceDriftStatus@ is @MODIFIED@ . For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
--
--
--
-- /See:/ 'propertyDifference' smart constructor.
data PropertyDifference = PropertyDifference'
  { _pdPropertyPath ::
      !Text,
    _pdExpectedValue :: !Text,
    _pdActualValue :: !Text,
    _pdDifferenceType :: !DifferenceType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PropertyDifference' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdPropertyPath' - The fully-qualified path to the resource property.
--
-- * 'pdExpectedValue' - The expected property value of the resource property, as defined in the stack template and any values specified as template parameters.
--
-- * 'pdActualValue' - The actual property value of the resource property.
--
-- * 'pdDifferenceType' - The type of property difference.     * @ADD@ : A value has been added to a resource property that is an array or list data type.     * @REMOVE@ : The property has been removed from the current resource configuration.     * @NOT_EQUAL@ : The current property value differs from its expected value (as defined in the stack template and any values specified as template parameters).
propertyDifference ::
  -- | 'pdPropertyPath'
  Text ->
  -- | 'pdExpectedValue'
  Text ->
  -- | 'pdActualValue'
  Text ->
  -- | 'pdDifferenceType'
  DifferenceType ->
  PropertyDifference
propertyDifference
  pPropertyPath_
  pExpectedValue_
  pActualValue_
  pDifferenceType_ =
    PropertyDifference'
      { _pdPropertyPath = pPropertyPath_,
        _pdExpectedValue = pExpectedValue_,
        _pdActualValue = pActualValue_,
        _pdDifferenceType = pDifferenceType_
      }

-- | The fully-qualified path to the resource property.
pdPropertyPath :: Lens' PropertyDifference Text
pdPropertyPath = lens _pdPropertyPath (\s a -> s {_pdPropertyPath = a})

-- | The expected property value of the resource property, as defined in the stack template and any values specified as template parameters.
pdExpectedValue :: Lens' PropertyDifference Text
pdExpectedValue = lens _pdExpectedValue (\s a -> s {_pdExpectedValue = a})

-- | The actual property value of the resource property.
pdActualValue :: Lens' PropertyDifference Text
pdActualValue = lens _pdActualValue (\s a -> s {_pdActualValue = a})

-- | The type of property difference.     * @ADD@ : A value has been added to a resource property that is an array or list data type.     * @REMOVE@ : The property has been removed from the current resource configuration.     * @NOT_EQUAL@ : The current property value differs from its expected value (as defined in the stack template and any values specified as template parameters).
pdDifferenceType :: Lens' PropertyDifference DifferenceType
pdDifferenceType = lens _pdDifferenceType (\s a -> s {_pdDifferenceType = a})

instance FromXML PropertyDifference where
  parseXML x =
    PropertyDifference'
      <$> (x .@ "PropertyPath")
      <*> (x .@ "ExpectedValue")
      <*> (x .@ "ActualValue")
      <*> (x .@ "DifferenceType")

instance Hashable PropertyDifference

instance NFData PropertyDifference
