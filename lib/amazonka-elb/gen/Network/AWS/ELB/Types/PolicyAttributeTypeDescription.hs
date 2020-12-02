{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.PolicyAttributeTypeDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.PolicyAttributeTypeDescription where

import Network.AWS.ELB.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a policy attribute type.
--
--
--
-- /See:/ 'policyAttributeTypeDescription' smart constructor.
data PolicyAttributeTypeDescription = PolicyAttributeTypeDescription'
  { _patdAttributeType ::
      !(Maybe Text),
    _patdCardinality ::
      !(Maybe Text),
    _patdDefaultValue ::
      !(Maybe Text),
    _patdAttributeName ::
      !(Maybe Text),
    _patdDescription ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PolicyAttributeTypeDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'patdAttributeType' - The type of the attribute. For example, @Boolean@ or @Integer@ .
--
-- * 'patdCardinality' - The cardinality of the attribute. Valid values:     * ONE(1) : Single value required     * ZERO_OR_ONE(0..1) : Up to one value is allowed     * ZERO_OR_MORE(0..*) : Optional. Multiple values are allowed     * ONE_OR_MORE(1..*0) : Required. Multiple values are allowed
--
-- * 'patdDefaultValue' - The default value of the attribute, if applicable.
--
-- * 'patdAttributeName' - The name of the attribute.
--
-- * 'patdDescription' - A description of the attribute.
policyAttributeTypeDescription ::
  PolicyAttributeTypeDescription
policyAttributeTypeDescription =
  PolicyAttributeTypeDescription'
    { _patdAttributeType = Nothing,
      _patdCardinality = Nothing,
      _patdDefaultValue = Nothing,
      _patdAttributeName = Nothing,
      _patdDescription = Nothing
    }

-- | The type of the attribute. For example, @Boolean@ or @Integer@ .
patdAttributeType :: Lens' PolicyAttributeTypeDescription (Maybe Text)
patdAttributeType = lens _patdAttributeType (\s a -> s {_patdAttributeType = a})

-- | The cardinality of the attribute. Valid values:     * ONE(1) : Single value required     * ZERO_OR_ONE(0..1) : Up to one value is allowed     * ZERO_OR_MORE(0..*) : Optional. Multiple values are allowed     * ONE_OR_MORE(1..*0) : Required. Multiple values are allowed
patdCardinality :: Lens' PolicyAttributeTypeDescription (Maybe Text)
patdCardinality = lens _patdCardinality (\s a -> s {_patdCardinality = a})

-- | The default value of the attribute, if applicable.
patdDefaultValue :: Lens' PolicyAttributeTypeDescription (Maybe Text)
patdDefaultValue = lens _patdDefaultValue (\s a -> s {_patdDefaultValue = a})

-- | The name of the attribute.
patdAttributeName :: Lens' PolicyAttributeTypeDescription (Maybe Text)
patdAttributeName = lens _patdAttributeName (\s a -> s {_patdAttributeName = a})

-- | A description of the attribute.
patdDescription :: Lens' PolicyAttributeTypeDescription (Maybe Text)
patdDescription = lens _patdDescription (\s a -> s {_patdDescription = a})

instance FromXML PolicyAttributeTypeDescription where
  parseXML x =
    PolicyAttributeTypeDescription'
      <$> (x .@? "AttributeType")
      <*> (x .@? "Cardinality")
      <*> (x .@? "DefaultValue")
      <*> (x .@? "AttributeName")
      <*> (x .@? "Description")

instance Hashable PolicyAttributeTypeDescription

instance NFData PolicyAttributeTypeDescription
