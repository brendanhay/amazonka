{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.SchemaAttributeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.SchemaAttributeType where

import Network.AWS.CognitoIdentityProvider.Types.AttributeDataType
import Network.AWS.CognitoIdentityProvider.Types.NumberAttributeConstraintsType
import Network.AWS.CognitoIdentityProvider.Types.StringAttributeConstraintsType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the schema attribute.
--
--
--
-- /See:/ 'schemaAttributeType' smart constructor.
data SchemaAttributeType = SchemaAttributeType'
  { _satNumberAttributeConstraints ::
      !(Maybe NumberAttributeConstraintsType),
    _satRequired :: !(Maybe Bool),
    _satAttributeDataType :: !(Maybe AttributeDataType),
    _satStringAttributeConstraints ::
      !(Maybe StringAttributeConstraintsType),
    _satName :: !(Maybe Text),
    _satDeveloperOnlyAttribute :: !(Maybe Bool),
    _satMutable :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SchemaAttributeType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'satNumberAttributeConstraints' - Specifies the constraints for an attribute of the number type.
--
-- * 'satRequired' - Specifies whether a user pool attribute is required. If the attribute is required and the user does not provide a value, registration or sign-in will fail.
--
-- * 'satAttributeDataType' - The attribute data type.
--
-- * 'satStringAttributeConstraints' - Specifies the constraints for an attribute of the string type.
--
-- * 'satName' - A schema attribute of the name type.
--
-- * 'satDeveloperOnlyAttribute' - Specifies whether the attribute type is developer only. This attribute can only be modified by an administrator. Users will not be able to modify this attribute using their access token. For example, @DeveloperOnlyAttribute@ can be modified using AdminUpdateUserAttributes but cannot be updated using UpdateUserAttributes.
--
-- * 'satMutable' - Specifies whether the value of the attribute can be changed. For any user pool attribute that's mapped to an identity provider attribute, you must set this parameter to @true@ . Amazon Cognito updates mapped attributes when users sign in to your application through an identity provider. If an attribute is immutable, Amazon Cognito throws an error when it attempts to update the attribute. For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-specifying-attribute-mapping.html Specifying Identity Provider Attribute Mappings for Your User Pool> .
schemaAttributeType ::
  SchemaAttributeType
schemaAttributeType =
  SchemaAttributeType'
    { _satNumberAttributeConstraints = Nothing,
      _satRequired = Nothing,
      _satAttributeDataType = Nothing,
      _satStringAttributeConstraints = Nothing,
      _satName = Nothing,
      _satDeveloperOnlyAttribute = Nothing,
      _satMutable = Nothing
    }

-- | Specifies the constraints for an attribute of the number type.
satNumberAttributeConstraints :: Lens' SchemaAttributeType (Maybe NumberAttributeConstraintsType)
satNumberAttributeConstraints = lens _satNumberAttributeConstraints (\s a -> s {_satNumberAttributeConstraints = a})

-- | Specifies whether a user pool attribute is required. If the attribute is required and the user does not provide a value, registration or sign-in will fail.
satRequired :: Lens' SchemaAttributeType (Maybe Bool)
satRequired = lens _satRequired (\s a -> s {_satRequired = a})

-- | The attribute data type.
satAttributeDataType :: Lens' SchemaAttributeType (Maybe AttributeDataType)
satAttributeDataType = lens _satAttributeDataType (\s a -> s {_satAttributeDataType = a})

-- | Specifies the constraints for an attribute of the string type.
satStringAttributeConstraints :: Lens' SchemaAttributeType (Maybe StringAttributeConstraintsType)
satStringAttributeConstraints = lens _satStringAttributeConstraints (\s a -> s {_satStringAttributeConstraints = a})

-- | A schema attribute of the name type.
satName :: Lens' SchemaAttributeType (Maybe Text)
satName = lens _satName (\s a -> s {_satName = a})

-- | Specifies whether the attribute type is developer only. This attribute can only be modified by an administrator. Users will not be able to modify this attribute using their access token. For example, @DeveloperOnlyAttribute@ can be modified using AdminUpdateUserAttributes but cannot be updated using UpdateUserAttributes.
satDeveloperOnlyAttribute :: Lens' SchemaAttributeType (Maybe Bool)
satDeveloperOnlyAttribute = lens _satDeveloperOnlyAttribute (\s a -> s {_satDeveloperOnlyAttribute = a})

-- | Specifies whether the value of the attribute can be changed. For any user pool attribute that's mapped to an identity provider attribute, you must set this parameter to @true@ . Amazon Cognito updates mapped attributes when users sign in to your application through an identity provider. If an attribute is immutable, Amazon Cognito throws an error when it attempts to update the attribute. For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-specifying-attribute-mapping.html Specifying Identity Provider Attribute Mappings for Your User Pool> .
satMutable :: Lens' SchemaAttributeType (Maybe Bool)
satMutable = lens _satMutable (\s a -> s {_satMutable = a})

instance FromJSON SchemaAttributeType where
  parseJSON =
    withObject
      "SchemaAttributeType"
      ( \x ->
          SchemaAttributeType'
            <$> (x .:? "NumberAttributeConstraints")
            <*> (x .:? "Required")
            <*> (x .:? "AttributeDataType")
            <*> (x .:? "StringAttributeConstraints")
            <*> (x .:? "Name")
            <*> (x .:? "DeveloperOnlyAttribute")
            <*> (x .:? "Mutable")
      )

instance Hashable SchemaAttributeType

instance NFData SchemaAttributeType

instance ToJSON SchemaAttributeType where
  toJSON SchemaAttributeType' {..} =
    object
      ( catMaybes
          [ ("NumberAttributeConstraints" .=)
              <$> _satNumberAttributeConstraints,
            ("Required" .=) <$> _satRequired,
            ("AttributeDataType" .=) <$> _satAttributeDataType,
            ("StringAttributeConstraints" .=)
              <$> _satStringAttributeConstraints,
            ("Name" .=) <$> _satName,
            ("DeveloperOnlyAttribute" .=) <$> _satDeveloperOnlyAttribute,
            ("Mutable" .=) <$> _satMutable
          ]
      )
