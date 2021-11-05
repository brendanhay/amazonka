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
-- Module      : Amazonka.CognitoIdentityProvider.Types.SchemaAttributeType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.SchemaAttributeType where

import Amazonka.CognitoIdentityProvider.Types.AttributeDataType
import Amazonka.CognitoIdentityProvider.Types.NumberAttributeConstraintsType
import Amazonka.CognitoIdentityProvider.Types.StringAttributeConstraintsType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the schema attribute.
--
-- /See:/ 'newSchemaAttributeType' smart constructor.
data SchemaAttributeType = SchemaAttributeType'
  { -- | Specifies the constraints for an attribute of the number type.
    numberAttributeConstraints :: Prelude.Maybe NumberAttributeConstraintsType,
    -- | Specifies whether a user pool attribute is required. If the attribute is
    -- required and the user does not provide a value, registration or sign-in
    -- will fail.
    required :: Prelude.Maybe Prelude.Bool,
    -- | The attribute data type.
    attributeDataType :: Prelude.Maybe AttributeDataType,
    -- | Specifies the constraints for an attribute of the string type.
    stringAttributeConstraints :: Prelude.Maybe StringAttributeConstraintsType,
    -- | A schema attribute of the name type.
    name :: Prelude.Maybe Prelude.Text,
    -- | We recommend that you use
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UserPoolClientType.html#CognitoUserPools-Type-UserPoolClientType-WriteAttributes WriteAttributes>
    -- in the user pool client to control how attributes can be mutated for new
    -- use cases instead of using @DeveloperOnlyAttribute@.
    --
    -- Specifies whether the attribute type is developer only. This attribute
    -- can only be modified by an administrator. Users will not be able to
    -- modify this attribute using their access token. For example,
    -- @DeveloperOnlyAttribute@ can be modified using AdminUpdateUserAttributes
    -- but cannot be updated using UpdateUserAttributes.
    developerOnlyAttribute :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether the value of the attribute can be changed.
    --
    -- For any user pool attribute that\'s mapped to an identity provider
    -- attribute, you must set this parameter to @true@. Amazon Cognito updates
    -- mapped attributes when users sign in to your application through an
    -- identity provider. If an attribute is immutable, Amazon Cognito throws
    -- an error when it attempts to update the attribute. For more information,
    -- see
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-specifying-attribute-mapping.html Specifying Identity Provider Attribute Mappings for Your User Pool>.
    mutable :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SchemaAttributeType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numberAttributeConstraints', 'schemaAttributeType_numberAttributeConstraints' - Specifies the constraints for an attribute of the number type.
--
-- 'required', 'schemaAttributeType_required' - Specifies whether a user pool attribute is required. If the attribute is
-- required and the user does not provide a value, registration or sign-in
-- will fail.
--
-- 'attributeDataType', 'schemaAttributeType_attributeDataType' - The attribute data type.
--
-- 'stringAttributeConstraints', 'schemaAttributeType_stringAttributeConstraints' - Specifies the constraints for an attribute of the string type.
--
-- 'name', 'schemaAttributeType_name' - A schema attribute of the name type.
--
-- 'developerOnlyAttribute', 'schemaAttributeType_developerOnlyAttribute' - We recommend that you use
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UserPoolClientType.html#CognitoUserPools-Type-UserPoolClientType-WriteAttributes WriteAttributes>
-- in the user pool client to control how attributes can be mutated for new
-- use cases instead of using @DeveloperOnlyAttribute@.
--
-- Specifies whether the attribute type is developer only. This attribute
-- can only be modified by an administrator. Users will not be able to
-- modify this attribute using their access token. For example,
-- @DeveloperOnlyAttribute@ can be modified using AdminUpdateUserAttributes
-- but cannot be updated using UpdateUserAttributes.
--
-- 'mutable', 'schemaAttributeType_mutable' - Specifies whether the value of the attribute can be changed.
--
-- For any user pool attribute that\'s mapped to an identity provider
-- attribute, you must set this parameter to @true@. Amazon Cognito updates
-- mapped attributes when users sign in to your application through an
-- identity provider. If an attribute is immutable, Amazon Cognito throws
-- an error when it attempts to update the attribute. For more information,
-- see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-specifying-attribute-mapping.html Specifying Identity Provider Attribute Mappings for Your User Pool>.
newSchemaAttributeType ::
  SchemaAttributeType
newSchemaAttributeType =
  SchemaAttributeType'
    { numberAttributeConstraints =
        Prelude.Nothing,
      required = Prelude.Nothing,
      attributeDataType = Prelude.Nothing,
      stringAttributeConstraints = Prelude.Nothing,
      name = Prelude.Nothing,
      developerOnlyAttribute = Prelude.Nothing,
      mutable = Prelude.Nothing
    }

-- | Specifies the constraints for an attribute of the number type.
schemaAttributeType_numberAttributeConstraints :: Lens.Lens' SchemaAttributeType (Prelude.Maybe NumberAttributeConstraintsType)
schemaAttributeType_numberAttributeConstraints = Lens.lens (\SchemaAttributeType' {numberAttributeConstraints} -> numberAttributeConstraints) (\s@SchemaAttributeType' {} a -> s {numberAttributeConstraints = a} :: SchemaAttributeType)

-- | Specifies whether a user pool attribute is required. If the attribute is
-- required and the user does not provide a value, registration or sign-in
-- will fail.
schemaAttributeType_required :: Lens.Lens' SchemaAttributeType (Prelude.Maybe Prelude.Bool)
schemaAttributeType_required = Lens.lens (\SchemaAttributeType' {required} -> required) (\s@SchemaAttributeType' {} a -> s {required = a} :: SchemaAttributeType)

-- | The attribute data type.
schemaAttributeType_attributeDataType :: Lens.Lens' SchemaAttributeType (Prelude.Maybe AttributeDataType)
schemaAttributeType_attributeDataType = Lens.lens (\SchemaAttributeType' {attributeDataType} -> attributeDataType) (\s@SchemaAttributeType' {} a -> s {attributeDataType = a} :: SchemaAttributeType)

-- | Specifies the constraints for an attribute of the string type.
schemaAttributeType_stringAttributeConstraints :: Lens.Lens' SchemaAttributeType (Prelude.Maybe StringAttributeConstraintsType)
schemaAttributeType_stringAttributeConstraints = Lens.lens (\SchemaAttributeType' {stringAttributeConstraints} -> stringAttributeConstraints) (\s@SchemaAttributeType' {} a -> s {stringAttributeConstraints = a} :: SchemaAttributeType)

-- | A schema attribute of the name type.
schemaAttributeType_name :: Lens.Lens' SchemaAttributeType (Prelude.Maybe Prelude.Text)
schemaAttributeType_name = Lens.lens (\SchemaAttributeType' {name} -> name) (\s@SchemaAttributeType' {} a -> s {name = a} :: SchemaAttributeType)

-- | We recommend that you use
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UserPoolClientType.html#CognitoUserPools-Type-UserPoolClientType-WriteAttributes WriteAttributes>
-- in the user pool client to control how attributes can be mutated for new
-- use cases instead of using @DeveloperOnlyAttribute@.
--
-- Specifies whether the attribute type is developer only. This attribute
-- can only be modified by an administrator. Users will not be able to
-- modify this attribute using their access token. For example,
-- @DeveloperOnlyAttribute@ can be modified using AdminUpdateUserAttributes
-- but cannot be updated using UpdateUserAttributes.
schemaAttributeType_developerOnlyAttribute :: Lens.Lens' SchemaAttributeType (Prelude.Maybe Prelude.Bool)
schemaAttributeType_developerOnlyAttribute = Lens.lens (\SchemaAttributeType' {developerOnlyAttribute} -> developerOnlyAttribute) (\s@SchemaAttributeType' {} a -> s {developerOnlyAttribute = a} :: SchemaAttributeType)

-- | Specifies whether the value of the attribute can be changed.
--
-- For any user pool attribute that\'s mapped to an identity provider
-- attribute, you must set this parameter to @true@. Amazon Cognito updates
-- mapped attributes when users sign in to your application through an
-- identity provider. If an attribute is immutable, Amazon Cognito throws
-- an error when it attempts to update the attribute. For more information,
-- see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-specifying-attribute-mapping.html Specifying Identity Provider Attribute Mappings for Your User Pool>.
schemaAttributeType_mutable :: Lens.Lens' SchemaAttributeType (Prelude.Maybe Prelude.Bool)
schemaAttributeType_mutable = Lens.lens (\SchemaAttributeType' {mutable} -> mutable) (\s@SchemaAttributeType' {} a -> s {mutable = a} :: SchemaAttributeType)

instance Core.FromJSON SchemaAttributeType where
  parseJSON =
    Core.withObject
      "SchemaAttributeType"
      ( \x ->
          SchemaAttributeType'
            Prelude.<$> (x Core..:? "NumberAttributeConstraints")
            Prelude.<*> (x Core..:? "Required")
            Prelude.<*> (x Core..:? "AttributeDataType")
            Prelude.<*> (x Core..:? "StringAttributeConstraints")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "DeveloperOnlyAttribute")
            Prelude.<*> (x Core..:? "Mutable")
      )

instance Prelude.Hashable SchemaAttributeType

instance Prelude.NFData SchemaAttributeType

instance Core.ToJSON SchemaAttributeType where
  toJSON SchemaAttributeType' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NumberAttributeConstraints" Core..=)
              Prelude.<$> numberAttributeConstraints,
            ("Required" Core..=) Prelude.<$> required,
            ("AttributeDataType" Core..=)
              Prelude.<$> attributeDataType,
            ("StringAttributeConstraints" Core..=)
              Prelude.<$> stringAttributeConstraints,
            ("Name" Core..=) Prelude.<$> name,
            ("DeveloperOnlyAttribute" Core..=)
              Prelude.<$> developerOnlyAttribute,
            ("Mutable" Core..=) Prelude.<$> mutable
          ]
      )
