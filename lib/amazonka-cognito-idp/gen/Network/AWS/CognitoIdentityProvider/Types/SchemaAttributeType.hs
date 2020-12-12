{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.SchemaAttributeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.SchemaAttributeType
  ( SchemaAttributeType (..),

    -- * Smart constructor
    mkSchemaAttributeType,

    -- * Lenses
    satNumberAttributeConstraints,
    satRequired,
    satAttributeDataType,
    satStringAttributeConstraints,
    satName,
    satDeveloperOnlyAttribute,
    satMutable,
  )
where

import Network.AWS.CognitoIdentityProvider.Types.AttributeDataType
import Network.AWS.CognitoIdentityProvider.Types.NumberAttributeConstraintsType
import Network.AWS.CognitoIdentityProvider.Types.StringAttributeConstraintsType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the schema attribute.
--
-- /See:/ 'mkSchemaAttributeType' smart constructor.
data SchemaAttributeType = SchemaAttributeType'
  { numberAttributeConstraints ::
      Lude.Maybe NumberAttributeConstraintsType,
    required :: Lude.Maybe Lude.Bool,
    attributeDataType :: Lude.Maybe AttributeDataType,
    stringAttributeConstraints ::
      Lude.Maybe StringAttributeConstraintsType,
    name :: Lude.Maybe Lude.Text,
    developerOnlyAttribute :: Lude.Maybe Lude.Bool,
    mutable :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SchemaAttributeType' with the minimum fields required to make a request.
--
-- * 'attributeDataType' - The attribute data type.
-- * 'developerOnlyAttribute' - Specifies whether the attribute type is developer only. This attribute can only be modified by an administrator. Users will not be able to modify this attribute using their access token. For example, @DeveloperOnlyAttribute@ can be modified using AdminUpdateUserAttributes but cannot be updated using UpdateUserAttributes.
-- * 'mutable' - Specifies whether the value of the attribute can be changed.
--
-- For any user pool attribute that's mapped to an identity provider attribute, you must set this parameter to @true@ . Amazon Cognito updates mapped attributes when users sign in to your application through an identity provider. If an attribute is immutable, Amazon Cognito throws an error when it attempts to update the attribute. For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-specifying-attribute-mapping.html Specifying Identity Provider Attribute Mappings for Your User Pool> .
-- * 'name' - A schema attribute of the name type.
-- * 'numberAttributeConstraints' - Specifies the constraints for an attribute of the number type.
-- * 'required' - Specifies whether a user pool attribute is required. If the attribute is required and the user does not provide a value, registration or sign-in will fail.
-- * 'stringAttributeConstraints' - Specifies the constraints for an attribute of the string type.
mkSchemaAttributeType ::
  SchemaAttributeType
mkSchemaAttributeType =
  SchemaAttributeType'
    { numberAttributeConstraints = Lude.Nothing,
      required = Lude.Nothing,
      attributeDataType = Lude.Nothing,
      stringAttributeConstraints = Lude.Nothing,
      name = Lude.Nothing,
      developerOnlyAttribute = Lude.Nothing,
      mutable = Lude.Nothing
    }

-- | Specifies the constraints for an attribute of the number type.
--
-- /Note:/ Consider using 'numberAttributeConstraints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satNumberAttributeConstraints :: Lens.Lens' SchemaAttributeType (Lude.Maybe NumberAttributeConstraintsType)
satNumberAttributeConstraints = Lens.lens (numberAttributeConstraints :: SchemaAttributeType -> Lude.Maybe NumberAttributeConstraintsType) (\s a -> s {numberAttributeConstraints = a} :: SchemaAttributeType)
{-# DEPRECATED satNumberAttributeConstraints "Use generic-lens or generic-optics with 'numberAttributeConstraints' instead." #-}

-- | Specifies whether a user pool attribute is required. If the attribute is required and the user does not provide a value, registration or sign-in will fail.
--
-- /Note:/ Consider using 'required' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satRequired :: Lens.Lens' SchemaAttributeType (Lude.Maybe Lude.Bool)
satRequired = Lens.lens (required :: SchemaAttributeType -> Lude.Maybe Lude.Bool) (\s a -> s {required = a} :: SchemaAttributeType)
{-# DEPRECATED satRequired "Use generic-lens or generic-optics with 'required' instead." #-}

-- | The attribute data type.
--
-- /Note:/ Consider using 'attributeDataType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satAttributeDataType :: Lens.Lens' SchemaAttributeType (Lude.Maybe AttributeDataType)
satAttributeDataType = Lens.lens (attributeDataType :: SchemaAttributeType -> Lude.Maybe AttributeDataType) (\s a -> s {attributeDataType = a} :: SchemaAttributeType)
{-# DEPRECATED satAttributeDataType "Use generic-lens or generic-optics with 'attributeDataType' instead." #-}

-- | Specifies the constraints for an attribute of the string type.
--
-- /Note:/ Consider using 'stringAttributeConstraints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satStringAttributeConstraints :: Lens.Lens' SchemaAttributeType (Lude.Maybe StringAttributeConstraintsType)
satStringAttributeConstraints = Lens.lens (stringAttributeConstraints :: SchemaAttributeType -> Lude.Maybe StringAttributeConstraintsType) (\s a -> s {stringAttributeConstraints = a} :: SchemaAttributeType)
{-# DEPRECATED satStringAttributeConstraints "Use generic-lens or generic-optics with 'stringAttributeConstraints' instead." #-}

-- | A schema attribute of the name type.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satName :: Lens.Lens' SchemaAttributeType (Lude.Maybe Lude.Text)
satName = Lens.lens (name :: SchemaAttributeType -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: SchemaAttributeType)
{-# DEPRECATED satName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Specifies whether the attribute type is developer only. This attribute can only be modified by an administrator. Users will not be able to modify this attribute using their access token. For example, @DeveloperOnlyAttribute@ can be modified using AdminUpdateUserAttributes but cannot be updated using UpdateUserAttributes.
--
-- /Note:/ Consider using 'developerOnlyAttribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satDeveloperOnlyAttribute :: Lens.Lens' SchemaAttributeType (Lude.Maybe Lude.Bool)
satDeveloperOnlyAttribute = Lens.lens (developerOnlyAttribute :: SchemaAttributeType -> Lude.Maybe Lude.Bool) (\s a -> s {developerOnlyAttribute = a} :: SchemaAttributeType)
{-# DEPRECATED satDeveloperOnlyAttribute "Use generic-lens or generic-optics with 'developerOnlyAttribute' instead." #-}

-- | Specifies whether the value of the attribute can be changed.
--
-- For any user pool attribute that's mapped to an identity provider attribute, you must set this parameter to @true@ . Amazon Cognito updates mapped attributes when users sign in to your application through an identity provider. If an attribute is immutable, Amazon Cognito throws an error when it attempts to update the attribute. For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-specifying-attribute-mapping.html Specifying Identity Provider Attribute Mappings for Your User Pool> .
--
-- /Note:/ Consider using 'mutable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satMutable :: Lens.Lens' SchemaAttributeType (Lude.Maybe Lude.Bool)
satMutable = Lens.lens (mutable :: SchemaAttributeType -> Lude.Maybe Lude.Bool) (\s a -> s {mutable = a} :: SchemaAttributeType)
{-# DEPRECATED satMutable "Use generic-lens or generic-optics with 'mutable' instead." #-}

instance Lude.FromJSON SchemaAttributeType where
  parseJSON =
    Lude.withObject
      "SchemaAttributeType"
      ( \x ->
          SchemaAttributeType'
            Lude.<$> (x Lude..:? "NumberAttributeConstraints")
            Lude.<*> (x Lude..:? "Required")
            Lude.<*> (x Lude..:? "AttributeDataType")
            Lude.<*> (x Lude..:? "StringAttributeConstraints")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "DeveloperOnlyAttribute")
            Lude.<*> (x Lude..:? "Mutable")
      )

instance Lude.ToJSON SchemaAttributeType where
  toJSON SchemaAttributeType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NumberAttributeConstraints" Lude..=)
              Lude.<$> numberAttributeConstraints,
            ("Required" Lude..=) Lude.<$> required,
            ("AttributeDataType" Lude..=) Lude.<$> attributeDataType,
            ("StringAttributeConstraints" Lude..=)
              Lude.<$> stringAttributeConstraints,
            ("Name" Lude..=) Lude.<$> name,
            ("DeveloperOnlyAttribute" Lude..=) Lude.<$> developerOnlyAttribute,
            ("Mutable" Lude..=) Lude.<$> mutable
          ]
      )
