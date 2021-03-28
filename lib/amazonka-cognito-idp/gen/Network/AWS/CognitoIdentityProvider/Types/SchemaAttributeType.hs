{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.SchemaAttributeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.SchemaAttributeType
  ( SchemaAttributeType (..)
  -- * Smart constructor
  , mkSchemaAttributeType
  -- * Lenses
  , satAttributeDataType
  , satDeveloperOnlyAttribute
  , satMutable
  , satName
  , satNumberAttributeConstraints
  , satRequired
  , satStringAttributeConstraints
  ) where

import qualified Network.AWS.CognitoIdentityProvider.Types.AttributeDataType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.Name as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.NumberAttributeConstraintsType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.StringAttributeConstraintsType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the schema attribute.
--
-- /See:/ 'mkSchemaAttributeType' smart constructor.
data SchemaAttributeType = SchemaAttributeType'
  { attributeDataType :: Core.Maybe Types.AttributeDataType
    -- ^ The attribute data type.
  , developerOnlyAttribute :: Core.Maybe Core.Bool
    -- ^ Specifies whether the attribute type is developer only. This attribute can only be modified by an administrator. Users will not be able to modify this attribute using their access token. For example, @DeveloperOnlyAttribute@ can be modified using AdminUpdateUserAttributes but cannot be updated using UpdateUserAttributes.
  , mutable :: Core.Maybe Core.Bool
    -- ^ Specifies whether the value of the attribute can be changed.
--
-- For any user pool attribute that's mapped to an identity provider attribute, you must set this parameter to @true@ . Amazon Cognito updates mapped attributes when users sign in to your application through an identity provider. If an attribute is immutable, Amazon Cognito throws an error when it attempts to update the attribute. For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-specifying-attribute-mapping.html Specifying Identity Provider Attribute Mappings for Your User Pool> .
  , name :: Core.Maybe Types.Name
    -- ^ A schema attribute of the name type.
  , numberAttributeConstraints :: Core.Maybe Types.NumberAttributeConstraintsType
    -- ^ Specifies the constraints for an attribute of the number type.
  , required :: Core.Maybe Core.Bool
    -- ^ Specifies whether a user pool attribute is required. If the attribute is required and the user does not provide a value, registration or sign-in will fail.
  , stringAttributeConstraints :: Core.Maybe Types.StringAttributeConstraintsType
    -- ^ Specifies the constraints for an attribute of the string type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SchemaAttributeType' value with any optional fields omitted.
mkSchemaAttributeType
    :: SchemaAttributeType
mkSchemaAttributeType
  = SchemaAttributeType'{attributeDataType = Core.Nothing,
                         developerOnlyAttribute = Core.Nothing, mutable = Core.Nothing,
                         name = Core.Nothing, numberAttributeConstraints = Core.Nothing,
                         required = Core.Nothing, stringAttributeConstraints = Core.Nothing}

-- | The attribute data type.
--
-- /Note:/ Consider using 'attributeDataType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satAttributeDataType :: Lens.Lens' SchemaAttributeType (Core.Maybe Types.AttributeDataType)
satAttributeDataType = Lens.field @"attributeDataType"
{-# INLINEABLE satAttributeDataType #-}
{-# DEPRECATED attributeDataType "Use generic-lens or generic-optics with 'attributeDataType' instead"  #-}

-- | Specifies whether the attribute type is developer only. This attribute can only be modified by an administrator. Users will not be able to modify this attribute using their access token. For example, @DeveloperOnlyAttribute@ can be modified using AdminUpdateUserAttributes but cannot be updated using UpdateUserAttributes.
--
-- /Note:/ Consider using 'developerOnlyAttribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satDeveloperOnlyAttribute :: Lens.Lens' SchemaAttributeType (Core.Maybe Core.Bool)
satDeveloperOnlyAttribute = Lens.field @"developerOnlyAttribute"
{-# INLINEABLE satDeveloperOnlyAttribute #-}
{-# DEPRECATED developerOnlyAttribute "Use generic-lens or generic-optics with 'developerOnlyAttribute' instead"  #-}

-- | Specifies whether the value of the attribute can be changed.
--
-- For any user pool attribute that's mapped to an identity provider attribute, you must set this parameter to @true@ . Amazon Cognito updates mapped attributes when users sign in to your application through an identity provider. If an attribute is immutable, Amazon Cognito throws an error when it attempts to update the attribute. For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-specifying-attribute-mapping.html Specifying Identity Provider Attribute Mappings for Your User Pool> .
--
-- /Note:/ Consider using 'mutable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satMutable :: Lens.Lens' SchemaAttributeType (Core.Maybe Core.Bool)
satMutable = Lens.field @"mutable"
{-# INLINEABLE satMutable #-}
{-# DEPRECATED mutable "Use generic-lens or generic-optics with 'mutable' instead"  #-}

-- | A schema attribute of the name type.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satName :: Lens.Lens' SchemaAttributeType (Core.Maybe Types.Name)
satName = Lens.field @"name"
{-# INLINEABLE satName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Specifies the constraints for an attribute of the number type.
--
-- /Note:/ Consider using 'numberAttributeConstraints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satNumberAttributeConstraints :: Lens.Lens' SchemaAttributeType (Core.Maybe Types.NumberAttributeConstraintsType)
satNumberAttributeConstraints = Lens.field @"numberAttributeConstraints"
{-# INLINEABLE satNumberAttributeConstraints #-}
{-# DEPRECATED numberAttributeConstraints "Use generic-lens or generic-optics with 'numberAttributeConstraints' instead"  #-}

-- | Specifies whether a user pool attribute is required. If the attribute is required and the user does not provide a value, registration or sign-in will fail.
--
-- /Note:/ Consider using 'required' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satRequired :: Lens.Lens' SchemaAttributeType (Core.Maybe Core.Bool)
satRequired = Lens.field @"required"
{-# INLINEABLE satRequired #-}
{-# DEPRECATED required "Use generic-lens or generic-optics with 'required' instead"  #-}

-- | Specifies the constraints for an attribute of the string type.
--
-- /Note:/ Consider using 'stringAttributeConstraints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satStringAttributeConstraints :: Lens.Lens' SchemaAttributeType (Core.Maybe Types.StringAttributeConstraintsType)
satStringAttributeConstraints = Lens.field @"stringAttributeConstraints"
{-# INLINEABLE satStringAttributeConstraints #-}
{-# DEPRECATED stringAttributeConstraints "Use generic-lens or generic-optics with 'stringAttributeConstraints' instead"  #-}

instance Core.FromJSON SchemaAttributeType where
        toJSON SchemaAttributeType{..}
          = Core.object
              (Core.catMaybes
                 [("AttributeDataType" Core..=) Core.<$> attributeDataType,
                  ("DeveloperOnlyAttribute" Core..=) Core.<$> developerOnlyAttribute,
                  ("Mutable" Core..=) Core.<$> mutable,
                  ("Name" Core..=) Core.<$> name,
                  ("NumberAttributeConstraints" Core..=) Core.<$>
                    numberAttributeConstraints,
                  ("Required" Core..=) Core.<$> required,
                  ("StringAttributeConstraints" Core..=) Core.<$>
                    stringAttributeConstraints])

instance Core.FromJSON SchemaAttributeType where
        parseJSON
          = Core.withObject "SchemaAttributeType" Core.$
              \ x ->
                SchemaAttributeType' Core.<$>
                  (x Core..:? "AttributeDataType") Core.<*>
                    x Core..:? "DeveloperOnlyAttribute"
                    Core.<*> x Core..:? "Mutable"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "NumberAttributeConstraints"
                    Core.<*> x Core..:? "Required"
                    Core.<*> x Core..:? "StringAttributeConstraints"
