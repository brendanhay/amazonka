{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserPoolDescriptionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.UserPoolDescriptionType
  ( UserPoolDescriptionType (..),

    -- * Smart constructor
    mkUserPoolDescriptionType,

    -- * Lenses
    updtCreationDate,
    updtId,
    updtLambdaConfig,
    updtLastModifiedDate,
    updtName,
    updtStatus,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types.LambdaConfigType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.StatusType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.UserPoolIdType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.UserPoolNameType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A user pool description.
--
-- /See:/ 'mkUserPoolDescriptionType' smart constructor.
data UserPoolDescriptionType = UserPoolDescriptionType'
  { -- | The date the user pool description was created.
    creationDate :: Core.Maybe Core.NominalDiffTime,
    -- | The ID in a user pool description.
    id :: Core.Maybe Types.UserPoolIdType,
    -- | The AWS Lambda configuration information in a user pool description.
    lambdaConfig :: Core.Maybe Types.LambdaConfigType,
    -- | The date the user pool description was last modified.
    lastModifiedDate :: Core.Maybe Core.NominalDiffTime,
    -- | The name in a user pool description.
    name :: Core.Maybe Types.UserPoolNameType,
    -- | The user pool status in a user pool description.
    status :: Core.Maybe Types.StatusType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UserPoolDescriptionType' value with any optional fields omitted.
mkUserPoolDescriptionType ::
  UserPoolDescriptionType
mkUserPoolDescriptionType =
  UserPoolDescriptionType'
    { creationDate = Core.Nothing,
      id = Core.Nothing,
      lambdaConfig = Core.Nothing,
      lastModifiedDate = Core.Nothing,
      name = Core.Nothing,
      status = Core.Nothing
    }

-- | The date the user pool description was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updtCreationDate :: Lens.Lens' UserPoolDescriptionType (Core.Maybe Core.NominalDiffTime)
updtCreationDate = Lens.field @"creationDate"
{-# DEPRECATED updtCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The ID in a user pool description.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updtId :: Lens.Lens' UserPoolDescriptionType (Core.Maybe Types.UserPoolIdType)
updtId = Lens.field @"id"
{-# DEPRECATED updtId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The AWS Lambda configuration information in a user pool description.
--
-- /Note:/ Consider using 'lambdaConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updtLambdaConfig :: Lens.Lens' UserPoolDescriptionType (Core.Maybe Types.LambdaConfigType)
updtLambdaConfig = Lens.field @"lambdaConfig"
{-# DEPRECATED updtLambdaConfig "Use generic-lens or generic-optics with 'lambdaConfig' instead." #-}

-- | The date the user pool description was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updtLastModifiedDate :: Lens.Lens' UserPoolDescriptionType (Core.Maybe Core.NominalDiffTime)
updtLastModifiedDate = Lens.field @"lastModifiedDate"
{-# DEPRECATED updtLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The name in a user pool description.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updtName :: Lens.Lens' UserPoolDescriptionType (Core.Maybe Types.UserPoolNameType)
updtName = Lens.field @"name"
{-# DEPRECATED updtName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The user pool status in a user pool description.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updtStatus :: Lens.Lens' UserPoolDescriptionType (Core.Maybe Types.StatusType)
updtStatus = Lens.field @"status"
{-# DEPRECATED updtStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON UserPoolDescriptionType where
  parseJSON =
    Core.withObject "UserPoolDescriptionType" Core.$
      \x ->
        UserPoolDescriptionType'
          Core.<$> (x Core..:? "CreationDate")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "LambdaConfig")
          Core.<*> (x Core..:? "LastModifiedDate")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "Status")
