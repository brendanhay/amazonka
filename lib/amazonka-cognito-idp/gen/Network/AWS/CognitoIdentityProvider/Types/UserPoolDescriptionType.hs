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
    updtStatus,
    updtLastModifiedDate,
    updtName,
    updtId,
    updtCreationDate,
    updtLambdaConfig,
  )
where

import Network.AWS.CognitoIdentityProvider.Types.LambdaConfigType
import Network.AWS.CognitoIdentityProvider.Types.StatusType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A user pool description.
--
-- /See:/ 'mkUserPoolDescriptionType' smart constructor.
data UserPoolDescriptionType = UserPoolDescriptionType'
  { status ::
      Lude.Maybe StatusType,
    lastModifiedDate ::
      Lude.Maybe Lude.Timestamp,
    name :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    creationDate :: Lude.Maybe Lude.Timestamp,
    lambdaConfig :: Lude.Maybe LambdaConfigType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UserPoolDescriptionType' with the minimum fields required to make a request.
--
-- * 'creationDate' - The date the user pool description was created.
-- * 'id' - The ID in a user pool description.
-- * 'lambdaConfig' - The AWS Lambda configuration information in a user pool description.
-- * 'lastModifiedDate' - The date the user pool description was last modified.
-- * 'name' - The name in a user pool description.
-- * 'status' - The user pool status in a user pool description.
mkUserPoolDescriptionType ::
  UserPoolDescriptionType
mkUserPoolDescriptionType =
  UserPoolDescriptionType'
    { status = Lude.Nothing,
      lastModifiedDate = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      creationDate = Lude.Nothing,
      lambdaConfig = Lude.Nothing
    }

-- | The user pool status in a user pool description.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updtStatus :: Lens.Lens' UserPoolDescriptionType (Lude.Maybe StatusType)
updtStatus = Lens.lens (status :: UserPoolDescriptionType -> Lude.Maybe StatusType) (\s a -> s {status = a} :: UserPoolDescriptionType)
{-# DEPRECATED updtStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The date the user pool description was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updtLastModifiedDate :: Lens.Lens' UserPoolDescriptionType (Lude.Maybe Lude.Timestamp)
updtLastModifiedDate = Lens.lens (lastModifiedDate :: UserPoolDescriptionType -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedDate = a} :: UserPoolDescriptionType)
{-# DEPRECATED updtLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The name in a user pool description.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updtName :: Lens.Lens' UserPoolDescriptionType (Lude.Maybe Lude.Text)
updtName = Lens.lens (name :: UserPoolDescriptionType -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UserPoolDescriptionType)
{-# DEPRECATED updtName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID in a user pool description.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updtId :: Lens.Lens' UserPoolDescriptionType (Lude.Maybe Lude.Text)
updtId = Lens.lens (id :: UserPoolDescriptionType -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: UserPoolDescriptionType)
{-# DEPRECATED updtId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The date the user pool description was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updtCreationDate :: Lens.Lens' UserPoolDescriptionType (Lude.Maybe Lude.Timestamp)
updtCreationDate = Lens.lens (creationDate :: UserPoolDescriptionType -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: UserPoolDescriptionType)
{-# DEPRECATED updtCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The AWS Lambda configuration information in a user pool description.
--
-- /Note:/ Consider using 'lambdaConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updtLambdaConfig :: Lens.Lens' UserPoolDescriptionType (Lude.Maybe LambdaConfigType)
updtLambdaConfig = Lens.lens (lambdaConfig :: UserPoolDescriptionType -> Lude.Maybe LambdaConfigType) (\s a -> s {lambdaConfig = a} :: UserPoolDescriptionType)
{-# DEPRECATED updtLambdaConfig "Use generic-lens or generic-optics with 'lambdaConfig' instead." #-}

instance Lude.FromJSON UserPoolDescriptionType where
  parseJSON =
    Lude.withObject
      "UserPoolDescriptionType"
      ( \x ->
          UserPoolDescriptionType'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "LastModifiedDate")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "CreationDate")
            Lude.<*> (x Lude..:? "LambdaConfig")
      )
