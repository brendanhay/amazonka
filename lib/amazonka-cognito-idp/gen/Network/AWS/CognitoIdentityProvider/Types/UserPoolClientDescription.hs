-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserPoolClientDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.UserPoolClientDescription
  ( UserPoolClientDescription (..),

    -- * Smart constructor
    mkUserPoolClientDescription,

    -- * Lenses
    upcdClientId,
    upcdUserPoolId,
    upcdClientName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The description of the user pool client.
--
-- /See:/ 'mkUserPoolClientDescription' smart constructor.
data UserPoolClientDescription = UserPoolClientDescription'
  { clientId ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    userPoolId :: Lude.Maybe Lude.Text,
    clientName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UserPoolClientDescription' with the minimum fields required to make a request.
--
-- * 'clientId' - The ID of the client associated with the user pool.
-- * 'clientName' - The client name from the user pool client description.
-- * 'userPoolId' - The user pool ID for the user pool where you want to describe the user pool client.
mkUserPoolClientDescription ::
  UserPoolClientDescription
mkUserPoolClientDescription =
  UserPoolClientDescription'
    { clientId = Lude.Nothing,
      userPoolId = Lude.Nothing,
      clientName = Lude.Nothing
    }

-- | The ID of the client associated with the user pool.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcdClientId :: Lens.Lens' UserPoolClientDescription (Lude.Maybe (Lude.Sensitive Lude.Text))
upcdClientId = Lens.lens (clientId :: UserPoolClientDescription -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {clientId = a} :: UserPoolClientDescription)
{-# DEPRECATED upcdClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | The user pool ID for the user pool where you want to describe the user pool client.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcdUserPoolId :: Lens.Lens' UserPoolClientDescription (Lude.Maybe Lude.Text)
upcdUserPoolId = Lens.lens (userPoolId :: UserPoolClientDescription -> Lude.Maybe Lude.Text) (\s a -> s {userPoolId = a} :: UserPoolClientDescription)
{-# DEPRECATED upcdUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The client name from the user pool client description.
--
-- /Note:/ Consider using 'clientName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcdClientName :: Lens.Lens' UserPoolClientDescription (Lude.Maybe Lude.Text)
upcdClientName = Lens.lens (clientName :: UserPoolClientDescription -> Lude.Maybe Lude.Text) (\s a -> s {clientName = a} :: UserPoolClientDescription)
{-# DEPRECATED upcdClientName "Use generic-lens or generic-optics with 'clientName' instead." #-}

instance Lude.FromJSON UserPoolClientDescription where
  parseJSON =
    Lude.withObject
      "UserPoolClientDescription"
      ( \x ->
          UserPoolClientDescription'
            Lude.<$> (x Lude..:? "ClientId")
            Lude.<*> (x Lude..:? "UserPoolId")
            Lude.<*> (x Lude..:? "ClientName")
      )
