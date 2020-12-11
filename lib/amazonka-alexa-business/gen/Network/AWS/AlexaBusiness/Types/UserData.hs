-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.UserData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.UserData
  ( UserData (..),

    -- * Smart constructor
    mkUserData,

    -- * Lenses
    udEmail,
    udLastName,
    udEnrollmentId,
    udUserARN,
    udFirstName,
    udEnrollmentStatus,
  )
where

import Network.AWS.AlexaBusiness.Types.EnrollmentStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information related to a user.
--
-- /See:/ 'mkUserData' smart constructor.
data UserData = UserData'
  { email :: Lude.Maybe Lude.Text,
    lastName :: Lude.Maybe Lude.Text,
    enrollmentId :: Lude.Maybe Lude.Text,
    userARN :: Lude.Maybe Lude.Text,
    firstName :: Lude.Maybe Lude.Text,
    enrollmentStatus :: Lude.Maybe EnrollmentStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UserData' with the minimum fields required to make a request.
--
-- * 'email' - The email of a user.
-- * 'enrollmentId' - The enrollment ARN of a user.
-- * 'enrollmentStatus' - The enrollment status of a user.
-- * 'firstName' - The first name of a user.
-- * 'lastName' - The last name of a user.
-- * 'userARN' - The ARN of a user.
mkUserData ::
  UserData
mkUserData =
  UserData'
    { email = Lude.Nothing,
      lastName = Lude.Nothing,
      enrollmentId = Lude.Nothing,
      userARN = Lude.Nothing,
      firstName = Lude.Nothing,
      enrollmentStatus = Lude.Nothing
    }

-- | The email of a user.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udEmail :: Lens.Lens' UserData (Lude.Maybe Lude.Text)
udEmail = Lens.lens (email :: UserData -> Lude.Maybe Lude.Text) (\s a -> s {email = a} :: UserData)
{-# DEPRECATED udEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | The last name of a user.
--
-- /Note:/ Consider using 'lastName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udLastName :: Lens.Lens' UserData (Lude.Maybe Lude.Text)
udLastName = Lens.lens (lastName :: UserData -> Lude.Maybe Lude.Text) (\s a -> s {lastName = a} :: UserData)
{-# DEPRECATED udLastName "Use generic-lens or generic-optics with 'lastName' instead." #-}

-- | The enrollment ARN of a user.
--
-- /Note:/ Consider using 'enrollmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udEnrollmentId :: Lens.Lens' UserData (Lude.Maybe Lude.Text)
udEnrollmentId = Lens.lens (enrollmentId :: UserData -> Lude.Maybe Lude.Text) (\s a -> s {enrollmentId = a} :: UserData)
{-# DEPRECATED udEnrollmentId "Use generic-lens or generic-optics with 'enrollmentId' instead." #-}

-- | The ARN of a user.
--
-- /Note:/ Consider using 'userARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udUserARN :: Lens.Lens' UserData (Lude.Maybe Lude.Text)
udUserARN = Lens.lens (userARN :: UserData -> Lude.Maybe Lude.Text) (\s a -> s {userARN = a} :: UserData)
{-# DEPRECATED udUserARN "Use generic-lens or generic-optics with 'userARN' instead." #-}

-- | The first name of a user.
--
-- /Note:/ Consider using 'firstName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udFirstName :: Lens.Lens' UserData (Lude.Maybe Lude.Text)
udFirstName = Lens.lens (firstName :: UserData -> Lude.Maybe Lude.Text) (\s a -> s {firstName = a} :: UserData)
{-# DEPRECATED udFirstName "Use generic-lens or generic-optics with 'firstName' instead." #-}

-- | The enrollment status of a user.
--
-- /Note:/ Consider using 'enrollmentStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udEnrollmentStatus :: Lens.Lens' UserData (Lude.Maybe EnrollmentStatus)
udEnrollmentStatus = Lens.lens (enrollmentStatus :: UserData -> Lude.Maybe EnrollmentStatus) (\s a -> s {enrollmentStatus = a} :: UserData)
{-# DEPRECATED udEnrollmentStatus "Use generic-lens or generic-optics with 'enrollmentStatus' instead." #-}

instance Lude.FromJSON UserData where
  parseJSON =
    Lude.withObject
      "UserData"
      ( \x ->
          UserData'
            Lude.<$> (x Lude..:? "Email")
            Lude.<*> (x Lude..:? "LastName")
            Lude.<*> (x Lude..:? "EnrollmentId")
            Lude.<*> (x Lude..:? "UserArn")
            Lude.<*> (x Lude..:? "FirstName")
            Lude.<*> (x Lude..:? "EnrollmentStatus")
      )
