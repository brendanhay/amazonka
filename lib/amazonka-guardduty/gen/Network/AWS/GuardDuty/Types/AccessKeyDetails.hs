-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.AccessKeyDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.AccessKeyDetails
  ( AccessKeyDetails (..),

    -- * Smart constructor
    mkAccessKeyDetails,

    -- * Lenses
    akdPrincipalId,
    akdUserName,
    akdAccessKeyId,
    akdUserType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the access keys.
--
-- /See:/ 'mkAccessKeyDetails' smart constructor.
data AccessKeyDetails = AccessKeyDetails'
  { principalId ::
      Lude.Maybe Lude.Text,
    userName :: Lude.Maybe Lude.Text,
    accessKeyId :: Lude.Maybe Lude.Text,
    userType :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AccessKeyDetails' with the minimum fields required to make a request.
--
-- * 'accessKeyId' - The access key ID of the user.
-- * 'principalId' - The principal ID of the user.
-- * 'userName' - The name of the user.
-- * 'userType' - The type of the user.
mkAccessKeyDetails ::
  AccessKeyDetails
mkAccessKeyDetails =
  AccessKeyDetails'
    { principalId = Lude.Nothing,
      userName = Lude.Nothing,
      accessKeyId = Lude.Nothing,
      userType = Lude.Nothing
    }

-- | The principal ID of the user.
--
-- /Note:/ Consider using 'principalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akdPrincipalId :: Lens.Lens' AccessKeyDetails (Lude.Maybe Lude.Text)
akdPrincipalId = Lens.lens (principalId :: AccessKeyDetails -> Lude.Maybe Lude.Text) (\s a -> s {principalId = a} :: AccessKeyDetails)
{-# DEPRECATED akdPrincipalId "Use generic-lens or generic-optics with 'principalId' instead." #-}

-- | The name of the user.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akdUserName :: Lens.Lens' AccessKeyDetails (Lude.Maybe Lude.Text)
akdUserName = Lens.lens (userName :: AccessKeyDetails -> Lude.Maybe Lude.Text) (\s a -> s {userName = a} :: AccessKeyDetails)
{-# DEPRECATED akdUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The access key ID of the user.
--
-- /Note:/ Consider using 'accessKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akdAccessKeyId :: Lens.Lens' AccessKeyDetails (Lude.Maybe Lude.Text)
akdAccessKeyId = Lens.lens (accessKeyId :: AccessKeyDetails -> Lude.Maybe Lude.Text) (\s a -> s {accessKeyId = a} :: AccessKeyDetails)
{-# DEPRECATED akdAccessKeyId "Use generic-lens or generic-optics with 'accessKeyId' instead." #-}

-- | The type of the user.
--
-- /Note:/ Consider using 'userType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akdUserType :: Lens.Lens' AccessKeyDetails (Lude.Maybe Lude.Text)
akdUserType = Lens.lens (userType :: AccessKeyDetails -> Lude.Maybe Lude.Text) (\s a -> s {userType = a} :: AccessKeyDetails)
{-# DEPRECATED akdUserType "Use generic-lens or generic-optics with 'userType' instead." #-}

instance Lude.FromJSON AccessKeyDetails where
  parseJSON =
    Lude.withObject
      "AccessKeyDetails"
      ( \x ->
          AccessKeyDetails'
            Lude.<$> (x Lude..:? "principalId")
            Lude.<*> (x Lude..:? "userName")
            Lude.<*> (x Lude..:? "accessKeyId")
            Lude.<*> (x Lude..:? "userType")
      )
