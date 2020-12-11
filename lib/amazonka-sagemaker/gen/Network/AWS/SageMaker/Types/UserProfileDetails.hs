-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.UserProfileDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.UserProfileDetails
  ( UserProfileDetails (..),

    -- * Smart constructor
    mkUserProfileDetails,

    -- * Lenses
    updCreationTime,
    updStatus,
    updUserProfileName,
    updLastModifiedTime,
    updDomainId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.UserProfileStatus

-- | The user profile details.
--
-- /See:/ 'mkUserProfileDetails' smart constructor.
data UserProfileDetails = UserProfileDetails'
  { creationTime ::
      Lude.Maybe Lude.Timestamp,
    status :: Lude.Maybe UserProfileStatus,
    userProfileName :: Lude.Maybe Lude.Text,
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    domainId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UserProfileDetails' with the minimum fields required to make a request.
--
-- * 'creationTime' - The creation time.
-- * 'domainId' - The domain ID.
-- * 'lastModifiedTime' - The last modified time.
-- * 'status' - The status.
-- * 'userProfileName' - The user profile name.
mkUserProfileDetails ::
  UserProfileDetails
mkUserProfileDetails =
  UserProfileDetails'
    { creationTime = Lude.Nothing,
      status = Lude.Nothing,
      userProfileName = Lude.Nothing,
      lastModifiedTime = Lude.Nothing,
      domainId = Lude.Nothing
    }

-- | The creation time.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updCreationTime :: Lens.Lens' UserProfileDetails (Lude.Maybe Lude.Timestamp)
updCreationTime = Lens.lens (creationTime :: UserProfileDetails -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: UserProfileDetails)
{-# DEPRECATED updCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updStatus :: Lens.Lens' UserProfileDetails (Lude.Maybe UserProfileStatus)
updStatus = Lens.lens (status :: UserProfileDetails -> Lude.Maybe UserProfileStatus) (\s a -> s {status = a} :: UserProfileDetails)
{-# DEPRECATED updStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The user profile name.
--
-- /Note:/ Consider using 'userProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updUserProfileName :: Lens.Lens' UserProfileDetails (Lude.Maybe Lude.Text)
updUserProfileName = Lens.lens (userProfileName :: UserProfileDetails -> Lude.Maybe Lude.Text) (\s a -> s {userProfileName = a} :: UserProfileDetails)
{-# DEPRECATED updUserProfileName "Use generic-lens or generic-optics with 'userProfileName' instead." #-}

-- | The last modified time.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updLastModifiedTime :: Lens.Lens' UserProfileDetails (Lude.Maybe Lude.Timestamp)
updLastModifiedTime = Lens.lens (lastModifiedTime :: UserProfileDetails -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: UserProfileDetails)
{-# DEPRECATED updLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The domain ID.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updDomainId :: Lens.Lens' UserProfileDetails (Lude.Maybe Lude.Text)
updDomainId = Lens.lens (domainId :: UserProfileDetails -> Lude.Maybe Lude.Text) (\s a -> s {domainId = a} :: UserProfileDetails)
{-# DEPRECATED updDomainId "Use generic-lens or generic-optics with 'domainId' instead." #-}

instance Lude.FromJSON UserProfileDetails where
  parseJSON =
    Lude.withObject
      "UserProfileDetails"
      ( \x ->
          UserProfileDetails'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "UserProfileName")
            Lude.<*> (x Lude..:? "LastModifiedTime")
            Lude.<*> (x Lude..:? "DomainId")
      )
