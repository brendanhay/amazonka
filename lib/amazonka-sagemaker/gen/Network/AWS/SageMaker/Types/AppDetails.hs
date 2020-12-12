{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AppDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AppDetails
  ( AppDetails (..),

    -- * Smart constructor
    mkAppDetails,

    -- * Lenses
    adCreationTime,
    adStatus,
    adUserProfileName,
    adAppName,
    adDomainId,
    adAppType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.AppStatus
import Network.AWS.SageMaker.Types.AppType

-- | Details about an Amazon SageMaker app.
--
-- /See:/ 'mkAppDetails' smart constructor.
data AppDetails = AppDetails'
  { creationTime ::
      Lude.Maybe Lude.Timestamp,
    status :: Lude.Maybe AppStatus,
    userProfileName :: Lude.Maybe Lude.Text,
    appName :: Lude.Maybe Lude.Text,
    domainId :: Lude.Maybe Lude.Text,
    appType :: Lude.Maybe AppType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AppDetails' with the minimum fields required to make a request.
--
-- * 'appName' - The name of the app.
-- * 'appType' - The type of app.
-- * 'creationTime' - The creation time.
-- * 'domainId' - The domain ID.
-- * 'status' - The status.
-- * 'userProfileName' - The user profile name.
mkAppDetails ::
  AppDetails
mkAppDetails =
  AppDetails'
    { creationTime = Lude.Nothing,
      status = Lude.Nothing,
      userProfileName = Lude.Nothing,
      appName = Lude.Nothing,
      domainId = Lude.Nothing,
      appType = Lude.Nothing
    }

-- | The creation time.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adCreationTime :: Lens.Lens' AppDetails (Lude.Maybe Lude.Timestamp)
adCreationTime = Lens.lens (creationTime :: AppDetails -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: AppDetails)
{-# DEPRECATED adCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adStatus :: Lens.Lens' AppDetails (Lude.Maybe AppStatus)
adStatus = Lens.lens (status :: AppDetails -> Lude.Maybe AppStatus) (\s a -> s {status = a} :: AppDetails)
{-# DEPRECATED adStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The user profile name.
--
-- /Note:/ Consider using 'userProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adUserProfileName :: Lens.Lens' AppDetails (Lude.Maybe Lude.Text)
adUserProfileName = Lens.lens (userProfileName :: AppDetails -> Lude.Maybe Lude.Text) (\s a -> s {userProfileName = a} :: AppDetails)
{-# DEPRECATED adUserProfileName "Use generic-lens or generic-optics with 'userProfileName' instead." #-}

-- | The name of the app.
--
-- /Note:/ Consider using 'appName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAppName :: Lens.Lens' AppDetails (Lude.Maybe Lude.Text)
adAppName = Lens.lens (appName :: AppDetails -> Lude.Maybe Lude.Text) (\s a -> s {appName = a} :: AppDetails)
{-# DEPRECATED adAppName "Use generic-lens or generic-optics with 'appName' instead." #-}

-- | The domain ID.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adDomainId :: Lens.Lens' AppDetails (Lude.Maybe Lude.Text)
adDomainId = Lens.lens (domainId :: AppDetails -> Lude.Maybe Lude.Text) (\s a -> s {domainId = a} :: AppDetails)
{-# DEPRECATED adDomainId "Use generic-lens or generic-optics with 'domainId' instead." #-}

-- | The type of app.
--
-- /Note:/ Consider using 'appType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAppType :: Lens.Lens' AppDetails (Lude.Maybe AppType)
adAppType = Lens.lens (appType :: AppDetails -> Lude.Maybe AppType) (\s a -> s {appType = a} :: AppDetails)
{-# DEPRECATED adAppType "Use generic-lens or generic-optics with 'appType' instead." #-}

instance Lude.FromJSON AppDetails where
  parseJSON =
    Lude.withObject
      "AppDetails"
      ( \x ->
          AppDetails'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "UserProfileName")
            Lude.<*> (x Lude..:? "AppName")
            Lude.<*> (x Lude..:? "DomainId")
            Lude.<*> (x Lude..:? "AppType")
      )
