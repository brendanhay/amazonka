{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.ServiceSoftwareOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.ServiceSoftwareOptions
  ( ServiceSoftwareOptions (..),

    -- * Smart constructor
    mkServiceSoftwareOptions,

    -- * Lenses
    ssoAutomatedUpdateDate,
    ssoCurrentVersion,
    ssoOptionalDeployment,
    ssoUpdateStatus,
    ssoCancellable,
    ssoUpdateAvailable,
    ssoDescription,
    ssoNewVersion,
  )
where

import Network.AWS.ElasticSearch.Types.DeploymentStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The current options of an Elasticsearch domain service software options.
--
-- /See:/ 'mkServiceSoftwareOptions' smart constructor.
data ServiceSoftwareOptions = ServiceSoftwareOptions'
  { automatedUpdateDate ::
      Lude.Maybe Lude.Timestamp,
    currentVersion :: Lude.Maybe Lude.Text,
    optionalDeployment :: Lude.Maybe Lude.Bool,
    updateStatus :: Lude.Maybe DeploymentStatus,
    cancellable :: Lude.Maybe Lude.Bool,
    updateAvailable :: Lude.Maybe Lude.Bool,
    description :: Lude.Maybe Lude.Text,
    newVersion :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServiceSoftwareOptions' with the minimum fields required to make a request.
--
-- * 'automatedUpdateDate' - Timestamp, in Epoch time, until which you can manually request a service software update. After this date, we automatically update your service software.
-- * 'cancellable' - @True@ if you are able to cancel your service software version update. @False@ if you are not able to cancel your service software version.
-- * 'currentVersion' - The current service software version that is present on the domain.
-- * 'description' - The description of the @UpdateStatus@ .
-- * 'newVersion' - The new service software version if one is available.
-- * 'optionalDeployment' - @True@ if a service software is never automatically updated. @False@ if a service software is automatically updated after @AutomatedUpdateDate@ .
-- * 'updateAvailable' - @True@ if you are able to update you service software version. @False@ if you are not able to update your service software version.
-- * 'updateStatus' - The status of your service software update. This field can take the following values: @ELIGIBLE@ , @PENDING_UPDATE@ , @IN_PROGRESS@ , @COMPLETED@ , and @NOT_ELIGIBLE@ .
mkServiceSoftwareOptions ::
  ServiceSoftwareOptions
mkServiceSoftwareOptions =
  ServiceSoftwareOptions'
    { automatedUpdateDate = Lude.Nothing,
      currentVersion = Lude.Nothing,
      optionalDeployment = Lude.Nothing,
      updateStatus = Lude.Nothing,
      cancellable = Lude.Nothing,
      updateAvailable = Lude.Nothing,
      description = Lude.Nothing,
      newVersion = Lude.Nothing
    }

-- | Timestamp, in Epoch time, until which you can manually request a service software update. After this date, we automatically update your service software.
--
-- /Note:/ Consider using 'automatedUpdateDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssoAutomatedUpdateDate :: Lens.Lens' ServiceSoftwareOptions (Lude.Maybe Lude.Timestamp)
ssoAutomatedUpdateDate = Lens.lens (automatedUpdateDate :: ServiceSoftwareOptions -> Lude.Maybe Lude.Timestamp) (\s a -> s {automatedUpdateDate = a} :: ServiceSoftwareOptions)
{-# DEPRECATED ssoAutomatedUpdateDate "Use generic-lens or generic-optics with 'automatedUpdateDate' instead." #-}

-- | The current service software version that is present on the domain.
--
-- /Note:/ Consider using 'currentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssoCurrentVersion :: Lens.Lens' ServiceSoftwareOptions (Lude.Maybe Lude.Text)
ssoCurrentVersion = Lens.lens (currentVersion :: ServiceSoftwareOptions -> Lude.Maybe Lude.Text) (\s a -> s {currentVersion = a} :: ServiceSoftwareOptions)
{-# DEPRECATED ssoCurrentVersion "Use generic-lens or generic-optics with 'currentVersion' instead." #-}

-- | @True@ if a service software is never automatically updated. @False@ if a service software is automatically updated after @AutomatedUpdateDate@ .
--
-- /Note:/ Consider using 'optionalDeployment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssoOptionalDeployment :: Lens.Lens' ServiceSoftwareOptions (Lude.Maybe Lude.Bool)
ssoOptionalDeployment = Lens.lens (optionalDeployment :: ServiceSoftwareOptions -> Lude.Maybe Lude.Bool) (\s a -> s {optionalDeployment = a} :: ServiceSoftwareOptions)
{-# DEPRECATED ssoOptionalDeployment "Use generic-lens or generic-optics with 'optionalDeployment' instead." #-}

-- | The status of your service software update. This field can take the following values: @ELIGIBLE@ , @PENDING_UPDATE@ , @IN_PROGRESS@ , @COMPLETED@ , and @NOT_ELIGIBLE@ .
--
-- /Note:/ Consider using 'updateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssoUpdateStatus :: Lens.Lens' ServiceSoftwareOptions (Lude.Maybe DeploymentStatus)
ssoUpdateStatus = Lens.lens (updateStatus :: ServiceSoftwareOptions -> Lude.Maybe DeploymentStatus) (\s a -> s {updateStatus = a} :: ServiceSoftwareOptions)
{-# DEPRECATED ssoUpdateStatus "Use generic-lens or generic-optics with 'updateStatus' instead." #-}

-- | @True@ if you are able to cancel your service software version update. @False@ if you are not able to cancel your service software version.
--
-- /Note:/ Consider using 'cancellable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssoCancellable :: Lens.Lens' ServiceSoftwareOptions (Lude.Maybe Lude.Bool)
ssoCancellable = Lens.lens (cancellable :: ServiceSoftwareOptions -> Lude.Maybe Lude.Bool) (\s a -> s {cancellable = a} :: ServiceSoftwareOptions)
{-# DEPRECATED ssoCancellable "Use generic-lens or generic-optics with 'cancellable' instead." #-}

-- | @True@ if you are able to update you service software version. @False@ if you are not able to update your service software version.
--
-- /Note:/ Consider using 'updateAvailable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssoUpdateAvailable :: Lens.Lens' ServiceSoftwareOptions (Lude.Maybe Lude.Bool)
ssoUpdateAvailable = Lens.lens (updateAvailable :: ServiceSoftwareOptions -> Lude.Maybe Lude.Bool) (\s a -> s {updateAvailable = a} :: ServiceSoftwareOptions)
{-# DEPRECATED ssoUpdateAvailable "Use generic-lens or generic-optics with 'updateAvailable' instead." #-}

-- | The description of the @UpdateStatus@ .
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssoDescription :: Lens.Lens' ServiceSoftwareOptions (Lude.Maybe Lude.Text)
ssoDescription = Lens.lens (description :: ServiceSoftwareOptions -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ServiceSoftwareOptions)
{-# DEPRECATED ssoDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The new service software version if one is available.
--
-- /Note:/ Consider using 'newVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssoNewVersion :: Lens.Lens' ServiceSoftwareOptions (Lude.Maybe Lude.Text)
ssoNewVersion = Lens.lens (newVersion :: ServiceSoftwareOptions -> Lude.Maybe Lude.Text) (\s a -> s {newVersion = a} :: ServiceSoftwareOptions)
{-# DEPRECATED ssoNewVersion "Use generic-lens or generic-optics with 'newVersion' instead." #-}

instance Lude.FromJSON ServiceSoftwareOptions where
  parseJSON =
    Lude.withObject
      "ServiceSoftwareOptions"
      ( \x ->
          ServiceSoftwareOptions'
            Lude.<$> (x Lude..:? "AutomatedUpdateDate")
            Lude.<*> (x Lude..:? "CurrentVersion")
            Lude.<*> (x Lude..:? "OptionalDeployment")
            Lude.<*> (x Lude..:? "UpdateStatus")
            Lude.<*> (x Lude..:? "Cancellable")
            Lude.<*> (x Lude..:? "UpdateAvailable")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "NewVersion")
      )
