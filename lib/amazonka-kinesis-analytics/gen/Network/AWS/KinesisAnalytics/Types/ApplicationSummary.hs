-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.ApplicationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.ApplicationSummary
  ( ApplicationSummary (..),

    -- * Smart constructor
    mkApplicationSummary,

    -- * Lenses
    asApplicationName,
    asApplicationARN,
    asApplicationStatus,
  )
where

import Network.AWS.KinesisAnalytics.Types.ApplicationStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides application summary information, including the application Amazon Resource Name (ARN), name, and status.
--
-- /See:/ 'mkApplicationSummary' smart constructor.
data ApplicationSummary = ApplicationSummary'
  { applicationName ::
      Lude.Text,
    applicationARN :: Lude.Text,
    applicationStatus :: ApplicationStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ApplicationSummary' with the minimum fields required to make a request.
--
-- * 'applicationARN' - ARN of the application.
-- * 'applicationName' - Name of the application.
-- * 'applicationStatus' - Status of the application.
mkApplicationSummary ::
  -- | 'applicationName'
  Lude.Text ->
  -- | 'applicationARN'
  Lude.Text ->
  -- | 'applicationStatus'
  ApplicationStatus ->
  ApplicationSummary
mkApplicationSummary
  pApplicationName_
  pApplicationARN_
  pApplicationStatus_ =
    ApplicationSummary'
      { applicationName = pApplicationName_,
        applicationARN = pApplicationARN_,
        applicationStatus = pApplicationStatus_
      }

-- | Name of the application.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asApplicationName :: Lens.Lens' ApplicationSummary Lude.Text
asApplicationName = Lens.lens (applicationName :: ApplicationSummary -> Lude.Text) (\s a -> s {applicationName = a} :: ApplicationSummary)
{-# DEPRECATED asApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | ARN of the application.
--
-- /Note:/ Consider using 'applicationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asApplicationARN :: Lens.Lens' ApplicationSummary Lude.Text
asApplicationARN = Lens.lens (applicationARN :: ApplicationSummary -> Lude.Text) (\s a -> s {applicationARN = a} :: ApplicationSummary)
{-# DEPRECATED asApplicationARN "Use generic-lens or generic-optics with 'applicationARN' instead." #-}

-- | Status of the application.
--
-- /Note:/ Consider using 'applicationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asApplicationStatus :: Lens.Lens' ApplicationSummary ApplicationStatus
asApplicationStatus = Lens.lens (applicationStatus :: ApplicationSummary -> ApplicationStatus) (\s a -> s {applicationStatus = a} :: ApplicationSummary)
{-# DEPRECATED asApplicationStatus "Use generic-lens or generic-optics with 'applicationStatus' instead." #-}

instance Lude.FromJSON ApplicationSummary where
  parseJSON =
    Lude.withObject
      "ApplicationSummary"
      ( \x ->
          ApplicationSummary'
            Lude.<$> (x Lude..: "ApplicationName")
            Lude.<*> (x Lude..: "ApplicationARN")
            Lude.<*> (x Lude..: "ApplicationStatus")
      )
