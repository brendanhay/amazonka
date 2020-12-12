{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.Application
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.Application
  ( Application (..),

    -- * Smart constructor
    mkApplication,

    -- * Lenses
    appEnabled,
    appLaunchPath,
    appLaunchParameters,
    appName,
    appDisplayName,
    appMetadata,
    appIconURL,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an application in the application catalog.
--
-- /See:/ 'mkApplication' smart constructor.
data Application = Application'
  { enabled :: Lude.Maybe Lude.Bool,
    launchPath :: Lude.Maybe Lude.Text,
    launchParameters :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    displayName :: Lude.Maybe Lude.Text,
    metadata :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    iconURL :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Application' with the minimum fields required to make a request.
--
-- * 'displayName' - The application name to display.
-- * 'enabled' - If there is a problem, the application can be disabled after image creation.
-- * 'iconURL' - The URL for the application icon. This URL might be time-limited.
-- * 'launchParameters' - The arguments that are passed to the application at launch.
-- * 'launchPath' - The path to the application executable in the instance.
-- * 'metadata' - Additional attributes that describe the application.
-- * 'name' - The name of the application.
mkApplication ::
  Application
mkApplication =
  Application'
    { enabled = Lude.Nothing,
      launchPath = Lude.Nothing,
      launchParameters = Lude.Nothing,
      name = Lude.Nothing,
      displayName = Lude.Nothing,
      metadata = Lude.Nothing,
      iconURL = Lude.Nothing
    }

-- | If there is a problem, the application can be disabled after image creation.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
appEnabled :: Lens.Lens' Application (Lude.Maybe Lude.Bool)
appEnabled = Lens.lens (enabled :: Application -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: Application)
{-# DEPRECATED appEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The path to the application executable in the instance.
--
-- /Note:/ Consider using 'launchPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
appLaunchPath :: Lens.Lens' Application (Lude.Maybe Lude.Text)
appLaunchPath = Lens.lens (launchPath :: Application -> Lude.Maybe Lude.Text) (\s a -> s {launchPath = a} :: Application)
{-# DEPRECATED appLaunchPath "Use generic-lens or generic-optics with 'launchPath' instead." #-}

-- | The arguments that are passed to the application at launch.
--
-- /Note:/ Consider using 'launchParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
appLaunchParameters :: Lens.Lens' Application (Lude.Maybe Lude.Text)
appLaunchParameters = Lens.lens (launchParameters :: Application -> Lude.Maybe Lude.Text) (\s a -> s {launchParameters = a} :: Application)
{-# DEPRECATED appLaunchParameters "Use generic-lens or generic-optics with 'launchParameters' instead." #-}

-- | The name of the application.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
appName :: Lens.Lens' Application (Lude.Maybe Lude.Text)
appName = Lens.lens (name :: Application -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Application)
{-# DEPRECATED appName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The application name to display.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
appDisplayName :: Lens.Lens' Application (Lude.Maybe Lude.Text)
appDisplayName = Lens.lens (displayName :: Application -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: Application)
{-# DEPRECATED appDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | Additional attributes that describe the application.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
appMetadata :: Lens.Lens' Application (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
appMetadata = Lens.lens (metadata :: Application -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {metadata = a} :: Application)
{-# DEPRECATED appMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | The URL for the application icon. This URL might be time-limited.
--
-- /Note:/ Consider using 'iconURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
appIconURL :: Lens.Lens' Application (Lude.Maybe Lude.Text)
appIconURL = Lens.lens (iconURL :: Application -> Lude.Maybe Lude.Text) (\s a -> s {iconURL = a} :: Application)
{-# DEPRECATED appIconURL "Use generic-lens or generic-optics with 'iconURL' instead." #-}

instance Lude.FromJSON Application where
  parseJSON =
    Lude.withObject
      "Application"
      ( \x ->
          Application'
            Lude.<$> (x Lude..:? "Enabled")
            Lude.<*> (x Lude..:? "LaunchPath")
            Lude.<*> (x Lude..:? "LaunchParameters")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "DisplayName")
            Lude.<*> (x Lude..:? "Metadata" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "IconURL")
      )
