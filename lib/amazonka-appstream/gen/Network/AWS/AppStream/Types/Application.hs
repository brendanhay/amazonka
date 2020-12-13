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
    afEnabled,
    afLaunchPath,
    afLaunchParameters,
    afName,
    afDisplayName,
    afMetadata,
    afIconURL,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an application in the application catalog.
--
-- /See:/ 'mkApplication' smart constructor.
data Application = Application'
  { -- | If there is a problem, the application can be disabled after image creation.
    enabled :: Lude.Maybe Lude.Bool,
    -- | The path to the application executable in the instance.
    launchPath :: Lude.Maybe Lude.Text,
    -- | The arguments that are passed to the application at launch.
    launchParameters :: Lude.Maybe Lude.Text,
    -- | The name of the application.
    name :: Lude.Maybe Lude.Text,
    -- | The application name to display.
    displayName :: Lude.Maybe Lude.Text,
    -- | Additional attributes that describe the application.
    metadata :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The URL for the application icon. This URL might be time-limited.
    iconURL :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Application' with the minimum fields required to make a request.
--
-- * 'enabled' - If there is a problem, the application can be disabled after image creation.
-- * 'launchPath' - The path to the application executable in the instance.
-- * 'launchParameters' - The arguments that are passed to the application at launch.
-- * 'name' - The name of the application.
-- * 'displayName' - The application name to display.
-- * 'metadata' - Additional attributes that describe the application.
-- * 'iconURL' - The URL for the application icon. This URL might be time-limited.
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
afEnabled :: Lens.Lens' Application (Lude.Maybe Lude.Bool)
afEnabled = Lens.lens (enabled :: Application -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: Application)
{-# DEPRECATED afEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The path to the application executable in the instance.
--
-- /Note:/ Consider using 'launchPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afLaunchPath :: Lens.Lens' Application (Lude.Maybe Lude.Text)
afLaunchPath = Lens.lens (launchPath :: Application -> Lude.Maybe Lude.Text) (\s a -> s {launchPath = a} :: Application)
{-# DEPRECATED afLaunchPath "Use generic-lens or generic-optics with 'launchPath' instead." #-}

-- | The arguments that are passed to the application at launch.
--
-- /Note:/ Consider using 'launchParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afLaunchParameters :: Lens.Lens' Application (Lude.Maybe Lude.Text)
afLaunchParameters = Lens.lens (launchParameters :: Application -> Lude.Maybe Lude.Text) (\s a -> s {launchParameters = a} :: Application)
{-# DEPRECATED afLaunchParameters "Use generic-lens or generic-optics with 'launchParameters' instead." #-}

-- | The name of the application.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afName :: Lens.Lens' Application (Lude.Maybe Lude.Text)
afName = Lens.lens (name :: Application -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Application)
{-# DEPRECATED afName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The application name to display.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afDisplayName :: Lens.Lens' Application (Lude.Maybe Lude.Text)
afDisplayName = Lens.lens (displayName :: Application -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: Application)
{-# DEPRECATED afDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | Additional attributes that describe the application.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afMetadata :: Lens.Lens' Application (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
afMetadata = Lens.lens (metadata :: Application -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {metadata = a} :: Application)
{-# DEPRECATED afMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | The URL for the application icon. This URL might be time-limited.
--
-- /Note:/ Consider using 'iconURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afIconURL :: Lens.Lens' Application (Lude.Maybe Lude.Text)
afIconURL = Lens.lens (iconURL :: Application -> Lude.Maybe Lude.Text) (\s a -> s {iconURL = a} :: Application)
{-# DEPRECATED afIconURL "Use generic-lens or generic-optics with 'iconURL' instead." #-}

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
