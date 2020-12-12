{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.Application
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.Application
  ( Application (..),

    -- * Smart constructor
    mkApplication,

    -- * Lenses
    aArgs,
    aAdditionalInfo,
    aName,
    aVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | With Amazon EMR release version 4.0 and later, the only accepted parameter is the application name. To pass arguments to applications, you use configuration classifications specified using configuration JSON objects. For more information, see <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/emr-configure-apps.html Configuring Applications> .
--
-- With earlier Amazon EMR releases, the application is any Amazon or third-party software that you can add to the cluster. This structure contains a list of strings that indicates the software to use with the cluster and accepts a user argument list. Amazon EMR accepts and forwards the argument list to the corresponding installation script as bootstrap action argument.
--
-- /See:/ 'mkApplication' smart constructor.
data Application = Application'
  { args :: Lude.Maybe [Lude.Text],
    additionalInfo :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    name :: Lude.Maybe Lude.Text,
    version :: Lude.Maybe Lude.Text
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
-- * 'additionalInfo' - This option is for advanced users only. This is meta information about third-party applications that third-party vendors use for testing purposes.
-- * 'args' - Arguments for Amazon EMR to pass to the application.
-- * 'name' - The name of the application.
-- * 'version' - The version of the application.
mkApplication ::
  Application
mkApplication =
  Application'
    { args = Lude.Nothing,
      additionalInfo = Lude.Nothing,
      name = Lude.Nothing,
      version = Lude.Nothing
    }

-- | Arguments for Amazon EMR to pass to the application.
--
-- /Note:/ Consider using 'args' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aArgs :: Lens.Lens' Application (Lude.Maybe [Lude.Text])
aArgs = Lens.lens (args :: Application -> Lude.Maybe [Lude.Text]) (\s a -> s {args = a} :: Application)
{-# DEPRECATED aArgs "Use generic-lens or generic-optics with 'args' instead." #-}

-- | This option is for advanced users only. This is meta information about third-party applications that third-party vendors use for testing purposes.
--
-- /Note:/ Consider using 'additionalInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAdditionalInfo :: Lens.Lens' Application (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
aAdditionalInfo = Lens.lens (additionalInfo :: Application -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {additionalInfo = a} :: Application)
{-# DEPRECATED aAdditionalInfo "Use generic-lens or generic-optics with 'additionalInfo' instead." #-}

-- | The name of the application.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aName :: Lens.Lens' Application (Lude.Maybe Lude.Text)
aName = Lens.lens (name :: Application -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Application)
{-# DEPRECATED aName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of the application.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aVersion :: Lens.Lens' Application (Lude.Maybe Lude.Text)
aVersion = Lens.lens (version :: Application -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: Application)
{-# DEPRECATED aVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.FromJSON Application where
  parseJSON =
    Lude.withObject
      "Application"
      ( \x ->
          Application'
            Lude.<$> (x Lude..:? "Args" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "AdditionalInfo" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Version")
      )

instance Lude.ToJSON Application where
  toJSON Application' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Args" Lude..=) Lude.<$> args,
            ("AdditionalInfo" Lude..=) Lude.<$> additionalInfo,
            ("Name" Lude..=) Lude.<$> name,
            ("Version" Lude..=) Lude.<$> version
          ]
      )
