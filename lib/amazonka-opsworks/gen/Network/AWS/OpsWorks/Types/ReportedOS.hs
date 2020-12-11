-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.ReportedOS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.ReportedOS
  ( ReportedOS (..),

    -- * Smart constructor
    mkReportedOS,

    -- * Lenses
    roFamily,
    roName,
    roVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A registered instance's reported operating system.
--
-- /See:/ 'mkReportedOS' smart constructor.
data ReportedOS = ReportedOS'
  { family :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ReportedOS' with the minimum fields required to make a request.
--
-- * 'family' - The operating system family.
-- * 'name' - The operating system name.
-- * 'version' - The operating system version.
mkReportedOS ::
  ReportedOS
mkReportedOS =
  ReportedOS'
    { family = Lude.Nothing,
      name = Lude.Nothing,
      version = Lude.Nothing
    }

-- | The operating system family.
--
-- /Note:/ Consider using 'family' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
roFamily :: Lens.Lens' ReportedOS (Lude.Maybe Lude.Text)
roFamily = Lens.lens (family :: ReportedOS -> Lude.Maybe Lude.Text) (\s a -> s {family = a} :: ReportedOS)
{-# DEPRECATED roFamily "Use generic-lens or generic-optics with 'family' instead." #-}

-- | The operating system name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
roName :: Lens.Lens' ReportedOS (Lude.Maybe Lude.Text)
roName = Lens.lens (name :: ReportedOS -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ReportedOS)
{-# DEPRECATED roName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The operating system version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
roVersion :: Lens.Lens' ReportedOS (Lude.Maybe Lude.Text)
roVersion = Lens.lens (version :: ReportedOS -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: ReportedOS)
{-# DEPRECATED roVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.FromJSON ReportedOS where
  parseJSON =
    Lude.withObject
      "ReportedOS"
      ( \x ->
          ReportedOS'
            Lude.<$> (x Lude..:? "Family")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Version")
      )
