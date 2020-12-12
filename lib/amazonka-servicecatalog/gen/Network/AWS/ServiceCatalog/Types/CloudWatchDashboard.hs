{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.CloudWatchDashboard
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.CloudWatchDashboard
  ( CloudWatchDashboard (..),

    -- * Smart constructor
    mkCloudWatchDashboard,

    -- * Lenses
    cwdName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a CloudWatch dashboard.
--
-- /See:/ 'mkCloudWatchDashboard' smart constructor.
newtype CloudWatchDashboard = CloudWatchDashboard'
  { name ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CloudWatchDashboard' with the minimum fields required to make a request.
--
-- * 'name' - The name of the CloudWatch dashboard.
mkCloudWatchDashboard ::
  CloudWatchDashboard
mkCloudWatchDashboard = CloudWatchDashboard' {name = Lude.Nothing}

-- | The name of the CloudWatch dashboard.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwdName :: Lens.Lens' CloudWatchDashboard (Lude.Maybe Lude.Text)
cwdName = Lens.lens (name :: CloudWatchDashboard -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CloudWatchDashboard)
{-# DEPRECATED cwdName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON CloudWatchDashboard where
  parseJSON =
    Lude.withObject
      "CloudWatchDashboard"
      (\x -> CloudWatchDashboard' Lude.<$> (x Lude..:? "Name"))
