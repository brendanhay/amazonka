{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.OperatingSystem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.OperatingSystem
  ( OperatingSystem (..),

    -- * Smart constructor
    mkOperatingSystem,

    -- * Lenses
    osType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkSpaces.Types.OperatingSystemType

-- | The operating system that the image is running.
--
-- /See:/ 'mkOperatingSystem' smart constructor.
newtype OperatingSystem = OperatingSystem'
  { -- | The operating system.
    type' :: Lude.Maybe OperatingSystemType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OperatingSystem' with the minimum fields required to make a request.
--
-- * 'type'' - The operating system.
mkOperatingSystem ::
  OperatingSystem
mkOperatingSystem = OperatingSystem' {type' = Lude.Nothing}

-- | The operating system.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osType :: Lens.Lens' OperatingSystem (Lude.Maybe OperatingSystemType)
osType = Lens.lens (type' :: OperatingSystem -> Lude.Maybe OperatingSystemType) (\s a -> s {type' = a} :: OperatingSystem)
{-# DEPRECATED osType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON OperatingSystem where
  parseJSON =
    Lude.withObject
      "OperatingSystem"
      (\x -> OperatingSystem' Lude.<$> (x Lude..:? "Type"))
