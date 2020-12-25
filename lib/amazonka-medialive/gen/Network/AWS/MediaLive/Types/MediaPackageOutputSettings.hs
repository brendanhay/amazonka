{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MediaPackageOutputSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MediaPackageOutputSettings
  ( MediaPackageOutputSettings (..),

    -- * Smart constructor
    mkMediaPackageOutputSettings,

    -- * Lenses
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Media Package Output Settings
--
-- /See:/ 'mkMediaPackageOutputSettings' smart constructor.
data MediaPackageOutputSettings = MediaPackageOutputSettings'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MediaPackageOutputSettings' value with any optional fields omitted.
mkMediaPackageOutputSettings ::
  MediaPackageOutputSettings
mkMediaPackageOutputSettings = MediaPackageOutputSettings'

instance Core.FromJSON MediaPackageOutputSettings where
  toJSON _ = Core.Object Core.mempty

instance Core.FromJSON MediaPackageOutputSettings where
  parseJSON =
    Core.withObject "MediaPackageOutputSettings" Core.$
      \x -> Core.pure MediaPackageOutputSettings'
