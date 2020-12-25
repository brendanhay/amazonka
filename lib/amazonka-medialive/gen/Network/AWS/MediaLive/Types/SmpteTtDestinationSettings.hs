{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.SmpteTtDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.SmpteTtDestinationSettings
  ( SmpteTtDestinationSettings (..),

    -- * Smart constructor
    mkSmpteTtDestinationSettings,

    -- * Lenses
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Smpte Tt Destination Settings
--
-- /See:/ 'mkSmpteTtDestinationSettings' smart constructor.
data SmpteTtDestinationSettings = SmpteTtDestinationSettings'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SmpteTtDestinationSettings' value with any optional fields omitted.
mkSmpteTtDestinationSettings ::
  SmpteTtDestinationSettings
mkSmpteTtDestinationSettings = SmpteTtDestinationSettings'

instance Core.FromJSON SmpteTtDestinationSettings where
  toJSON _ = Core.Object Core.mempty

instance Core.FromJSON SmpteTtDestinationSettings where
  parseJSON =
    Core.withObject "SmpteTtDestinationSettings" Core.$
      \x -> Core.pure SmpteTtDestinationSettings'
